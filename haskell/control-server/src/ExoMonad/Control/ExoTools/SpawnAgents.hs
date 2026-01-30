{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.ExoTools.SpawnAgents
  ( spawnAgentsLogic,
    SpawnAgentsArgs (..),
    SpawnAgentsResult (..),
    SpawnAgentsGraph (..),
    CleanupAgentsGraph (..),
    CleanupAgentsArgs (..),
    CleanupAgentsResult (..),
    findRepoRoot,
  )
where

import Control.Monad (forM)
import Control.Monad.Freer (Eff, Member)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Control.Combinators (withEffect)
import ExoMonad.Control.ExoTools.Internal (slugify)
import ExoMonad.Control.ExoTools.SpawnAgents.Prompt (renderInitialPrompt)
import ExoMonad.Control.ExoTools.SpawnAgents.Types
import ExoMonad.Control.ExoTools.SpawnCleanup (SpawnCleanup, SpawnProgress (..), acquireContainer, acquireWorktree, cleanupAll, emitProgress, runSpawnCleanup)
import ExoMonad.Control.Runtime.Paths as Paths
import ExoMonad.Effect.Log (Log, logError, logInfo, logWarn)
import ExoMonad.Effect.Types (Return)
import ExoMonad.Effects.DockerSpawner (ContainerId (..), DockerSpawner, ExecResult (..), SpawnConfig (..), execContainer, spawnContainer, stopContainer)
import ExoMonad.Effects.Env (Env, getEnv)
import ExoMonad.Effects.FileSystem (FileSystem, directoryExists, fileExists, writeFileText)
import ExoMonad.Effects.Git (Git, WorktreeInfo (..), getWorktreeInfo)
import ExoMonad.Effects.GitHub (GitHub, Issue (..), IssueState (..), Repo (..), getIssue)
import ExoMonad.Effects.Worktree (Worktree, WorktreePath (..), WorktreeSpec (..), createWorktree, deleteWorktree, listWorktrees)
import ExoMonad.Effects.Zellij (LayoutSpec (..), TabConfig (..), TabId (..), Zellij, checkZellijEnv, generateLayout, newTab)
import ExoMonad.Graph.Generic (type (:-))
import ExoMonad.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import ExoMonad.Graph.Goto (Goto, To)
import ExoMonad.Graph.Types (Exit, Input, MCPExport, MCPRoleHint, MCPToolDef, UsesEffects, type (:@))
import ExoMonad.Role (Role (..))
import GHC.Generics (Generic)
import System.FilePath ((</>))

-- | Graph definition for spawn_agents tool.
data SpawnAgentsGraph mode = SpawnAgentsGraph
  { saEntry ::
      mode
        :- EntryNode SpawnAgentsArgs
        :@ MCPExport
        :@ MCPToolDef '("spawn_agents", "Spawn isolated environments for parallel task execution")
        :@ MCPRoleHint 'TL,
    saRun ::
      mode
        :- LogicNode
        :@ Input SpawnAgentsArgs
        :@ UsesEffects '[GitHub, Git, Worktree, FileSystem, Zellij, Env, DockerSpawner, Log, Return SpawnAgentsResult]
  }
  deriving (Generic)

-- | Graph definition for cleanup_agents tool.
data CleanupAgentsGraph mode = CleanupAgentsGraph
  { caEntry ::
      mode
        :- EntryNode CleanupAgentsArgs
        :@ MCPExport
        :@ MCPToolDef '("cleanup_agents", "Cleanup spawn_agents resources (worktrees, containers)")
        :@ MCPRoleHint 'TL,
    caRun ::
      mode
        :- LogicNode
        :@ Input CleanupAgentsArgs
        :@ UsesEffects '[Worktree, DockerSpawner, Log, Return CleanupAgentsResult]
  }
  deriving (Generic)

-- | Find the repository root by checking ENV or using Git.
findRepoRoot ::
  (Member Env es, Member Git es) =>
  Eff es (Maybe FilePath)
findRepoRoot = do
  -- 1. Check EXOMONAD_ROOT environment variable
  mEnv <- getEnv "EXOMONAD_ROOT"
  case mEnv of
    Just path -> pure $ Just (T.unpack path)
    Nothing -> do
      -- 2. Use Git to find top level
      mWtInfo <- getWorktreeInfo
      pure $ fmap (\wi -> wi.wiRepoRoot) mWtInfo

-- | Spawn mode for agents.
data SpawnMode
  = -- | Use local Zellij tabs (legacy)
    SpawnZellij
  | -- | Use Docker containers via DockerSpawner
    SpawnDocker
  deriving (Show, Eq)

parseSpawnMode :: Maybe Text -> SpawnMode
parseSpawnMode (Just "zellij") = SpawnZellij
parseSpawnMode _ = SpawnDocker

-- | Core logic for spawn_agents.
spawnAgentsLogic ::
  (Member GitHub es, Member Git es, Member Worktree es, Member FileSystem es, Member Zellij es, Member Env es, Member DockerSpawner es, Member Log es) =>
  SpawnAgentsArgs ->
  Eff es SpawnAgentsResult
spawnAgentsLogic args = do
  -- 1. Check Zellij environment (fail if not in Zellij)
  mZellijSession <- checkZellijEnv
  case mZellijSession of
    Nothing ->
      pure $
        SpawnAgentsResult
          { worktrees = [],
            tabs = [],
            failed = [("*", "Not running in Zellij session")]
          }
    Just _ -> do
      logInfo $ "Starting spawn_agents for issues: " <> T.intercalate ", " args.issueNumbers
      -- 2. Determine Repo Root and Worktree Base Path
      spawnModeEnv <- getEnv "SPAWN_MODE"
      let spawnMode = parseSpawnMode spawnModeEnv
      mRepoRoot <- findRepoRoot
      let repoRoot = fromMaybe "." mRepoRoot

      -- Target directory discovery
      wtBaseDir <- case (spawnMode, mRepoRoot) of
        (SpawnZellij, Just rr) -> pure $ rr </> "worktrees"
        (SpawnZellij, Nothing) -> pure $ repoRoot </> ".worktrees" </> "exomonad"
        _ -> do
          -- Docker mode: prefer EXOMONAD_WORKTREES_PATH
          mWorktreesPath <- getEnv "EXOMONAD_WORKTREES_PATH"
          pure $ T.unpack $ fromMaybe "/worktrees" mWorktreesPath

      -- 3. Validate and normalize backend parameter
      let backendRaw = T.toLower $ fromMaybe "claude" args.backend
          validBackends = ["claude", "gemini"]

      if backendRaw `notElem` validBackends
        then
          pure $
            SpawnAgentsResult
              { worktrees = [],
                tabs = [],
                failed = [("*", "Invalid backend '" <> backendRaw <> "'. Must be 'claude' or 'gemini'.")]
              }
        else do
          -- 4. Process each issue
          results <- forM args.issueNumbers $ \shortId -> do
            (res, _orphans) <- runSpawnCleanup $ processIssue spawnMode repoRoot wtBaseDir backendRaw shortId
            pure res

          -- Partition results
          let (failed, succeeded) = partitionEithers results
              worktreesList = [(sid, path) | (sid, path, _) <- succeeded]
              tabsList = [(sid, tabId) | (sid, _, TabId tabId) <- succeeded]

          pure $
            SpawnAgentsResult
              { worktrees = worktreesList,
                tabs = tabsList,
                failed = failed
              }

-- | Process a single issue: create worktree, bootstrap .exomonad/, write context, launch tab.
processIssue ::
  (Member GitHub es, Member Worktree es, Member FileSystem es, Member Zellij es, Member Env es, Member DockerSpawner es, Member Log es, Member SpawnCleanup es) =>
  -- | Spawn mode (Zellij or Docker)
  SpawnMode ->
  -- | Repo root
  FilePath ->
  -- | Worktree base directory
  FilePath ->
  -- | Backend ("claude" or "gemini")
  Text ->
  -- | Issue number as text
  Text ->
  -- | Left (id, error) or Right (id, path, tabId)
  Eff es (Either (Text, Text) (Text, FilePath, TabId))
processIssue spawnMode repoRoot wtBaseDir backend shortId = do
  emitProgress $ SpawnStarted shortId 4 -- Steps: Worktree, Container, HealthCheck, Tab
  -- Validate shortId (prevent path traversal)
  if T.any (\c -> c == '/' || c == '\\') shortId
    then do
      let err = "Invalid issue ID: contains path separators"
      emitProgress $ SpawnFailed shortId err
      pure $ Left (shortId, err)
    else do
      let issueNum = case (reads (T.unpack shortId) :: [(Int, String)]) of
            [(n, "")] -> Just n
            _ -> Nothing

      case issueNum of
        Nothing -> do
          let err = "Invalid issue number: " <> shortId
          emitProgress $ SpawnFailed shortId err
          pure $ Left (shortId, err)
        Just n -> do
          -- Determine binary directory based on mode
          mBinDirEnv <- getEnv "EXOMONAD_BIN_DIR"
          let binDir = case (spawnMode, mBinDirEnv) of
                (SpawnZellij, _) -> Paths.runtimeBinDir repoRoot
                (_, Just binDirVal) -> T.unpack binDirVal
                _ -> "/usr/local/bin"

          -- Early validation: Check for binary existence
          let binPath = Paths.controlServerBin binDir
          binExistsRes <- fileExists binPath

          -- Handle fileExists result (Left is error, Right is boolean existence)
          let binExists = case binExistsRes of
                Right True -> True
                _ -> False

          if not binExists
            then do
              let err = "Binary missing: " <> T.pack binPath
              emitProgress $ SpawnFailed shortId err
              pure $ Left (shortId, err)
            else do
              -- Get repo from env var or use default
              mEnvRepo <- getEnv "GITHUB_REPO"
              let repo = Repo $ fromMaybe "tidepool-heavy-industries/exomonad" mEnvRepo
              issueResult <- getIssue repo n False
              case issueResult of
                Left err -> do
                  let errMsg = "GitHub error fetching issue: " <> shortId <> " (" <> T.pack (show err) <> ")"
                  emitProgress $ SpawnFailed shortId errMsg
                  pure $ Left (shortId, errMsg)
                Right Nothing -> do
                  let err = "Issue not found: " <> shortId
                  emitProgress $ SpawnFailed shortId err
                  pure $ Left (shortId, err)
                Right (Just issue) -> do
                  if issue.issueState == IssueClosed
                    then do
                      let err = "Issue is closed: " <> shortId
                      emitProgress $ SpawnFailed shortId err
                      pure $ Left (shortId, err)
                    else do
                      let slug = slugify issue.issueTitle
                          branchName = "gh-" <> shortId <> "/" <> slug
                          -- New location: <wtBaseDir>/gh-<id>-<slug>
                          targetPath = wtBaseDir </> "gh-" <> T.unpack shortId <> "-" <> T.unpack slug
                          spec =
                            WorktreeSpec
                              { baseName = "gh-" <> shortId,
                                fromBranch = Just "origin/main",
                                branchName = Just branchName,
                                path = Just targetPath
                              }

                      -- Check if worktree directory already exists (idempotency)
                      dirExistsRes <- directoryExists targetPath
                      let dirExists = case dirExistsRes of
                            Right True -> True
                            _ -> False

                      -- Idempotency: if worktree exists, skip creation and reuse it
                      -- This allows re-launching a Zellij tab for an existing worktree
                      mPath <-
                        if dirExists
                          then do
                            logInfo $ "[" <> shortId <> "] Reusing existing worktree: " <> T.pack targetPath
                            emitProgress $ WorktreeCreated shortId targetPath
                            pure $ Right targetPath -- Reuse existing worktree
                          else do
                            -- a. Create worktree (only if it doesn't exist)
                            logInfo $ "[" <> shortId <> "] Creating worktree: " <> T.pack targetPath
                            withEffect
                              (createWorktree spec)
                              ( \(WorktreePath p) -> do
                                  acquireWorktree (WorktreePath p)
                                  emitProgress $ WorktreeCreated shortId p
                                  pure $ Right p
                              )
                              ( \err -> do
                                  let errMsg = T.pack (show err)
                                  emitProgress $ SpawnFailed shortId errMsg
                                  pure $ Left (shortId, errMsg)
                              )

                      case mPath of
                        Left errTuple -> pure $ Left errTuple
                        Right path -> do
                          -- b. Configure environment for TCP control-server
                          let backendCmd = case backend of
                                "gemini" -> "gemini --debug"
                                "claude" -> "claude --debug --verbose"
                                _ -> "claude --debug --verbose"

                              -- Resolve socket paths based on spawn mode
                              (controlSocket, tuiSocket) = case spawnMode of
                                SpawnDocker -> ("/sockets/control.sock", "/sockets/tui.sock")
                                SpawnZellij ->
                                  ( repoRoot </> ".exomonad" </> "sockets" </> "control.sock",
                                    repoRoot </> ".exomonad" </> "sockets" </> "tui.sock"
                                  )

                          -- Capture auth and repo vars for subagents
                          ghToken <- getEnv "GH_TOKEN"
                          repoUrl <- getEnv "EXOMONAD_REPO_URL"
                          repoBranch <- getEnv "EXOMONAD_REPO_BRANCH"

                          -- Subagents connect to shared control-server via TCP (Docker) or UDS (Zellij)
                          -- No local control-server startup needed
                          let envVars =
                                [ ("SUBAGENT_CMD", backendCmd),
                                  ("CONTROL_SERVER_URL", "http://control-server:7432"),
                                  ("EXOMONAD_ROLE", "dev"),
                                  ("EXOMONAD_BIN_DIR", T.pack binDir),
                                  ("EXOMONAD_CONTROL_SOCKET", T.pack controlSocket),
                                  ("EXOMONAD_TUI_SOCKET", T.pack tuiSocket)
                                ]
                                  ++ maybe [] (\t -> [("GH_TOKEN", t)]) ghToken
                                  ++ maybe [] (\u -> [("EXOMONAD_REPO_URL", u)]) repoUrl
                                  ++ maybe [] (\b -> [("EXOMONAD_REPO_BRANCH", b)]) repoBranch

                          -- Skip bootstrapExoMonad - subagents use shared control-server
                          -- entrypoint.sh writes .mcp.json based on CONTROL_SERVER_URL
                          -- Issue context comes from GitHub (no context file needed)

                          -- c. Launch agent (Zellij or Docker)
                          res <- case spawnMode of
                            SpawnZellij -> do
                              logInfo $ "[" <> shortId <> "] Launching Zellij tab"
                              let tabConfig =
                                    TabConfig
                                      { tcName = shortId,
                                        tcLayout = repoRoot </> ".zellij" </> "worktree.kdl",
                                        tcCwd = path,
                                        tcEnv = envVars,
                                        tcCommand = Nothing
                                      }
                              withEffect
                                (newTab tabConfig)
                                ( \tabId -> do
                                    emitProgress $ TabLaunched shortId tabId
                                    pure $ Right (shortId, path, tabId)
                                )
                                ( \err -> do
                                    let errMsg = "Tab launch failed: " <> T.pack (show err)
                                    logError $ "[" <> shortId <> "] " <> errMsg
                                    emitProgress $ SpawnFailed shortId errMsg
                                    cleanupAll
                                    pure $ Left (shortId, errMsg)
                                )
                            SpawnDocker -> do
                              logInfo $ "[" <> shortId <> "] Spawning Docker container"

                              -- Render initial prompt
                              let promptCtx =
                                    InitialPromptContext
                                      { issue_number = shortId,
                                        issue_title = issue.issueTitle,
                                        issue_body = issue.issueBody,
                                        branch_name = branchName,
                                        worktree_path = T.pack path
                                      }
                              let initialPrompt = renderInitialPrompt promptCtx

                              -- Write initial prompt to file for Gemini (avoid shell limits/escaping)
                              writeRes <-
                                if backend == "gemini"
                                  then do
                                    let contextFile = path </> "INITIAL_CONTEXT.md"
                                    writeFileText contextFile initialPrompt
                                  else pure (Right ())

                              case writeRes of
                                Left err -> do
                                  let errMsg = "Failed to write context file: " <> T.pack (show err)
                                  logError $ "[" <> shortId <> "] " <> errMsg
                                  emitProgress $ SpawnFailed shortId errMsg
                                  cleanupAll
                                  pure $ Left (shortId, errMsg)
                                Right () -> do
                                  -- Build command with prompt
                                  let cmdOverride = case backend of
                                        "claude" -> Just ["claude", "--permission-mode", "bypassPermissions", "--debug", "--verbose", initialPrompt]
                                        "gemini" -> Just ["gemini", "--debug", "--prompt-interactive", "Read @INITIAL_CONTEXT.md and begin the task."]
                                        _ -> Nothing

                                  let config =
                                        SpawnConfig
                                          { scIssueId = shortId,
                                            scWorktreePath = path,
                                            scBackend = backend,
                                            scUid = Nothing,
                                            scGid = Nothing,
                                            scEnv = envVars,
                                            scCmd = cmdOverride
                                          }
                                  containerResult <- spawnContainer config
                                  case containerResult of
                                    Left err -> do
                                      let errMsg = "Container spawn failed: " <> T.pack (show err)
                                      logError $ "[" <> shortId <> "] " <> errMsg
                                      emitProgress $ SpawnFailed shortId errMsg
                                      cleanupAll
                                      pure $ Left (shortId, errMsg)
                                    Right containerId -> do
                                      acquireContainer containerId
                                      emitProgress $ ContainerSpawned shortId containerId

                                      -- Health Check
                                      logInfo $ "[" <> shortId <> "] Running health check for container: " <> containerId.unContainerId
                                      -- Try to run 'echo ok' in the container
                                      healthRes <- execContainer containerId ["echo", "ok"] Nothing Nothing
                                      case healthRes of
                                        Left err -> do
                                          let errMsg = "Health check failed (Docker error): " <> T.pack (show err)
                                          logError $ "[" <> shortId <> "] " <> errMsg
                                          emitProgress $ SpawnFailed shortId errMsg
                                          cleanupAll
                                          pure $ Left (shortId, errMsg)
                                        Right execRes ->
                                          if execRes.erExitCode == Just 0
                                            then do
                                              logInfo $ "[" <> shortId <> "] Health check passed"
                                              -- Generate layout dynamically with zellij-gen
                                              -- This bakes commands into the layout as literals (env vars don't propagate to panes)
                                              logInfo $ "[" <> shortId <> "] Generating layout via zellij-gen"
                                              genResult <- generateLayout $ SubagentLayout shortId containerId.unContainerId
                                              case genResult of
                                                Left genErr -> do
                                                  let errMsg = "Layout generation failed: " <> T.pack (show genErr)
                                                  logError $ "[" <> shortId <> "] " <> errMsg
                                                  emitProgress $ SpawnFailed shortId errMsg
                                                  cleanupAll
                                                  pure $ Left (shortId, errMsg)
                                                Right layoutPath -> do
                                                  logInfo $ "[" <> shortId <> "] Generated layout: " <> T.pack layoutPath
                                                  let tabConfig =
                                                        TabConfig
                                                          { tcName = shortId,
                                                            tcLayout = layoutPath,
                                                            tcCwd = path,
                                                            tcEnv = envVars, -- No SUBAGENT_CMD needed - command baked into layout
                                                            tcCommand = Nothing
                                                          }
                                                  tabRes <- newTab tabConfig
                                                  case tabRes of
                                                    Left err -> do
                                                      let errMsg = "Container spawned (" <> containerId.unContainerId <> ") but tab launch failed: " <> T.pack (show err)
                                                      logError $ "[" <> shortId <> "] " <> errMsg
                                                      emitProgress $ SpawnFailed shortId errMsg
                                                      cleanupAll
                                                      pure $ Left (shortId, errMsg)
                                                    Right tabId -> do
                                                      emitProgress $ TabLaunched shortId tabId
                                                      pure $ Right (shortId, path, tabId)
                                            else do
                                              let exitText = maybe "unknown" (T.pack . show) execRes.erExitCode
                                                  errMsg = "Health check failed (exit code " <> exitText <> "): " <> execRes.erStderr
                                              logError $ "[" <> shortId <> "] " <> errMsg
                                              emitProgress $ SpawnFailed shortId errMsg
                                              cleanupAll
                                              pure $ Left (shortId, errMsg)

                          case res of
                            Right _ -> emitProgress $ SpawnComplete shortId
                            _ -> pure ()
                          pure res

-- | Logic for cleanup_agents.
cleanupAgentsLogic ::
  (Member Worktree es, Member DockerSpawner es, Member Log es) =>
  CleanupAgentsArgs ->
  Eff es CleanupAgentsResult
cleanupAgentsLogic args = do
  logInfo $ "Cleaning up agents for issues: " <> T.intercalate ", " args.issueNumbers

  -- 1. Get all worktrees to find matches
  withEffect
    listWorktrees
    ( \allWorktrees -> do
        results <- forM args.issueNumbers $ \shortId -> do
          logInfo $ "[" <> shortId <> "] Starting cleanup"

          -- a. Stop container (using predictable name)
          let containerId = ContainerId $ "exomonad-agent-" <> shortId
          stopStatus <-
            withEffect
              (stopContainer containerId)
              ( \() -> do
                  logInfo $ "[" <> shortId <> "] Container stopped"
                  pure (Right ())
              )
              ( \err -> do
                  let errText = T.pack (show err)
                  -- Check if error is "No such container" (ignorable) or actual failure
                  -- Note: DockerSpawner interpreter currently returns DockerConnectionError for all failures
                  if "No such container" `T.isInfixOf` errText || "not found" `T.isInfixOf` errText
                    then do
                      logInfo $ "[" <> shortId <> "] Container already gone (not found)"
                      pure (Right ())
                    else do
                      logWarn $ "[" <> shortId <> "] Container stop failed: " <> errText
                      pure (Left errText)
              )

          -- b. Delete worktree
          -- Find worktrees that match "gh-<shortId>-"
          let prefix = "gh-" <> shortId <> "-"
              matchingWts = filter (\(WorktreePath p, _) -> prefix `T.isInfixOf` T.pack p) allWorktrees

          cleanupWtResults <- forM matchingWts $ \(wtPath@(WorktreePath p), _) -> do
            logInfo $ "[" <> shortId <> "] Deleting worktree: " <> T.pack p
            deleteWorktree wtPath

          let wtFailures = [T.pack (show err) | Left err <- cleanupWtResults]

          -- Consolidate results
          case (stopStatus, wtFailures) of
            (Right (), []) -> pure $ Right shortId
            (Left stopErr, []) -> pure $ Left (shortId, "Container stop failed: " <> stopErr)
            (Right (), wts) -> pure $ Left (shortId, "Worktree deletion failed: " <> T.intercalate "; " wts)
            (Left stopErr, wts) -> pure $ Left (shortId, "Container stop failed: " <> stopErr <> "; Worktree deletion failed: " <> T.intercalate "; " wts)

        let (failed, succeeded) = partitionEithers results
        pure $
          CleanupAgentsResult
            { cleaned = succeeded,
              failed = failed
            }
    )
    ( \err -> do
        -- Propagate worktree enumeration failure
        let msg = "Failed to list worktrees: " <> T.pack (show err)
            failed = [(shortId, msg) | shortId <- args.issueNumbers]
        logError msg
        pure $
          CleanupAgentsResult
            { cleaned = [],
              failed = failed
            }
    )
