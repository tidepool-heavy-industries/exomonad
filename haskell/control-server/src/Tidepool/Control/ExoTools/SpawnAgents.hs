{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tidepool.Control.ExoTools.SpawnAgents
  ( SpawnAgentsGraph(..)
  , spawnAgentsHandlers
  , spawnAgentsLogic
  , SpawnAgentsArgs(..)
  , SpawnAgentsResult(..)
  )
where

import Control.Monad (forM)
import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=), object, withObject, encode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import System.FilePath ((</>), takeDirectory)

import Tidepool.Effects.BD (BD, BeadInfo(..), BeadStatus(..), DependencyInfo(..), getBead)
import Tidepool.Effects.DockerSpawner (DockerSpawner, SpawnConfig(..), spawnContainer, ContainerId(..))
import Tidepool.Effects.Env (Env, getEnv, getEnvironment)
import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo)
import Tidepool.Effects.Worktree (Worktree, WorktreeSpec(..), WorktreePath(..), createWorktree)

import Tidepool.Effects.FileSystem (FileSystem, createDirectory, writeFileText, fileExists, directoryExists, readFileText)
import Tidepool.Effects.Zellij (Zellij, TabConfig(..), TabId(..), checkZellijEnv, newTab)
import Tidepool.Role (Role(..))
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef, MCPRoleHint)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, arraySchema, emptySchema, SchemaType(..), describeField)

import Tidepool.Control.ExoTools.Internal (slugify)
import Tidepool.Control.Runtime.Paths as Paths
import Tidepool.Control.Runtime.ProcessCompose as PC
import qualified Data.Yaml as Yaml

-- | Find the hangar root by checking ENV or walking up from repo root.
findHangarRoot
  :: (Member Env es, Member FileSystem es, Member Git es)
  => Eff es (Maybe FilePath)
findHangarRoot = do
  -- 1. Check HANGAR_ROOT environment variable
  mEnv <- getEnv "HANGAR_ROOT"
  case mEnv of
    Just path -> pure $ Just (T.unpack path)
    Nothing -> do
      -- 2. Walk up from current repo root
      mWtInfo <- getWorktreeInfo
      case mWtInfo of
        Nothing -> pure Nothing
        Just wi -> walkUp wi.wiRepoRoot
  where
    walkUp path = do
      res <- fileExists (path </> "Hangar.toml")
      case res of
        Right True -> pure $ Just path
        _ ->
          let parent = takeDirectory path
          in if parent == path
             then pure Nothing
             else walkUp parent

-- | Build minimal bead context (task description only).
-- Workflow instructions are now in SessionStart template.
buildBeadContext :: BeadInfo -> Text -> Text
buildBeadContext bead branchName = T.unlines
  [
    "# Task: " <> bead.biTitle
  , ""
  , "**Bead ID:** " <> bead.biId
  , "**Branch:** " <> branchName
  , "**Priority:** P" <> T.pack (show bead.biPriority)
  , ""
  , "## Description"
  , ""
  , fromMaybe "(No description provided)" bead.biDescription
  , ""
  , if null bead.biDependencies
      then ""
      else T.unlines $
        [ "## Dependencies"
        , ""
        ] ++ map formatDep bead.biDependencies
  ]
  where
    formatDep (DependencyInfo { diId = depId, diTitle = depTitle, diStatus = depStatus }) =
      "- **" <> depId <> "**: " <> depTitle <> " (" <> statusToText depStatus <> ")"

    statusToText = \case
      StatusOpen -> "open"
      StatusInProgress -> "in_progress"
      StatusClosed -> "completed"
      StatusHooked -> "hooked"
      StatusBlocked -> "blocked"

-- | Arguments for spawn_agents tool.
data SpawnAgentsArgs = SpawnAgentsArgs
  { saaBeadIds :: [Text]  -- ^ List of short form bead IDs (e.g. "wzi", "1b2").
  , saaBackend :: Maybe Text  -- ^ Backend to use: "claude" or "gemini" (defaults to "claude").
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema SpawnAgentsArgs where
  jsonSchema = objectSchema
    [
      ("bead_ids", describeField "bead_ids" "List of short-form bead IDs to spawn worktrees for." (arraySchema (emptySchema TString)))
    , ("backend", describeField "backend" "Backend to use: 'claude' or 'gemini' (defaults to 'claude')." (emptySchema TString))
    ]
    ["bead_ids"]

instance FromJSON SpawnAgentsArgs where
  parseJSON = withObject "SpawnAgentsArgs" $ \v ->
    SpawnAgentsArgs
      <$> v .: "bead_ids"
      <*> v .:? "backend"

instance ToJSON SpawnAgentsArgs where
  toJSON args = object
    [
      "bead_ids" .= saaBeadIds args
    , "backend" .= saaBackend args
    ]

-- | Result of spawn_agents tool.
data SpawnAgentsResult = SpawnAgentsResult
  {
    sarWorktrees :: [(Text, FilePath)]  -- ^ Successfully created worktrees: (shortId, path)
  , sarTabs      :: [(Text, Text)]      -- ^ Successfully launched tabs: (shortId, tabId)
  , sarFailed    :: [(Text, Text)]      -- ^ Failed operations: (shortId, reason)
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SpawnAgentsResult where
  parseJSON = withObject "SpawnAgentsResult" $ \v ->
    SpawnAgentsResult
      <$> v .: "worktrees"
      <*> v .: "tabs"
      <*> v .: "failed"

instance ToJSON SpawnAgentsResult where
  toJSON res = object
    [
      "worktrees" .= sarWorktrees res
    , "tabs"      .= sarTabs res
    , "failed"    .= sarFailed res
    ]

-- | Graph definition for spawn_agents tool.
data SpawnAgentsGraph mode = SpawnAgentsGraph
  {
    saEntry :: mode :- EntryNode SpawnAgentsArgs
      :@ MCPExport
      :@ MCPToolDef '("spawn_agents", "Create worktrees and branches for parallel agent dispatch. Accepts optional 'backend' parameter ('claude' or 'gemini', defaults to 'claude').")
      :@ MCPRoleHint 'TL

  , saRun :: mode :- LogicNode
      :@ Input SpawnAgentsArgs
      :@ UsesEffects '[BD, Git, Worktree, FileSystem, Zellij, Env, DockerSpawner, Goto Exit SpawnAgentsResult]

  , saExit :: mode :- ExitNode SpawnAgentsResult
  }
  deriving Generic

-- | Handlers for spawn_agents graph.
spawnAgentsHandlers
  :: (Member BD es, Member Git es, Member Worktree es, Member FileSystem es, Member Zellij es, Member Env es, Member DockerSpawner es)
  => SpawnAgentsGraph (AsHandler es)
spawnAgentsHandlers = SpawnAgentsGraph
  {
    saEntry = ()
  , saRun = spawnAgentsLogic
  , saExit = ()
  }

-- | Core logic for spawn_agents.
spawnAgentsLogic
  :: (Member BD es, Member Git es, Member Worktree es, Member FileSystem es, Member Zellij es, Member Env es, Member DockerSpawner es)
  => SpawnAgentsArgs
  -> Eff es (GotoChoice '[To Exit SpawnAgentsResult])
spawnAgentsLogic args = do
  -- 1. Check Zellij environment (fail if not in Zellij)
  mZellijSession <- checkZellijEnv
  case mZellijSession of
    Nothing -> pure $ gotoExit $ SpawnAgentsResult
      {
        sarWorktrees = []
      , sarTabs = []
      , sarFailed = [("*", "Not running in Zellij session")]
      }
    Just _ -> do
      -- 2. Determine Hangar Root and Worktree Base Path
      spawnMode <- fromMaybe "docker" <$> getEnv "SPAWN_MODE"
      mHangarRoot <- findHangarRoot
      mWtInfo <- getWorktreeInfo
      let repoRoot = maybe "." (\wi -> wi.wiRepoRoot) mWtInfo
      
      -- Target directory discovery
      wtBaseDir <- case (spawnMode, mHangarRoot) of
        ("zellij", Just hr) -> pure $ hr </> "worktrees"
        ("zellij", Nothing) -> pure $ repoRoot </> ".worktrees" </> "tidepool"
        _ -> do
          -- Docker mode: prefer TIDEPOOL_WORKTREES_PATH
          mWorktreesPath <- getEnv "TIDEPOOL_WORKTREES_PATH"
          pure $ T.unpack $ fromMaybe "/worktrees" mWorktreesPath

      -- 3. Validate and normalize backend parameter
      let backendRaw = T.toLower $ fromMaybe "claude" args.saaBackend
          validBackends = ["claude", "gemini"]

      if backendRaw `notElem` validBackends
        then pure $ gotoExit $ SpawnAgentsResult
          {
            sarWorktrees = []
          , sarTabs = []
          , sarFailed = [("*", "Invalid backend '" <> backendRaw <> "'. Must be 'claude' or 'gemini'.")]
          }
        else do
          -- 4. Process each bead
          results <- forM args.saaBeadIds $ \shortId -> do
            processBead mHangarRoot repoRoot wtBaseDir backendRaw shortId

          -- Partition results
          let (failed, succeeded) = partitionEithers results
              worktrees = [(sid, path) | (sid, path, _) <- succeeded]
              tabs = [(sid, tabId) | (sid, _, TabId tabId) <- succeeded]

          pure $ gotoExit $ SpawnAgentsResult
            {
              sarWorktrees = worktrees
            , sarTabs = tabs
            , sarFailed = failed
            }


-- | Process a single bead: create worktree, bootstrap .tidepool/, write context, launch tab.
processBead
  :: (Member BD es, Member Worktree es, Member FileSystem es, Member Zellij es, Member Env es, Member DockerSpawner es)
  => Maybe FilePath                                    -- ^ Hangar root (for templates)
  -> FilePath                                          -- ^ Repo root (for symlinks/templates)
  -> FilePath                                          -- ^ Worktree base directory
  -> Text                                              -- ^ Backend ("claude" or "gemini")
  -> Text                                              -- ^ Short bead ID
  -> Eff es (Either (Text, Text) (Text, FilePath, TabId))  -- ^ Left (id, error) or Right (id, path, tabId)
processBead mHangarRoot repoRoot wtBaseDir backend shortId = do
  -- Validate shortId (prevent path traversal)
  if T.any (\c -> c == '/' || c == '\\') shortId
    then pure $ Left (shortId, "Invalid bead ID: contains path separators")
    else do
      -- Normalize bead ID (ensure it starts with tidepool-)
      let fullId = if "tidepool-" `T.isPrefixOf` shortId
                   then shortId
                   else "tidepool-" <> shortId
          hr = fromMaybe repoRoot mHangarRoot

      -- Determine binary directory based on mode
      spawnMode <- fromMaybe "docker" <$> getEnv "SPAWN_MODE"
      mBinDirEnv <- getEnv "TIDEPOOL_BIN_DIR"
      let binDir = case (spawnMode, mBinDirEnv) of
            ("zellij", _) -> Paths.runtimeBinDir hr
            (_, Just bd)  -> T.unpack bd
            _             -> "/usr/local/bin"

      -- Early validation: Check for binary existence
      let binPath = Paths.controlServerBin binDir
      binExistsRes <- fileExists binPath
      
      -- Handle fileExists result (Left is error, Right is boolean existence)
      let binExists = case binExistsRes of
            Right True -> True
            _ -> False

      if not binExists
        then pure $ Left (shortId, "Binary missing: " <> T.pack binPath)
        else do
          mBead <- getBead fullId
          case mBead of
            Nothing -> pure $ Left (shortId, "Bead not found: " <> fullId)
            Just bead -> do
              if bead.biStatus == StatusBlocked
                then pure $ Left (shortId, "Bead is blocked: " <> fullId)
                else do
                  let slug = slugify bead.biTitle
                      branchName = "bd-" <> shortId <> "/" <> slug
                      -- New location: <wtBaseDir>/bd-<id>-<slug>
                      targetPath = wtBaseDir </> "bd-" <> T.unpack shortId <> "-" <> T.unpack slug
                      spec = WorktreeSpec
                        {
                          wsBaseName = "bd-" <> shortId
                        , wsFromBranch = Just "origin/main"
                        , wsBranchName = Just branchName
                        , wsPath = Just targetPath
                        }

                  -- Check if worktree directory already exists (idempotency)
                  dirExistsRes <- directoryExists targetPath
                  let dirExists = case dirExistsRes of
                        Right True -> True
                        _ -> False

                  if dirExists
                    then pure $ Left (shortId, "Worktree already exists at " <> T.pack targetPath <> ". Use Zellij tabs or delete worktree first.")
                    else do
                      -- a. Create worktree
                      wtRes <- createWorktree spec
                      case wtRes of
                        Left err -> pure $ Left (shortId, T.pack (show err))
                        Right (WorktreePath path) -> do
                          -- b. Bootstrap .tidepool/ directory structure
                          -- Compute socket paths here (before bootstrap) since we need them for:
                          -- 1. Creating socket directory
                          -- 2. Passing to writeGeminiConfig for MCP config
                          mSocketsPath <- getEnv "TIDEPOOL_SOCKETS_PATH"
                          let socketDir = case (spawnMode, mSocketsPath) of
                                ("zellij", _) -> Paths.socketDirectoryFor shortId
                                (_, Just sp)  -> T.unpack sp </> T.unpack shortId
                                _             -> "/sockets" </> T.unpack shortId
                              controlSocket = Paths.controlSocketPath socketDir
                              tuiSocket = Paths.tuiSocketPath socketDir
                              
                              -- d. Write subagent environment
                              -- Socket paths (socketDir, controlSocket) were computed above.
                              -- Inject HANGAR_ROOT and TIDEPOOL_BIN_DIR to avoid fragile shell discovery.
                              -- Launch backend with appropriate flags for hook execution visibility
                              backendCmd = case backend of
                                    "gemini" -> "gemini --debug"
                                    "claude" -> "claude --debug --verbose"
                                    _        -> "claude --debug --verbose"
                              envVars =
                                [
                                  ("SUBAGENT_CMD", backendCmd)
                                , ("HANGAR_ROOT", T.pack hr)
                                , ("TIDEPOOL_BIN_DIR", T.pack binDir)
                                , ("TIDEPOOL_CONTAINER", "agent-" <> shortId)
                                , ("TIDEPOOL_SOCKET_DIR", T.pack socketDir)
                                , ("TIDEPOOL_CONTROL_SOCKET", T.pack controlSocket)
                                , ("TIDEPOOL_TUI_SOCKET", T.pack tuiSocket)
                                , ("TIDEPOOL_ROLE", "dev")
                                ]

                          bootstrapRes <- bootstrapTidepool mHangarRoot repoRoot path backend socketDir controlSocket envVars
                          case bootstrapRes of
                            Left errMsg -> pure $ Left (shortId, "Worktree created but bootstrap failed: " <> errMsg)
                            Right () -> do
                              -- c. Write bead context
                              let context = buildBeadContext bead branchName
                              contextRes <- writeBeadContext path context
                              case contextRes of
                                Left errMsg -> pure $ Left (shortId, "Worktree created but context write failed: " <> errMsg)
                                Right () -> do
                                  -- e. Launch agent (Zellij or Docker)
                                  spawnModeLocal <- fromMaybe "docker" <$> getEnv "SPAWN_MODE"
                                  
                                  case spawnModeLocal of
                                    "zellij" -> do
                                      -- Old path: Launch Zellij tab with layout that runs process-compose
                                      let tabConfig = TabConfig
                                            {
                                              tcName = shortId
                                            , tcLayout = repoRoot </> ".zellij" </> "worktree.kdl"
                                            , tcCwd = path
                                            , tcEnv = envVars
                                            , tcCommand = Nothing
                                            }
                                      tabRes <- newTab tabConfig
                                      case tabRes of
                                        Left err -> pure $ Left (shortId, "Worktree created but tab launch failed: " <> T.pack (show err))
                                        Right tabId -> pure $ Right (shortId, path, tabId)
                                    
                                    _ -> do
                                      -- New path: Spawn Docker container and attach via Zellij
                                      let config = SpawnConfig
                                            { scBeadId = shortId
                                            , scWorktreePath = path
                                            , scBackend = backend
                                            , scUid = Nothing
                                            , scGid = Nothing
                                            }
                                      containerResult <- spawnContainer config
                                      
                                      case containerResult of
                                        Left err -> pure $ Left (shortId, "Worktree created but container spawn failed: " <> T.pack (show err))
                                        Right containerId -> do
                                          -- Create Zellij tab with docker attach
                                          let attachCmd = "docker attach " <> containerId.unContainerId
                                              tabConfig = TabConfig
                                                { tcName = shortId
                                                , tcLayout = ""
                                                , tcCwd = path
                                                , tcEnv = envVars
                                                , tcCommand = Just attachCmd
                                                }
                                          
                                          tabRes <- newTab tabConfig
                                          case tabRes of
                                            Left err -> pure $ Left (shortId, "Container spawned (" <> containerId.unContainerId <> ") but tab launch failed: " <> T.pack (show err))
                                            Right tabId -> pure $ Right (shortId, path, tabId)


-- | Bootstrap .tidepool/ directory structure in a worktree.
bootstrapTidepool
  :: (Member FileSystem es, Member Env es)
  => Maybe FilePath  -- ^ Hangar root (for templates)
  -> FilePath        -- ^ Repo root (for symlink source)
  -> FilePath        -- ^ Worktree path
  -> Text            -- ^ Backend ("claude" or "gemini")
  -> FilePath        -- ^ Socket directory (e.g., /tmp/tidepool-<id>)
  -> FilePath        -- ^ Control socket path (e.g., /tmp/tidepool-<id>/control.sock)
  -> [(Text, Text)]  -- ^ Subagent environment variables
  -> Eff es (Either Text ()) 
bootstrapTidepool mHangarRoot repoRoot worktreePath backend socketDir controlSocket subagentVars = do
  -- Create socket directory in /tmp (avoids SUN_LEN path length limits).
  -- Unix sockets have ~104 byte path limit on macOS; worktree paths can exceed this.
  res0 <- createDirectory socketDir
  case res0 of
    Left err -> pure $ Left $ "Failed to create socket directory " <> T.pack socketDir <> ": " <> T.pack (show err)
    Right () -> do
      -- Create .tidepool directory structure for logs (sockets are in /tmp now).
      let logsDir = worktreePath </> ".tidepool" </> "logs"

      -- Create directories (FileSystem.createDirectory creates parents automatically)
      res2 <- createDirectory logsDir
      case res2 of
        Left err -> pure $ Left $ "Failed to create .tidepool/logs: " <> T.pack (show err)
        Right () -> do
          -- Generate process-compose.yaml from Haskell types
          -- This ensures type safety and removes dependency on external template files
          let binDir = Paths.runtimeBinDir (fromMaybe repoRoot mHangarRoot)
              binPath = Paths.controlServerBin binDir
              tuiSocket = Paths.tuiSocketPath socketDir
              pcConfig = PC.generateSubagentConfig binPath socketDir controlSocket tuiSocket
              dstConfig = worktreePath </> "process-compose.yaml"
              pcYaml = TE.decodeUtf8 $ Yaml.encode pcConfig
          
          writePCRes <- writeFileText dstConfig pcYaml
          case writePCRes of
            Left err -> pure $ Left $ "Failed to write process-compose.yaml: " <> T.pack (show err)
            Right () -> do
              -- Write custom .env file (merging root .env and subagent config)
              -- This replaces the symlink approach which caused variable shadowing issues.
              -- We snapshot the current environment (Haskell as source of truth) to ensure
              -- all secrets and config are propagated, while overriding socket paths.
              rootEnvVars <- getEnvironment
              
              -- Filter out any keys from the root env that are explicitly set in subagentVars.
              -- This ensures subagent-defined values always take precedence in the merged env.
              let subagentKeys     = map fst subagentVars
                  filteredRootVars = filter (\(k, _) -> k `notElem` subagentKeys) rootEnvVars
                  
                  -- Merge vars: subagent vars take precedence
                  mergedVars = filteredRootVars ++ subagentVars
                  
                  -- Generate .env content
                  dstEnv = worktreePath </> ".env"
                  escapeEnvValue v = T.replace "\"" "\\\"" (T.replace "\\" "\\\\" v)
                  envContent = T.unlines [ k <> "=\"" <> escapeEnvValue v <> "\"" | (k, v) <- mergedVars ]

              writeRes <- writeFileText dstEnv envContent
              case writeRes of
                Left err -> pure $ Left $ "Failed to write .env: " <> T.pack (show err)
                Right () -> do
                  -- Write Claude local settings with SessionStart hooks
                  -- This bypasses FSWatcher issues with project-scope settings in worktrees
                  -- Use hangar root (or repo root fallback) to build absolute binary path
                  let hr = fromMaybe repoRoot mHangarRoot
                  settingsRes <- writeClaudeLocalSettings hr worktreePath
                  case settingsRes of
                    Left err -> pure $ Left $ "Failed to write Claude settings: " <> err
                    Right () -> do
                      -- Write .mcp.json for Claude Code
                      mcpRes <- writeClaudeMcpConfig controlSocket worktreePath "dev"
                      case mcpRes of
                        Left err -> pure $ Left $ "Failed to write MCP config: " <> err
                        Right () -> do
                          -- Write Gemini config if backend is "gemini"
                          geminiRes <- if backend == "gemini"
                                         then writeGeminiConfig hr worktreePath controlSocket "dev"
                                         else pure $ Right ()
                          case geminiRes of
                            Left err -> pure $ Left $ "Failed to write Gemini config: " <> err
                            Right () -> do
                              -- Update .gitignore for both backends
                              gitignoreRes <- appendBackendToGitignore worktreePath
                              case gitignoreRes of
                                Left err -> pure $ Left $ "Failed to update .gitignore: " <> err
                                Right () -> pure $ Right ()


-- | Write bead context to .claude/context/bead.md.
writeBeadContext
  :: (Member FileSystem es)
  => FilePath  -- ^ Worktree path
  -> Text      -- ^ Context content
  -> Eff es (Either Text ()) 
writeBeadContext worktreePath context = do
  let contextDir = worktreePath </> ".claude" </> "context"
      contextFile = contextDir </> "bead.md"

  dirRes <- createDirectory contextDir
  case dirRes of
    Left err -> pure $ Left $ T.pack (show err)
    Right () -> do
      writeRes <- writeFileText contextFile context
      case writeRes of
        Left err -> pure $ Left $ T.pack (show err)
        Right () -> pure $ Right ()


-- | Common helper for writing backend settings files.
-- Reduces duplication between Claude and Gemini config generation.
writeBackendSettings
  :: (Member FileSystem es)
  => FilePath       -- ^ Directory to create (e.g., ".claude", ".gemini")
  -> FilePath       -- ^ Settings file path
  -> Aeson.Value    -- ^ JSON settings object
  -> Text           -- ^ Directory name for error messages
  -> Text           -- ^ File name for error messages
  -> Eff es (Either Text ()) 
writeBackendSettings configDir settingsFile settings dirName fileName = do
  let content = TE.decodeUtf8 . BL.toStrict $ encode settings

  dirRes <- createDirectory configDir
  case dirRes of
    Left err -> pure $ Left $ "Failed to create " <> dirName <> " directory: " <> T.pack (show err)
    Right () -> do
      writeRes <- writeFileText settingsFile content
      case writeRes of
        Left err -> pure $ Left $ "Failed to write " <> fileName <> ": " <> T.pack (show err)
        Right () -> pure $ Right ()


-- | Write .claude/settings.local.json with SessionStart hooks.
-- This bypasses FSWatcher issues with project-scope settings in worktrees.
-- Uses absolute path to mantle-agent (no environment variable dependency).
writeClaudeLocalSettings
  :: (Member FileSystem es)
  => FilePath  -- ^ Hangar root (for building absolute binary path)
  -> FilePath  -- ^ Worktree path
  -> Eff es (Either Text ()) 
writeClaudeLocalSettings hangarRoot worktreePath = do
  let claudeDir = worktreePath </> ".claude"
      settingsFile = claudeDir </> "settings.local.json"
      binDir = Paths.runtimeBinDir hangarRoot
      mantleAgentPath = Paths.mantleAgentBin binDir

      settings = object
        [
          "hooks" .= object
          [
            "SessionStart" .= 
              [
                object
                  [
                    "matcher" .= ("startup" :: Text)
                  , "hooks" .= 
                    [
                      object
                        [
                          "type" .= ("command" :: Text)
                        , "command" .= (T.pack mantleAgentPath <> " hook session-start --role=dev")
                        ]
                    ]
                  ]
                ,
                object
                  [
                    "matcher" .= ("resume" :: Text)
                  , "hooks" .=
                    [
                      object
                        [
                          "type" .= ("command" :: Text)
                        , "command" .= (T.pack mantleAgentPath <> " hook session-start --role=dev")
                        ]
                    ]
                  ]
              ]
          ]
        ]

  writeBackendSettings claudeDir settingsFile settings ".claude" "settings.local.json"


-- | Write .mcp.json for Claude Code MCP configuration.
-- Points to control-server via role-prefixed endpoint.
writeClaudeMcpConfig
  :: (Member FileSystem es)
  => FilePath  -- ^ Control socket path
  -> FilePath  -- ^ Worktree path
  -> Text      -- ^ Role (dev, tl, pm)
  -> Eff es (Either Text ())
writeClaudeMcpConfig controlSocket worktreePath role = do
  let mcpFile = worktreePath </> ".mcp.json"

      -- Build the HTTP+Unix URL with role path
      -- Format: http+unix://<socket_path>/role/<role>/mcp
      socketUrl = "http+unix://" <> T.pack controlSocket <> "/role/" <> role <> "/mcp"

      config = object
        [ "mcpServers" .= object
          [ "tidepool" .= object
            [ "transport" .= ("http" :: Text)
            , "url" .= socketUrl
            ]
          ]
        ]

      content = TE.decodeUtf8 . BL.toStrict $ encode config

  writeRes <- writeFileText mcpFile content
  case writeRes of
    Left err -> pure $ Left $ "Failed to write .mcp.json: " <> T.pack (show err)
    Right () -> pure $ Right ()


-- | Write .gemini/settings.json with hooks and MCP configuration.
-- Gemini combines hooks and MCP in a single file (unlike Claude).
-- Uses absolute paths since Gemini CLI doesn't expand env vars in settings.json.
writeGeminiConfig
  :: (Member FileSystem es)
  => FilePath  -- ^ Hangar root (for absolute path to mantle-agent)
  -> FilePath  -- ^ Worktree path
  -> FilePath  -- ^ Control socket path (short path in /tmp to avoid SUN_LEN limits)
  -> Text      -- ^ Role
  -> Eff es (Either Text ()) 
writeGeminiConfig hangarRoot worktreePath controlSocket role = do
  let geminiDir = worktreePath </> ".gemini"
      settingsFile = geminiDir </> "settings.json"
      -- Gemini CLI doesn't expand $GEMINI_PROJECT_DIR, so use absolute paths
      binDir = Paths.runtimeBinDir hangarRoot
      mantleAgent = Paths.mantleAgentBin binDir
      hookCmd = mantleAgent <> " hook session-start --role=" <> T.unpack role
      -- controlSocket is passed in (short path in /tmp to avoid SUN_LEN limits)

      settings = object
        [
          "hooksConfig" .= object
          [
            "enabled" .= True
          ]
        , "hooks" .= object
          [
            "SessionStart" .= 
              [
                object
                  [
                    "matcher" .= ("startup" :: Text)
                  , "hooks" .= 
                    [
                      object
                        [
                          "name" .= ("init-agent" :: Text)
                        , "type" .= ("command" :: Text)
                        , "command" .= T.pack hookCmd
                        ]
                    ]
                  ]
                ,
                object
                  [
                    "matcher" .= ("resume" :: Text)
                  , "hooks" .= 
                    [
                      object
                        [
                          "name" .= ("resume-agent" :: Text)
                        , "type" .= ("command" :: Text)
                        , "command" .= T.pack hookCmd
                        ]
                    ]
                  ]
              ]
          ]
        , "mcpServers" .= object
          [
            "tidepool" .= object
            [
              "command" .= T.pack mantleAgent
            , "args" .= (["mcp", "--role", role] :: [Text])
            , "env" .= object
              [
                "TIDEPOOL_CONTROL_SOCKET" .= T.pack controlSocket
              ]
            ]
          ]
        ]

  writeBackendSettings geminiDir settingsFile settings ".gemini" "settings.json"

-- | Append .claude/ and .gemini/ to .gitignore if not already present.
-- Ensures both backend directories are gitignored for consistency.
appendBackendToGitignore
  :: (Member FileSystem es)
  => FilePath  -- ^ Worktree path
  -> Eff es (Either Text ()) 
appendBackendToGitignore worktreePath = do
  let gitignorePath = worktreePath </> ".gitignore"

  -- Read existing .gitignore (or handle non-existent file)
  readRes <- readFileText gitignorePath
  case readRes of
    Left _ -> do
      -- .gitignore doesn't exist, create it with all entries
      let content = T.unlines [".claude/", ".gemini/", ".mcp.json"]
      writeRes <- writeFileText gitignorePath content
      case writeRes of
        Left err -> pure $ Left $ "Failed to create .gitignore: " <> T.pack (show err)
        Right () -> pure $ Right ()

    Right existingContent -> do
      let lines' = T.lines existingContent
          hasClaudeEntry = any (\line -> T.strip line == ".claude/") lines'
          hasGeminiEntry = any (\line -> T.strip line == ".gemini/") lines'
          hasMcpEntry = any (\line -> T.strip line == ".mcp.json") lines'

      -- Determine what needs to be added
      let needsClaudeEntry = not hasClaudeEntry
          needsGeminiEntry = not hasGeminiEntry
          needsMcpEntry = not hasMcpEntry

      if not needsClaudeEntry && not needsGeminiEntry && not needsMcpEntry
        then pure $ Right ()  -- Already has all entries
        else do
          -- Build list of entries to add
          let entriesToAdd = catMaybes
                [
                  if needsClaudeEntry then Just ".claude/" else Nothing
                , if needsGeminiEntry then Just ".gemini/" else Nothing
                , if needsMcpEntry then Just ".mcp.json" else Nothing
                ]
          -- Append missing entries (preserve existing content)
          let newContent = existingContent <> "\n" <> T.unlines entriesToAdd
          writeRes <- writeFileText gitignorePath newContent
          case writeRes of
            Left err -> pure $ Left $ "Failed to update .gitignore: " <> T.pack (show err)
            Right () -> pure $ Right ()
