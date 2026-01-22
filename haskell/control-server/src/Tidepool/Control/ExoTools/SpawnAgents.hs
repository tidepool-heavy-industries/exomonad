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
import Tidepool.Effects.Env (Env, getEnv)
import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo)
import Tidepool.Effects.Worktree (Worktree, WorktreeSpec(..), WorktreePath(..), createWorktree)
import Tidepool.Effects.FileSystem (FileSystem, createDirectory, writeFileText, copyFile, createSymlink, fileExists, directoryExists, readFileText)
import Tidepool.Effects.Zellij (Zellij, TabConfig(..), TabId(..), checkZellijEnv, newTab)
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, arraySchema, emptySchema, SchemaType(..), describeField)

import Tidepool.Control.ExoTools.Internal (slugify)

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

-- | Build markdown context for a bead.
buildBeadContext :: BeadInfo -> Text -> Text
buildBeadContext bead branchName = T.unlines
  [ "# Task: " <> bead.biTitle
  , ""
  , "**ID:** " <> bead.biId
  , "**Status:** " <> statusToText bead.biStatus
  , "**Priority:** " <> T.pack (show bead.biPriority)
  , "**Branch:** " <> branchName
  , ""
  , "## Description"
  , ""
  , fromMaybe "(no description)" bead.biDescription
  , ""
  , "## Dependencies"
  , ""
  , if null bead.biDependencies
      then "None"
      else T.unlines $ map formatDep bead.biDependencies
  , ""
  , "## Workflow"
  , ""
  , "1. Implement changes"
  , "2. Commit: [" <> bead.biId <> "] <description>"
  , "3. Push: git push -u origin " <> branchName
  , "4. File PR: Call the 'file_pr' tool (do NOT use gh cli manually)"
  ]
  where
    statusToText = \case
      StatusOpen -> "open"
      StatusInProgress -> "in_progress"
      StatusClosed -> "closed"
      StatusHooked -> "hooked"
      StatusBlocked -> "blocked"

    formatDep (DependencyInfo { diId = depId, diTitle = depTitle, diStatus = depStatus }) =
      "- " <> depId <> ": " <> depTitle <> " (" <> statusToText depStatus <> ")"

-- | Arguments for spawn_agents tool.
data SpawnAgentsArgs = SpawnAgentsArgs
  { saaBeadIds :: [Text]  -- ^ List of short form bead IDs (e.g. "wzi", "1b2").
  , saaBackend :: Maybe Text  -- ^ Backend to use: "claude" or "gemini" (defaults to "claude").
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema SpawnAgentsArgs where
  jsonSchema = objectSchema
    [ ("bead_ids", describeField "bead_ids" "List of short-form bead IDs to spawn worktrees for." (arraySchema (emptySchema TString)))
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
    [ "bead_ids" .= saaBeadIds args
    , "backend" .= saaBackend args
    ]

-- | Result of spawn_agents tool.
data SpawnAgentsResult = SpawnAgentsResult
  { sarWorktrees :: [(Text, FilePath)]  -- ^ Successfully created worktrees: (shortId, path)
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
    [ "worktrees" .= sarWorktrees res
    , "tabs"      .= sarTabs res
    , "failed"    .= sarFailed res
    ]

-- | Graph definition for spawn_agents tool.
data SpawnAgentsGraph mode = SpawnAgentsGraph
  { saEntry :: mode :- EntryNode SpawnAgentsArgs
      :@ MCPExport
      :@ MCPToolDef '("spawn_agents", "Create worktrees and branches for parallel agent dispatch. Accepts optional 'backend' parameter ('claude' or 'gemini', defaults to 'claude').")

  , saRun :: mode :- LogicNode
      :@ Input SpawnAgentsArgs
      :@ UsesEffects '[BD, Git, Worktree, FileSystem, Zellij, Env, Goto Exit SpawnAgentsResult]

  , saExit :: mode :- ExitNode SpawnAgentsResult
  }
  deriving Generic

-- | Handlers for spawn_agents graph.
spawnAgentsHandlers
  :: (Member BD es, Member Git es, Member Worktree es, Member FileSystem es, Member Zellij es, Member Env es)
  => SpawnAgentsGraph (AsHandler es)
spawnAgentsHandlers = SpawnAgentsGraph
  { saEntry = ()
  , saRun = spawnAgentsLogic
  , saExit = ()
  }

-- | Core logic for spawn_agents.
spawnAgentsLogic
  :: (Member BD es, Member Git es, Member Worktree es, Member FileSystem es, Member Zellij es, Member Env es)
  => SpawnAgentsArgs
  -> Eff es (GotoChoice '[To Exit SpawnAgentsResult])
spawnAgentsLogic args = do
  -- 1. Check Zellij environment (fail if not in Zellij)
  mZellijSession <- checkZellijEnv
  case mZellijSession of
    Nothing -> pure $ gotoExit $ SpawnAgentsResult
      { sarWorktrees = []
      , sarTabs = []
      , sarFailed = [("*", "Not running in Zellij session")]
      }
    Just _ -> do
      -- 2. Determine Hangar Root and Worktree Base Path
      mHangarRoot <- findHangarRoot
      mWtInfo <- getWorktreeInfo
      let repoRoot = maybe "." (\wi -> wi.wiRepoRoot) mWtInfo
          -- Target directory: <hangar>/worktrees/ or repo/.worktrees/tidepool/ as fallback
          wtBaseDir = case mHangarRoot of
            Just hr -> hr </> "worktrees"
            Nothing -> repoRoot </> ".worktrees" </> "tidepool"

      -- 3. Validate and normalize backend parameter
      let backendRaw = T.toLower $ fromMaybe "claude" args.saaBackend
          validBackends = ["claude", "gemini"]

      if backendRaw `notElem` validBackends
        then pure $ gotoExit $ SpawnAgentsResult
          { sarWorktrees = []
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
            { sarWorktrees = worktrees
            , sarTabs = tabs
            , sarFailed = failed
            }


-- | Process a single bead: create worktree, bootstrap .tidepool/, write context, launch tab.
processBead
  :: (Member BD es, Member Worktree es, Member FileSystem es, Member Zellij es)
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

      -- Early validation: Check for binary existence
      let binPath = hr </> "runtime" </> "bin" </> "tidepool-control-server"
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
                        { wsBaseName = "bd-" <> shortId
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
                          bootstrapRes <- bootstrapTidepool mHangarRoot repoRoot path backend
                          case bootstrapRes of
                            Left errMsg -> pure $ Left (shortId, "Worktree created but bootstrap failed: " <> errMsg)
                            Right () -> do
                              -- c. Write bead context
                              let context = buildBeadContext bead branchName
                              contextRes <- writeBeadContext path context
                              case contextRes of
                                Left errMsg -> pure $ Left (shortId, "Worktree created but context write failed: " <> errMsg)
                                Right () -> do
                                  -- d. Write subagent environment
                                  -- Explicitly set sockets to relative paths to ensure isolation from root instance.
                                  -- Inject HANGAR_ROOT and TIDEPOOL_BIN_DIR to avoid fragile shell discovery.
                                  -- Launch backend with appropriate flags for hook execution visibility
                                  -- (backend is already validated and normalized to lowercase)
                                  let backendCmd = case backend of
                                        "gemini" -> "gemini --debug --verbose"
                                        "claude" -> "claude --debug --verbose"
                                        _        -> "claude --debug --verbose"  -- Unreachable: validation ensures only claude|gemini
                                      envVars =
                                        [ ("SUBAGENT_CMD", backendCmd)
                                        , ("HANGAR_ROOT", T.pack hr)
                                        , ("TIDEPOOL_BIN_DIR", T.pack $ hr </> "runtime" </> "bin")
                                        , ("TIDEPOOL_CONTROL_SOCKET", ".tidepool/sockets/control.sock")
                                        , ("TIDEPOOL_TUI_SOCKET", ".tidepool/sockets/tui.sock")
                                        ]
                                  envRes <- writeEnvFile path envVars
                                  case envRes of
                                    Left errMsg -> pure $ Left (shortId, "Worktree created but env write failed: " <> errMsg)
                                    Right () -> do
                                      -- e. Launch Zellij tab
                                      -- Use absolute path for layout to avoid CWD resolution issues
                                      let tabConfig = TabConfig
                                            { tcName = shortId
                                            , tcLayout = repoRoot </> ".zellij" </> "worktree.kdl"
                                            , tcCwd = path
                                            , tcEnv = envVars
                                            }
                                      tabRes <- newTab tabConfig
                                      case tabRes of
                                        Left err -> pure $ Left (shortId, "Worktree created but tab launch failed: " <> T.pack (show err))
                                        Right tabId -> pure $ Right (shortId, path, tabId)


-- | Bootstrap .tidepool/ directory structure in a worktree.
bootstrapTidepool
  :: (Member FileSystem es)
  => Maybe FilePath  -- ^ Hangar root (for templates)
  -> FilePath        -- ^ Repo root (for symlink source)
  -> FilePath        -- ^ Worktree path
  -> Text            -- ^ Backend ("claude" or "gemini")
  -> Eff es (Either Text ())
bootstrapTidepool mHangarRoot repoRoot worktreePath backend = do
  -- Create .tidepool directory structure.
  -- These MUST exist for process-compose log tailing and control sockets.
  let socketsDir = worktreePath </> ".tidepool" </> "sockets"
      logsDir = worktreePath </> ".tidepool" </> "logs"

  -- Create directories (FileSystem.createDirectory creates parents automatically)
  res1 <- createDirectory socketsDir
  case res1 of
    Left err -> pure $ Left $ "Failed to create .tidepool/sockets: " <> T.pack (show err)
    Right () -> do
      res2 <- createDirectory logsDir
      case res2 of
        Left err -> pure $ Left $ "Failed to create .tidepool/logs: " <> T.pack (show err)
        Right () -> do
          -- Copy subagent process-compose template
          let srcTemplate = case mHangarRoot of
                Just hr -> hr </> "templates" </> "subagent-pc.yaml"
                Nothing -> repoRoot </> ".tidepool" </> "templates" </> "subagent-pc.yaml"  -- fallback
              dstConfig = worktreePath </> "process-compose.yaml"
          copyRes <- copyFile srcTemplate dstConfig
          case copyRes of
            Left err -> pure $ Left $ "Failed to copy process-compose.yaml: " <> T.pack (show err)
            Right () -> do
              -- Symlink .env from repo root
              let srcEnv = repoRoot </> ".env"
                  dstEnv = worktreePath </> ".env"
              symlinkRes <- createSymlink srcEnv dstEnv
              case symlinkRes of
                Left err -> pure $ Left $ "Failed to symlink .env: " <> T.pack (show err)
                Right () -> do
                  -- Write Claude local settings with SessionStart hooks
                  -- This bypasses FSWatcher issues with project-scope settings in worktrees
                  -- Use hangar root (or repo root fallback) to build absolute binary path
                  let hr = fromMaybe repoRoot mHangarRoot
                  settingsRes <- writeClaudeLocalSettings hr worktreePath
                  case settingsRes of
                    Left err -> pure $ Left $ "Failed to write Claude settings: " <> err
                    Right () -> do
                      -- Write Gemini config if backend is "gemini"
                      geminiRes <- if backend == "gemini"
                                     then writeGeminiConfig hr worktreePath
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


-- | Write environment variables to a file (e.g. .env.subagent).
writeEnvFile
  :: (Member FileSystem es)
  => FilePath  -- ^ Worktree path
  -> [(Text, Text)] -- ^ Env vars
  -> Eff es (Either Text ())
writeEnvFile worktreePath envVars = do
  -- Validate all values are non-empty
  let invalid = filter (T.null . snd) envVars
  if not (null invalid)
    then pure $ Left $ "Empty env vars: " <> T.pack (show invalid)
    else do
      let envFile = worktreePath </> ".env.subagent"
          -- Quote values to handle spaces (e.g., "claude --debug --verbose")
          content = T.unlines [ "export " <> k <> "=\"" <> v <> "\"" | (k, v) <- envVars ]
      writeRes <- writeFileText envFile content
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
      mantleAgentPath = hangarRoot </> "runtime" </> "bin" </> "mantle-agent"

      settings = object
        [ "hooks" .= object
          [ "SessionStart" .=
            [ object
              [ "matcher" .= ("startup" :: Text)
              , "hooks" .=
                [ object
                  [ "type" .= ("command" :: Text)
                  , "command" .= (T.pack mantleAgentPath <> " hook session-start")
                  ]
                ]
              ]
            , object
              [ "matcher" .= ("resume" :: Text)
              , "hooks" .=
                [ object
                  [ "type" .= ("command" :: Text)
                  , "command" .= (T.pack mantleAgentPath <> " hook session-start")
                  ]
                ]
              ]
            ]
          ]
        ]

  writeBackendSettings claudeDir settingsFile settings ".claude" "settings.local.json"


-- | Write .gemini/settings.json with hooks and MCP configuration.
-- Gemini combines hooks and MCP in a single file (unlike Claude).
-- Uses $GEMINI_PROJECT_DIR path variable for portability.
writeGeminiConfig
  :: (Member FileSystem es)
  => FilePath  -- ^ Hangar root (unused for Gemini, kept for API consistency)
  -> FilePath  -- ^ Worktree path
  -> Eff es (Either Text ())
writeGeminiConfig _hangarRoot worktreePath = do
  let geminiDir = worktreePath </> ".gemini"
      settingsFile = geminiDir </> "settings.json"

      settings = object
        [ "hooksConfig" .= object
          [ "enabled" .= True
          ]
        , "hooks" .= object
          [ "SessionStart" .=
            [ object
              [ "matcher" .= ("startup" :: Text)
              , "hooks" .=
                [ object
                  [ "name" .= ("init-agent" :: Text)
                  , "type" .= ("command" :: Text)
                  , "command" .= ("$GEMINI_PROJECT_DIR/../runtime/bin/mantle-agent hook session-start" :: Text)
                  ]
                ]
              ]
            , object
              [ "matcher" .= ("resume" :: Text)
              , "hooks" .=
                [ object
                  [ "name" .= ("resume-agent" :: Text)
                  , "type" .= ("command" :: Text)
                  , "command" .= ("$GEMINI_PROJECT_DIR/../runtime/bin/mantle-agent hook session-start" :: Text)
                  ]
                ]
              ]
            ]
          ]
        , "mcpServers" .= object
          [ "tidepool" .= object
            [ "command" .= ("$GEMINI_PROJECT_DIR/../runtime/bin/mantle-agent" :: Text)
            , "args" .= (["mcp"] :: [Text])
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
      -- .gitignore doesn't exist, create it with both entries
      let content = T.unlines [".claude/", ".gemini/"]
      writeRes <- writeFileText gitignorePath content
      case writeRes of
        Left err -> pure $ Left $ "Failed to create .gitignore: " <> T.pack (show err)
        Right () -> pure $ Right ()

    Right existingContent -> do
      let lines' = T.lines existingContent
          hasClaudeEntry = any (\line -> T.strip line == ".claude/") lines'
          hasGeminiEntry = any (\line -> T.strip line == ".gemini/") lines'

      -- Determine what needs to be added
      let needsClaudeEntry = not hasClaudeEntry
          needsGeminiEntry = not hasGeminiEntry

      if not needsClaudeEntry && not needsGeminiEntry
        then pure $ Right ()  -- Already has both entries
        else do
          -- Build list of entries to add
          let entriesToAdd = catMaybes
                [ if needsClaudeEntry then Just ".claude/" else Nothing
                , if needsGeminiEntry then Just ".gemini/" else Nothing
                ]
          -- Append missing entries (preserve existing content)
          let newContent = existingContent <> "\n" <> T.unlines entriesToAdd
          writeRes <- writeFileText gitignorePath newContent
          case writeRes of
            Left err -> pure $ Left $ "Failed to update .gitignore: " <> T.pack (show err)
            Right () -> pure $ Right ()
