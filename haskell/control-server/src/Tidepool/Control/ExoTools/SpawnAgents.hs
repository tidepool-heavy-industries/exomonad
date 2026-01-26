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
  , findHangarRoot
  )
where

import Control.Monad (forM)
import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=), object, withObject)
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.FilePath ((</>), takeDirectory)

import Tidepool.Effects.GitHub (GitHub, Issue(..), IssueState(..), Repo(..), getIssue)
import Tidepool.Effects.DockerSpawner (DockerSpawner, SpawnConfig(..), spawnContainer, ContainerId(..))
import Tidepool.Effects.Env (Env, getEnv)
import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo)
import Tidepool.Effects.Worktree (Worktree, WorktreeSpec(..), WorktreePath(..), createWorktree)

import Tidepool.Effects.FileSystem (FileSystem, fileExists, directoryExists)
import Tidepool.Effects.Zellij (Zellij, TabConfig(..), TabId(..), checkZellijEnv, newTab)
import Tidepool.Role (Role(..))
import Tidepool.Graph.Generic (AsHandler, type (:-))
import Tidepool.Graph.Generic.Core (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)
import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, MCPExport, MCPToolDef, MCPRoleHint)
import Tidepool.Schema (HasJSONSchema(..), objectSchema, arraySchema, emptySchema, SchemaType(..), describeField)

import Tidepool.Control.ExoTools.Internal (slugify)
import Tidepool.Control.Runtime.Paths as Paths

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

-- | Arguments for spawn_agents tool.
data SpawnAgentsArgs = SpawnAgentsArgs
  { saaIssueNumbers :: [Text]  -- ^ List of issue numbers (e.g. "123", "456").
  , saaBackend :: Maybe Text  -- ^ Backend to use: "claude" or "gemini" (defaults to "claude").
  }
  deriving stock (Show, Eq, Generic)

instance HasJSONSchema SpawnAgentsArgs where
  jsonSchema = objectSchema
    [
      ("issue_numbers", describeField "issue_numbers" "List of issue numbers to spawn worktrees for." (arraySchema (emptySchema TString)))
    , ("backend", describeField "backend" "Backend to use: 'claude' or 'gemini' (defaults to 'claude')." (emptySchema TString))
    ]
    ["issue_numbers"]

instance FromJSON SpawnAgentsArgs where
  parseJSON = withObject "SpawnAgentsArgs" $ \v ->
    SpawnAgentsArgs
      <$> v .: "issue_numbers"
      <*> v .:? "backend"

instance ToJSON SpawnAgentsArgs where
  toJSON args = object
    [
      "issue_numbers" .= saaIssueNumbers args
    , "backend" .= saaBackend args
    ]

-- | Result of spawn_agents tool.
data SpawnAgentsResult = SpawnAgentsResult
  {
    sarWorktrees :: [(Text, FilePath)]  -- ^ Successfully created worktrees: (issueNum, path)
  , sarTabs      :: [(Text, Text)]      -- ^ Successfully launched tabs: (issueNum, tabId)
  , sarFailed    :: [(Text, Text)]      -- ^ Failed operations: (issueNum, reason)
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

-- | Spawn mode for agents.
data SpawnMode
  = SpawnZellij  -- ^ Use local Zellij tabs (legacy)
  | SpawnDocker  -- ^ Use Docker containers via DockerSpawner
  deriving stock (Show, Eq)

parseSpawnMode :: Maybe Text -> SpawnMode
parseSpawnMode (Just "zellij") = SpawnZellij
parseSpawnMode _               = SpawnDocker

-- | Graph definition for spawn_agents tool.
data SpawnAgentsGraph mode = SpawnAgentsGraph
  {
    saEntry :: mode :- EntryNode SpawnAgentsArgs
      :@ MCPExport
      :@ MCPToolDef '("spawn_agents", "Create worktrees and branches for parallel agent dispatch. Accepts optional 'backend' parameter ('claude' or 'gemini', defaults to 'claude').")
      :@ MCPRoleHint 'TL

  , saRun :: mode :- LogicNode
      :@ Input SpawnAgentsArgs
      :@ UsesEffects '[GitHub, Git, Worktree, FileSystem, Zellij, Env, DockerSpawner, Goto Exit SpawnAgentsResult]

  , saExit :: mode :- ExitNode SpawnAgentsResult
  }
  deriving Generic

-- | Handlers for spawn_agents graph.
spawnAgentsHandlers
  :: (Member GitHub es, Member Git es, Member Worktree es, Member FileSystem es, Member Zellij es, Member Env es, Member DockerSpawner es)
  => SpawnAgentsGraph (AsHandler es)
spawnAgentsHandlers = SpawnAgentsGraph
  {
    saEntry = ()
  , saRun = spawnAgentsLogic
  , saExit = ()
  }

-- | Core logic for spawn_agents.
spawnAgentsLogic
  :: (Member GitHub es, Member Git es, Member Worktree es, Member FileSystem es, Member Zellij es, Member Env es, Member DockerSpawner es)
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
      spawnModeEnv <- getEnv "SPAWN_MODE"
      let spawnMode = parseSpawnMode spawnModeEnv
      mHangarRoot <- findHangarRoot
      mWtInfo <- getWorktreeInfo
      let repoRoot = maybe "." (\wi -> wi.wiRepoRoot) mWtInfo
      
      -- Target directory discovery
      wtBaseDir <- case (spawnMode, mHangarRoot) of
        (SpawnZellij, Just hr) -> pure $ hr </> "worktrees"
        (SpawnZellij, Nothing) -> pure $ repoRoot </> ".worktrees" </> "tidepool"
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
          -- 4. Process each issue
          results <- forM args.saaIssueNumbers $ \shortId -> do
            processIssue spawnMode mHangarRoot repoRoot wtBaseDir backendRaw shortId

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


-- | Process a single issue: create worktree, bootstrap .tidepool/, write context, launch tab.
processIssue
  :: (Member GitHub es, Member Worktree es, Member FileSystem es, Member Zellij es, Member Env es, Member DockerSpawner es)
  => SpawnMode                                         -- ^ Spawn mode (Zellij or Docker)
  -> Maybe FilePath                                    -- ^ Hangar root (for templates)
  -> FilePath                                          -- ^ Repo root (for symlinks/templates)
  -> FilePath                                          -- ^ Worktree base directory
  -> Text                                              -- ^ Backend ("claude" or "gemini")
  -> Text                                              -- ^ Issue number as text
  -> Eff es (Either (Text, Text) (Text, FilePath, TabId))  -- ^ Left (id, error) or Right (id, path, tabId)
processIssue spawnMode mHangarRoot repoRoot wtBaseDir backend shortId = do
  -- Validate shortId (prevent path traversal)
  if T.any (\c -> c == '/' || c == '\\') shortId
    then pure $ Left (shortId, "Invalid issue ID: contains path separators")
    else do
      let hr = fromMaybe repoRoot mHangarRoot
          issueNum = case (reads (T.unpack shortId) :: [(Int, String)]) of
            [(n, "")] -> Just n
            _ -> Nothing

      case issueNum of
        Nothing -> pure $ Left (shortId, "Invalid issue number: " <> shortId)
        Just n -> do
          -- Determine binary directory based on mode
          mBinDirEnv <- getEnv "TIDEPOOL_BIN_DIR"
          let binDir = case (spawnMode, mBinDirEnv) of
                (SpawnZellij, _) -> Paths.runtimeBinDir hr
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
              -- Get repo from env var or use default
              mEnvRepo <- getEnv "GITHUB_REPO"
              let repo = Repo $ fromMaybe "tidepool-heavy-industries/tidepool" mEnvRepo
              mIssue <- getIssue repo n False
              case mIssue of
                Nothing -> pure $ Left (shortId, "Issue not found: " <> shortId)
                Just issue -> do
                  if issue.issueState == IssueClosed
                    then pure $ Left (shortId, "Issue is closed: " <> shortId)
                    else do
                      let slug = slugify issue.issueTitle
                          branchName = "gh-" <> shortId <> "/" <> slug
                          -- New location: <wtBaseDir>/gh-<id>-<slug>
                          targetPath = wtBaseDir </> "gh-" <> T.unpack shortId <> "-" <> T.unpack slug
                          spec = WorktreeSpec
                            {
                              wsBaseName = "gh-" <> shortId
                            , wsFromBranch = Just "origin/main"
                            , wsBranchName = Just branchName
                            , wsPath = Just targetPath
                            }

                      -- Check if worktree directory already exists (idempotency)
                      dirExistsRes <- directoryExists targetPath
                      let dirExists = case dirExistsRes of
                            Right True -> True
                            _ -> False

                      -- Idempotency: if worktree exists, skip creation and reuse it
                      -- This allows re-launching a Zellij tab for an existing worktree
                      mPath <- if dirExists
                        then pure $ Right targetPath  -- Reuse existing worktree
                        else do
                          -- a. Create worktree (only if it doesn't exist)
                          wtRes <- createWorktree spec
                          case wtRes of
                            Left err -> pure $ Left (shortId, T.pack (show err))
                            Right (WorktreePath p) -> pure $ Right p

                      case mPath of
                        Left errTuple -> pure $ Left errTuple
                        Right path -> do
                          -- b. Configure environment for TCP control-server
                          let backendCmd = case backend of
                                    "gemini" -> "gemini --debug"
                                    "claude" -> "claude --debug --verbose"
                                    _        -> "claude --debug --verbose"
                              -- Subagents connect to shared control-server via TCP
                              -- No local control-server startup needed
                              envVars =
                                [
                                  ("SUBAGENT_CMD", backendCmd)
                                , ("CONTROL_SERVER_URL", "http://control-server:7432")
                                , ("TIDEPOOL_ROLE", "dev")
                                ]

                          -- Skip bootstrapTidepool - subagents use shared control-server
                          -- entrypoint.sh writes .mcp.json based on CONTROL_SERVER_URL
                          -- Issue context comes from GitHub (no bead.md needed)

                          -- c. Launch agent (Zellij or Docker)
                          case spawnMode of
                            SpawnZellij -> do
                              let tabConfig = TabConfig
                                    { tcName = shortId
                                    , tcLayout = repoRoot </> ".zellij" </> "worktree.kdl"
                                    , tcCwd = path
                                    , tcEnv = envVars
                                    , tcCommand = Nothing
                                    }
                              tabRes <- newTab tabConfig
                              case tabRes of
                                Left err -> pure $ Left (shortId, "Tab launch failed: " <> T.pack (show err))
                                Right tabId -> pure $ Right (shortId, path, tabId)

                            SpawnDocker -> do
                              let config = SpawnConfig
                                    { scIssueId = shortId
                                    , scWorktreePath = path
                                    , scBackend = backend
                                    , scUid = Nothing
                                    , scGid = Nothing
                                    , scEnv = envVars
                                    }
                              containerResult <- spawnContainer config
                              case containerResult of
                                Left err -> pure $ Left (shortId, "Container spawn failed: " <> T.pack (show err))
                                Right containerId -> do
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
