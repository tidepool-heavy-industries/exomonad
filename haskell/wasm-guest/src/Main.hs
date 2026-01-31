{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Exception (SomeException, try)
import Control.Monad.Freer
import Data.Aeson (FromJSON, ToJSON, Value, object, (.=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text, pack)
import qualified Data.Text as T
import ExoMonad.Guest.HostCall
import ExoMonad.Guest.Effects.AgentControl qualified as AC
import ExoMonad.Guest.Effects.FileSystem qualified as FS
import ExoMonad.Guest.Tools qualified as Tools
import Extism.PDK (input, output)
import Foreign.C.Types (CInt (..))
import GHC.Generics (Generic)

-- ============================================================================
-- Git Effect (high-level semantic operation)
-- ============================================================================

data Git r where
  GetBranch :: Git String

data GetBranchReq = GetBranchReq
  deriving (Show, Generic)

instance ToJSON GetBranchReq

data GetBranchResp = GetBranchResp {branch :: String}
  deriving (Show, Generic)

instance FromJSON GetBranchResp

runGit :: (LastMember IO effs) => Eff (Git ': effs) a -> Eff effs a
runGit = interpret $ \case
  GetBranch -> sendM $ do
    res <- callHost host_git_get_branch GetBranchReq
    case res of
      Left err -> pure ("Error: " ++ err)
      Right (GetBranchResp b) -> pure b

getBranch :: (Member Git effs) => Eff effs String
getBranch = send GetBranch

-- ============================================================================
-- Log Effect (fire-and-forget)
-- ============================================================================

data Log r where
  LogInfo :: Text -> Log ()

data LogInfoReq = LogInfoReq {message :: Text}
  deriving (Show, Generic)

instance ToJSON LogInfoReq

runLog :: (LastMember IO effs) => Eff (Log ': effs) a -> Eff effs a
runLog = interpret $ \case
  LogInfo msg -> sendM $ callHostVoid host_log_info (LogInfoReq msg)

logInfo :: (Member Log effs) => Text -> Eff effs ()
logInfo msg = send (LogInfo msg)

-- ============================================================================
-- MCP Tool Call Types
-- ============================================================================

data MCPCallInput = MCPCallInput
  { toolName :: Text,
    toolArgs :: Value
  }
  deriving (Show, Generic)

instance FromJSON MCPCallInput where
  parseJSON = Aeson.withObject "MCPCallInput" $ \v ->
    MCPCallInput
      <$> v .: "toolName"
      <*> v .: "toolArgs"

data MCPCallOutput = MCPCallOutput
  { success :: Bool,
    result :: Maybe Value,
    mcpError :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON MCPCallOutput where
  toJSON (MCPCallOutput s r e) =
    object
      [ "success" .= s,
        "result" .= r,
        "error" .= e
      ]

successResult :: Value -> MCPCallOutput
successResult v = MCPCallOutput True (Just v) Nothing

errorResult :: Text -> MCPCallOutput
errorResult msg = MCPCallOutput False Nothing (Just msg)

-- ============================================================================
-- HookOutput types (matches Rust protocol.rs)
-- ============================================================================

data HookOutput = HookOutput
  { continue_ :: Bool,
    stopReason :: Maybe Text,
    suppressOutput :: Maybe Bool,
    systemMessage :: Maybe Text,
    hookSpecificOutput :: Maybe HookSpecificOutput
  }
  deriving (Show, Generic)

instance ToJSON HookOutput where
  toJSON h =
    Aeson.object $
      filter
        ((/= Aeson.Null) . snd)
        [ "continue" .= continue_ h,
          "stopReason" .= stopReason h,
          "suppressOutput" .= suppressOutput h,
          "systemMessage" .= systemMessage h,
          "hookSpecificOutput" .= hookSpecificOutput h
        ]

data HookSpecificOutput
  = PreToolUseOutput
  { permissionDecision :: Text,
    permissionDecisionReason :: Maybe Text,
    updatedInput :: Maybe Value
  }
  deriving (Show, Generic)

instance ToJSON HookSpecificOutput where
  toJSON (PreToolUseOutput decision reason updated) =
    Aeson.object $
      filter
        ((/= Aeson.Null) . snd)
        [ "hookEventName" .= ("PreToolUse" :: Text),
          "permissionDecision" .= decision,
          "permissionDecisionReason" .= reason,
          "updatedInput" .= updated
        ]

-- Helper to create an "allow" response
allowResponse :: Maybe Text -> HookOutput
allowResponse reason =
  HookOutput
    { continue_ = True,
      stopReason = Nothing,
      suppressOutput = Nothing,
      systemMessage = Nothing,
      hookSpecificOutput =
        Just $
          PreToolUseOutput
            { permissionDecision = "allow",
              permissionDecisionReason = reason,
              updatedInput = Nothing
            }
    }

-- ============================================================================
-- HookInput types (matches Rust protocol.rs)
-- ============================================================================

data HookInput = HookInput
  { hiSessionId :: Text,
    hiHookEventName :: Text,
    hiToolName :: Maybe Text,
    hiToolInput :: Maybe Value
  }
  deriving (Show, Generic)

instance FromJSON HookInput where
  parseJSON = Aeson.withObject "HookInput" $ \v ->
    HookInput
      <$> v .: "session_id"
      <*> v .: "hook_event_name"
      <*> v .:? "tool_name"
      <*> v .:? "tool_input"

-- ============================================================================
-- MCP Tool Dispatch
-- ============================================================================

-- | Dispatch an MCP tool call to the appropriate handler.
dispatchTool :: Text -> Value -> IO MCPCallOutput
dispatchTool name args = case name of
  -- Agent control tools
  "spawn_agents" -> handleSpawnAgents args
  "cleanup_agents" -> handleCleanupAgents args
  "list_agents" -> handleListAgents

  -- Filesystem tools
  "read_file" -> handleReadFile args
  "write_file" -> handleWriteFile args

  -- Git tools
  "git_branch" -> handleGitBranch args
  "git_status" -> handleGitStatus args
  "git_log" -> handleGitLog args

  -- GitHub tools
  "github_list_issues" -> handleGitHubListIssues args
  "github_get_issue" -> handleGitHubGetIssue args
  "github_list_prs" -> handleGitHubListPRs args

  -- Unknown tool
  _ -> pure $ errorResult $ "Unknown tool: " <> name

-- ============================================================================
-- Tool Handlers
-- ============================================================================

-- | Handle spawn_agents MCP tool call.
handleSpawnAgents :: Value -> IO MCPCallOutput
handleSpawnAgents args = do
  case parseEither parseSpawnAgentsArgs args of
    Left err -> pure $ errorResult $ "Invalid arguments: " <> pack err
    Right (issueIds, opts) -> do
      result <- runM $ AC.runAgentControl $ AC.spawnAgents issueIds opts
      pure $ successResult $ Aeson.toJSON result

parseSpawnAgentsArgs :: Value -> Parser ([Text], AC.SpawnOptions)
parseSpawnAgentsArgs = Aeson.withObject "spawn_agents args" $ \v -> do
  issues <- v .: "issues"
  owner <- v .: "owner"
  repo <- v .: "repo"
  worktreeDir <- v .:? "worktree_dir"
  pure (issues, AC.SpawnOptions owner repo worktreeDir)

-- | Handle cleanup_agents MCP tool call.
handleCleanupAgents :: Value -> IO MCPCallOutput
handleCleanupAgents args = do
  case parseEither parseCleanupAgentsArgs args of
    Left err -> pure $ errorResult $ "Invalid arguments: " <> pack err
    Right (issueIds, force) -> do
      result <- runM $ AC.runAgentControl $ AC.cleanupAgents issueIds force
      pure $ successResult $ Aeson.toJSON result

parseCleanupAgentsArgs :: Value -> Parser ([Text], Bool)
parseCleanupAgentsArgs = Aeson.withObject "cleanup_agents args" $ \v -> do
  issues <- v .: "issues"
  force <- v .:? "force" >>= \case
    Nothing -> pure False
    Just f -> pure f
  pure (issues, force)

-- | Handle list_agents MCP tool call.
handleListAgents :: IO MCPCallOutput
handleListAgents = do
  result <- runM $ AC.runAgentControl AC.listAgents
  case result of
    Left err -> pure $ errorResult err
    Right agents -> pure $ successResult $ Aeson.toJSON agents

-- | Handle read_file MCP tool call.
handleReadFile :: Value -> IO MCPCallOutput
handleReadFile args = do
  case parseEither parseReadFileArgs args of
    Left err -> pure $ errorResult $ "Invalid arguments: " <> pack err
    Right (path, maxBytes) -> do
      result <- runM $ FS.runFileSystem $ FS.readFile path maxBytes
      case result of
        Left err -> pure $ errorResult err
        Right output -> pure $ successResult $ Aeson.toJSON output

parseReadFileArgs :: Value -> Parser (Text, Int)
parseReadFileArgs = Aeson.withObject "read_file args" $ \v -> do
  path <- v .: "path"
  maxBytes <- v .:? "max_bytes" >>= \case
    Nothing -> pure 0
    Just m -> pure m
  pure (path, maxBytes)

-- | Handle write_file MCP tool call.
handleWriteFile :: Value -> IO MCPCallOutput
handleWriteFile args = do
  case parseEither parseWriteFileArgs args of
    Left err -> pure $ errorResult $ "Invalid arguments: " <> pack err
    Right (path, content, createParents) -> do
      result <- runM $ FS.runFileSystem $ FS.writeFile path content createParents
      case result of
        Left err -> pure $ errorResult err
        Right output -> pure $ successResult $ Aeson.toJSON output

parseWriteFileArgs :: Value -> Parser (Text, Text, Bool)
parseWriteFileArgs = Aeson.withObject "write_file args" $ \v -> do
  path <- v .: "path"
  content <- v .: "content"
  createParents <- v .:? "create_parents" >>= \case
    Nothing -> pure True
    Just c -> pure c
  pure (path, content, createParents)

-- | Handle git_branch MCP tool call.
handleGitBranch :: Value -> IO MCPCallOutput
handleGitBranch _args = do
  br <- runM $ runLog $ runGit getBranch
  pure $ successResult $ Aeson.toJSON br

-- | Handle git_status MCP tool call.
handleGitStatus :: Value -> IO MCPCallOutput
handleGitStatus args = do
  case parseEither parseGitPathArgs args of
    Left err -> pure $ errorResult $ "Invalid arguments: " <> pack err
    Right path -> do
      result <- callHost host_git_get_dirty_files (GitDirtyFilesReq path)
      case result of
        Left err -> pure $ errorResult $ pack err
        Right (resp :: GitDirtyFilesResp) -> pure $ successResult $ Aeson.toJSON resp

data GitDirtyFilesReq = GitDirtyFilesReq { gdfPath :: Maybe Text }
  deriving (Show, Generic)

instance ToJSON GitDirtyFilesReq where
  toJSON (GitDirtyFilesReq p) = object ["path" .= p]

data GitDirtyFilesResp = GitDirtyFilesResp { files :: [Text] }
  deriving (Show, Generic)

instance FromJSON GitDirtyFilesResp
instance ToJSON GitDirtyFilesResp

-- | Handle git_log MCP tool call.
handleGitLog :: Value -> IO MCPCallOutput
handleGitLog args = do
  case parseEither parseGitLogArgs args of
    Left err -> pure $ errorResult $ "Invalid arguments: " <> pack err
    Right (path, limit) -> do
      result <- callHost host_git_get_recent_commits (GitLogReq path limit)
      case result of
        Left err -> pure $ errorResult $ pack err
        Right (resp :: GitLogResp) -> pure $ successResult $ Aeson.toJSON resp

parseGitLogArgs :: Value -> Parser (Maybe Text, Int)
parseGitLogArgs = Aeson.withObject "git_log args" $ \v -> do
  path <- v .:? "path"
  limit <- v .:? "limit" >>= \case
    Nothing -> pure 10
    Just l -> pure l
  pure (path, limit)

data GitLogReq = GitLogReq { glPath :: Maybe Text, glLimit :: Int }
  deriving (Show, Generic)

instance ToJSON GitLogReq where
  toJSON (GitLogReq p l) = object ["path" .= p, "limit" .= l]

data GitLogResp = GitLogResp { commits :: [GitCommit] }
  deriving (Show, Generic)

instance FromJSON GitLogResp
instance ToJSON GitLogResp

data GitCommit = GitCommit
  { gcSha :: Text,
    gcMessage :: Text,
    gcAuthor :: Text,
    gcDate :: Text
  }
  deriving (Show, Generic)

instance FromJSON GitCommit where
  parseJSON = Aeson.withObject "GitCommit" $ \v ->
    GitCommit
      <$> v .: "sha"
      <*> v .: "message"
      <*> v .: "author"
      <*> v .: "date"

instance ToJSON GitCommit where
  toJSON (GitCommit s m a d) =
    object ["sha" .= s, "message" .= m, "author" .= a, "date" .= d]

-- Helper for git path-only args
parseGitPathArgs :: Value -> Parser (Maybe Text)
parseGitPathArgs = Aeson.withObject "git args" $ \v -> v .:? "path"

-- | Handle github_list_issues MCP tool call.
handleGitHubListIssues :: Value -> IO MCPCallOutput
handleGitHubListIssues args = do
  case parseEither parseListIssuesArgs args of
    Left err -> pure $ errorResult $ "Invalid arguments: " <> pack err
    Right req -> do
      result <- callHost host_github_list_issues req
      case result of
        Left err -> pure $ errorResult $ pack err
        Right (resp :: GitHubIssuesResp) -> pure $ successResult $ Aeson.toJSON resp

parseListIssuesArgs :: Value -> Parser GitHubListIssuesReq
parseListIssuesArgs = Aeson.withObject "github_list_issues args" $ \v ->
  GitHubListIssuesReq
    <$> v .: "owner"
    <*> v .: "repo"
    <*> v .:? "state"
    <*> v .:? "labels"

data GitHubListIssuesReq = GitHubListIssuesReq
  { ghliOwner :: Text,
    ghliRepo :: Text,
    ghliState :: Maybe Text,
    ghliLabels :: Maybe [Text]
  }
  deriving (Show, Generic)

instance ToJSON GitHubListIssuesReq where
  toJSON (GitHubListIssuesReq o r s l) =
    object ["owner" .= o, "repo" .= r, "state" .= s, "labels" .= l]

data GitHubIssuesResp = GitHubIssuesResp { ghIssues :: [GitHubIssue] }
  deriving (Show, Generic)

instance FromJSON GitHubIssuesResp where
  parseJSON = Aeson.withObject "GitHubIssuesResp" $ \v ->
    GitHubIssuesResp <$> v .: "issues"

instance ToJSON GitHubIssuesResp where
  toJSON (GitHubIssuesResp is) = object ["issues" .= is]

data GitHubIssue = GitHubIssue
  { ghiNumber :: Int,
    ghiTitle :: Text,
    ghiState :: Text,
    ghiLabels :: [Text],
    ghiCreatedAt :: Text
  }
  deriving (Show, Generic)

instance FromJSON GitHubIssue where
  parseJSON = Aeson.withObject "GitHubIssue" $ \v ->
    GitHubIssue
      <$> v .: "number"
      <*> v .: "title"
      <*> v .: "state"
      <*> v .: "labels"
      <*> v .: "created_at"

instance ToJSON GitHubIssue where
  toJSON (GitHubIssue n t s l c) =
    object ["number" .= n, "title" .= t, "state" .= s, "labels" .= l, "created_at" .= c]

-- | Handle github_get_issue MCP tool call.
handleGitHubGetIssue :: Value -> IO MCPCallOutput
handleGitHubGetIssue args = do
  case parseEither parseGetIssueArgs args of
    Left err -> pure $ errorResult $ "Invalid arguments: " <> pack err
    Right req -> do
      result <- callHost host_github_get_issue req
      case result of
        Left err -> pure $ errorResult $ pack err
        Right (resp :: GitHubIssueDetailResp) -> pure $ successResult $ Aeson.toJSON resp

parseGetIssueArgs :: Value -> Parser GitHubGetIssueReq
parseGetIssueArgs = Aeson.withObject "github_get_issue args" $ \v ->
  GitHubGetIssueReq
    <$> v .: "owner"
    <*> v .: "repo"
    <*> v .: "number"

data GitHubGetIssueReq = GitHubGetIssueReq
  { ghgiOwner :: Text,
    ghgiRepo :: Text,
    ghgiNumber :: Int
  }
  deriving (Show, Generic)

instance ToJSON GitHubGetIssueReq where
  toJSON (GitHubGetIssueReq o r n) =
    object ["owner" .= o, "repo" .= r, "number" .= n]

data GitHubIssueDetailResp = GitHubIssueDetailResp
  { ghidNumber :: Int,
    ghidTitle :: Text,
    ghidBody :: Maybe Text,
    ghidState :: Text,
    ghidLabels :: [Text],
    ghidAssignees :: [Text],
    ghidCreatedAt :: Text,
    ghidUpdatedAt :: Text
  }
  deriving (Show, Generic)

instance FromJSON GitHubIssueDetailResp where
  parseJSON = Aeson.withObject "GitHubIssueDetailResp" $ \v ->
    GitHubIssueDetailResp
      <$> v .: "number"
      <*> v .: "title"
      <*> v .:? "body"
      <*> v .: "state"
      <*> v .: "labels"
      <*> v .: "assignees"
      <*> v .: "created_at"
      <*> v .: "updated_at"

instance ToJSON GitHubIssueDetailResp where
  toJSON r =
    object
      [ "number" .= ghidNumber r,
        "title" .= ghidTitle r,
        "body" .= ghidBody r,
        "state" .= ghidState r,
        "labels" .= ghidLabels r,
        "assignees" .= ghidAssignees r,
        "created_at" .= ghidCreatedAt r,
        "updated_at" .= ghidUpdatedAt r
      ]

-- | Handle github_list_prs MCP tool call.
handleGitHubListPRs :: Value -> IO MCPCallOutput
handleGitHubListPRs args = do
  case parseEither parseListPRsArgs args of
    Left err -> pure $ errorResult $ "Invalid arguments: " <> pack err
    Right req -> do
      result <- callHost host_github_list_prs req
      case result of
        Left err -> pure $ errorResult $ pack err
        Right (resp :: GitHubPRsResp) -> pure $ successResult $ Aeson.toJSON resp

parseListPRsArgs :: Value -> Parser GitHubListPRsReq
parseListPRsArgs = Aeson.withObject "github_list_prs args" $ \v ->
  GitHubListPRsReq
    <$> v .: "owner"
    <*> v .: "repo"
    <*> v .:? "state"
    <*> v .:? "limit"

data GitHubListPRsReq = GitHubListPRsReq
  { ghlpOwner :: Text,
    ghlpRepo :: Text,
    ghlpState :: Maybe Text,
    ghlpLimit :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON GitHubListPRsReq where
  toJSON (GitHubListPRsReq o r s l) =
    object ["owner" .= o, "repo" .= r, "state" .= s, "limit" .= l]

data GitHubPRsResp = GitHubPRsResp { ghPRs :: [GitHubPR] }
  deriving (Show, Generic)

instance FromJSON GitHubPRsResp where
  parseJSON = Aeson.withObject "GitHubPRsResp" $ \v ->
    GitHubPRsResp <$> v .: "prs"

instance ToJSON GitHubPRsResp where
  toJSON (GitHubPRsResp prs) = object ["prs" .= prs]

data GitHubPR = GitHubPR
  { ghprNumber :: Int,
    ghprTitle :: Text,
    ghprState :: Text,
    ghprHead :: Text,
    ghprBase :: Text,
    ghprCreatedAt :: Text
  }
  deriving (Show, Generic)

instance FromJSON GitHubPR where
  parseJSON = Aeson.withObject "GitHubPR" $ \v ->
    GitHubPR
      <$> v .: "number"
      <*> v .: "title"
      <*> v .: "state"
      <*> v .: "head"
      <*> v .: "base"
      <*> v .: "created_at"

instance ToJSON GitHubPR where
  toJSON (GitHubPR n t s h b c) =
    object ["number" .= n, "title" .= t, "state" .= s, "head" .= h, "base" .= b, "created_at" .= c]

-- ============================================================================
-- Exported WASM functions
-- ============================================================================

foreign export ccall handle_mcp_call :: IO CInt

foreign export ccall handle_pre_tool_use :: IO CInt

foreign export ccall handle_list_tools :: IO CInt

handle_mcp_call :: IO CInt
handle_mcp_call = wrapHandler $ do
  inp <- input @ByteString
  case Aeson.eitherDecodeStrict inp of
    Left err -> do
      let resp = errorResult $ "Parse error: " <> pack err
      output (BSL.toStrict $ Aeson.encode resp)
      pure 1
    Right mcpCall -> do
      resp <- dispatchTool (toolName mcpCall) (toolArgs mcpCall)
      output (BSL.toStrict $ Aeson.encode resp)
      if success resp then pure 0 else pure 1

handle_pre_tool_use :: IO CInt
handle_pre_tool_use = wrapHandler $ do
  -- Parse input
  inp <- input @ByteString
  case Aeson.eitherDecodeStrict inp of
    Left err -> do
      let errResp = object ["error" .= ("Parse error: " ++ err)]
      output (BSL.toStrict $ Aeson.encode errResp)
      pure 1
    Right (_hookInput :: HookInput) -> do
      -- Run effects to gather context
      br <- runM $ runLog $ runGit $ do
        logInfo "Handling PreToolUse hook"
        getBranch

      -- For now, always allow with branch info in reason
      let reason = Just $ "Branch: " <> pack br
      let resp = allowResponse reason

      output (BSL.toStrict $ Aeson.encode resp)
      pure 0

-- | List all available MCP tools.
-- Returns JSON array of tool definitions.
handle_list_tools :: IO CInt
handle_list_tools = wrapHandler $ do
  let tools = map Tools.toMCPFormat Tools.allTools
  output (BSL.toStrict $ Aeson.encode tools)
  pure 0

wrapHandler :: IO CInt -> IO CInt
wrapHandler action = do
  res <- try @SomeException action
  case res of
    Right code -> pure code
    Left err -> do
      let errJson = Aeson.encode $ object ["error" .= show err]
      output (BSL.toStrict errJson)
      pure 1

main :: IO ()
main = pure ()
