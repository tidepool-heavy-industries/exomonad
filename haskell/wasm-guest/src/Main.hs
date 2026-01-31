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

-- | Handle git_status MCP tool call (placeholder).
handleGitStatus :: Value -> IO MCPCallOutput
handleGitStatus _args = do
  -- TODO: Implement proper git status via host function
  pure $ successResult $ object ["status" .= ("clean" :: Text)]

-- ============================================================================
-- Exported WASM functions
-- ============================================================================

foreign export ccall handle_mcp_call :: IO CInt

foreign export ccall handle_pre_tool_use :: IO CInt

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
    Right (hookInput :: HookInput) -> do
      -- Run effects to gather context
      br <- runM $ runLog $ runGit $ do
        logInfo "Handling PreToolUse hook"
        getBranch

      -- For now, always allow with branch info in reason
      let reason = Just $ "Branch: " <> pack br
      let resp = allowResponse reason

      output (BSL.toStrict $ Aeson.encode resp)
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
