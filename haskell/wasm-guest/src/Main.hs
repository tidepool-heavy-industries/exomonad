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
import Data.Aeson (FromJSON, ToJSON, Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text, pack)
import ExoMonad.Guest.HostCall
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
  { sessionId :: Text,
    hookEventName :: Text,
    toolName :: Maybe Text,
    toolInput :: Maybe Value
  }
  deriving (Show, Generic)

instance FromJSON HookInput where
  parseJSON = Aeson.withObject "HookInput" $ \v ->
    HookInput
      <$> v Aeson..: "session_id"
      <*> v Aeson..: "hook_event_name"
      <*> v Aeson..:? "tool_name"
      <*> v Aeson..:? "tool_input"

-- ============================================================================
-- Exported WASM functions
-- ============================================================================

foreign export ccall handle_mcp_call :: IO CInt

foreign export ccall handle_pre_tool_use :: IO CInt

handle_mcp_call :: IO CInt
handle_mcp_call = wrapHandler $ do
  -- Just echo for now (Phase 4 will implement real MCP dispatch)
  inp <- input @ByteString
  output inp
  pure 0

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
