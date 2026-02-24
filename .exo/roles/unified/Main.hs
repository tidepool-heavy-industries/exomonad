{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Unified WASM entry point for all roles.
--
-- Unlike the per-role Main.hs files, this reads the role from the input JSON
-- and dispatches to the correct role's config. The Rust server passes the role
-- via the "role" field in the JSON payload.
--
-- IMPORTANT: The extism PDK's `input` can only be called once per invocation.
-- All handlers must extract the role AND their specific payload from a single
-- parse of the input bytes.
module Main where

import AllRoles (lookupRole, roleDispatch, roleHooks, roleToolsMCP)
import Data.Aeson (Value, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.Tool.Class (MCPCallOutput (..), WasmResult (..))
import ExoMonad.Guest.Tool.Runtime (wrapHandler, resumeHandler, handleWorkerExit)
import ExoMonad.Guest.Tool.Suspend (statusToWasmResult)
import ExoMonad.Guest.Types (HookInput (..), HookEventType (..))
import ExoMonad.PDK (input, output)
import ExoMonad.Types (HookConfig (..), HookEffects)
import Foreign.C.Types (CInt (..))
import Control.Monad.Freer (runM)
import Control.Monad.Freer.Coroutine (runC)

foreign export ccall handle_mcp_call :: IO CInt
foreign export ccall handle_list_tools :: IO CInt
foreign export ccall handle_pre_tool_use :: IO CInt
foreign export ccall resume :: IO CInt

-- | Input for role-aware MCP calls: { "role": "dev", "toolName": "...", "toolArgs": {...} }
data RoleAwareMCPInput = RoleAwareMCPInput
  { ramRole :: Text,
    ramToolName :: Maybe Text,
    ramToolArgs :: Maybe Value
  }

instance Aeson.FromJSON RoleAwareMCPInput where
  parseJSON = Aeson.withObject "RoleAwareMCPInput" $ \v ->
    RoleAwareMCPInput
      <$> v .: "role"
      <*> v .:? "toolName"
      <*> v .:? "toolArgs"

-- | Input for role-aware list calls: { "role": "dev" }
newtype RoleAwareListInput = RoleAwareListInput
  { ralRole :: Text
  }

instance Aeson.FromJSON RoleAwareListInput where
  parseJSON = Aeson.withObject "RoleAwareListInput" $ \v ->
    RoleAwareListInput
      <$> v .: "role"

-- | Input for role-aware hook calls: { "role": "dev", ...hook fields... }
-- The role is extracted, then the full object is re-parsed as HookInput.
data RoleAwareHookInput = RoleAwareHookInput
  { rahRole :: Text,
    rahRawValue :: Value
  }

instance Aeson.FromJSON RoleAwareHookInput where
  parseJSON val = Aeson.withObject "RoleAwareHookInput" (\v -> do
    role <- v .: "role"
    pure $ RoleAwareHookInput role val) val

-- | Dispatch an MCP tool call to the correct role's handler.
handle_mcp_call :: IO CInt
handle_mcp_call = wrapHandler $ do
  inp <- input @ByteString
  case Aeson.eitherDecodeStrict inp of
    Left err -> do
      outputError $ "Parse error: " <> T.pack err
      pure 0
    Right rai -> case lookupRole (ramRole rai) of
      Nothing -> do
        outputError $ "Unknown role: " <> ramRole rai
        pure 0
      Just roleCfg -> do
        let name = maybe "" id (ramToolName rai)
            args = maybe Aeson.Null id (ramToolArgs rai)
        resp <- roleDispatch roleCfg name args
        output (BSL.toStrict $ Aeson.encode resp)
        pure 0

-- | List tools for a given role. Reads role from input JSON.
handle_list_tools :: IO CInt
handle_list_tools = wrapHandler $ do
  inp <- input @ByteString
  case Aeson.eitherDecodeStrict inp of
    Left _ -> do
      output (BSL.toStrict $ Aeson.encode ([] :: [Value]))
      pure 0
    Right rai -> case lookupRole (ralRole rai) of
      Nothing -> do
        output (BSL.toStrict $ Aeson.encode ([] :: [Value]))
        pure 0
      Just roleCfg -> do
        let toolDefs = roleToolsMCP roleCfg
        output (BSL.toStrict $ Aeson.encode toolDefs)
        pure 0

-- | Handle a hook call (PreToolUse, SessionEnd, SubagentStop) for a given role.
--
-- Parses the input once to extract both the role and the hook payload.
-- Inlines the dispatch logic from Runtime.hookHandler to avoid the
-- double-read problem with the extism PDK's `input`.
handle_pre_tool_use :: IO CInt
handle_pre_tool_use = wrapHandler $ do
  inp <- input @ByteString
  case Aeson.eitherDecodeStrict inp of
    Left err -> do
      outputError $ "Hook parse error: " <> T.pack err
      pure 1
    Right rahi -> case lookupRole (rahRole rahi) of
      Nothing -> do
        outputError $ "Unknown role: " <> rahRole rahi
        pure 1
      Just roleCfg ->
        case Aeson.fromJSON (rahRawValue rahi) of
          Aeson.Error err -> do
            outputError $ "Hook input parse error: " <> T.pack err
            pure 1
          Aeson.Success hookInput ->
            dispatchHook (roleHooks roleCfg) hookInput

-- | Dispatch a pre-parsed HookInput to the correct hook handler.
--
-- This is an inlined version of Runtime.hookHandler that operates on
-- already-parsed input, avoiding the double-read of extism PDK `input`.
dispatchHook :: HookConfig -> HookInput -> IO CInt
dispatchHook cfg hookInput =
  case hiHookEventName hookInput of
    SessionStart -> do
      status <- runM $ runC (onSessionStart cfg hookInput)
      result <- statusToWasmResult status
      output (BSL.toStrict $ Aeson.encode result)
      pure 0
    SessionEnd -> runStopHook (onStop cfg)
    Stop -> runStopHook (onStop cfg)
    SubagentStop -> runStopHook (onSubagentStop cfg)
    PreToolUse -> do
      status <- runM $ runC (preToolUse cfg hookInput)
      result <- statusToWasmResult status
      output (BSL.toStrict $ Aeson.encode result)
      pure 0
    PostToolUse -> do
      status <- runM $ runC (postToolUse cfg hookInput)
      result <- statusToWasmResult status
      output (BSL.toStrict $ Aeson.encode result)
      pure 0
    WorkerExit -> handleWorkerExit hookInput
  where
    runStopHook hook = do
      status <- runM $ runC (hook hookInput)
      result <- statusToWasmResult status
      output (BSL.toStrict $ Aeson.encode result)
      pure 0

-- | Output an error in WasmResult MCPCallOutput format.
outputError :: Text -> IO ()
outputError msg = output (BSL.toStrict $ Aeson.encode $ Done $ MCPCallOutput False Nothing (Just msg))

-- | Resume a suspended continuation.
resume :: IO CInt
resume = wrapHandler resumeHandler

main :: IO ()
main = pure ()
