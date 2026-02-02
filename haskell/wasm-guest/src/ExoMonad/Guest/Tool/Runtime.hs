-- | Runtime handlers for WASM exports.
--
-- Separated from TH.hs because these use Extism.PDK which cannot be
-- loaded by the TH external interpreter during cross-compilation.
module ExoMonad.Guest.Tool.Runtime
  ( mcpHandlerRecord,
    listHandlerRecord,
    hookHandler,
    wrapHandler,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Text (Text)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import ExoMonad.Guest.Effects.StopHook (runStopHookChecks)
import ExoMonad.Guest.HostCall (LogLevel (..), LogPayload (..), callHostVoid, host_log_error, host_log_info, host_emit_event)
import ExoMonad.Guest.Tool.Class (MCPCallOutput (..), toMCPFormat)
import ExoMonad.Guest.Tool.Mode (AsHandler)
import ExoMonad.Guest.Tool.Record (DispatchRecord (..), ReifyRecord (..))
import ExoMonad.Guest.Types (HookInput (..), MCPCallInput (..), StopHookOutput (..), StopDecision (..), allowResponse)
import Extism.PDK (input, output)
import Foreign.C.Types (CInt (..))
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName)

-- | MCP call handler - dispatches to tools based on a record.
mcpHandlerRecord :: forall tools. (DispatchRecord tools) => tools AsHandler -> IO CInt
mcpHandlerRecord handlers = do
  inp <- input @ByteString
  case Aeson.eitherDecodeStrict inp of
    Left err -> do
      callHostVoid host_log_error (LogPayload Error ("MCP parse error: " <> T.pack err) Nothing)
      let resp = MCPCallOutput False Nothing (Just $ "Parse error: " <> T.pack err)
      output (BSL.toStrict $ Aeson.encode resp)
      pure 1
    Right mcpCall -> do
      callHostVoid host_log_info (LogPayload Info ("Dispatching tool: " <> toolName mcpCall) Nothing)

      resp <- dispatchRecord handlers (toolName mcpCall) (toolArgs mcpCall)

      unless (success resp) $
        callHostVoid host_log_error (LogPayload Error ("Tool failed: " <> fromMaybe "No error message" (mcpError resp)) Nothing)

      output (BSL.toStrict $ Aeson.encode resp)
      if success resp then pure 0 else pure 1

-- | List tools handler - returns all tool definitions for a record type.
listHandlerRecord :: forall tools. (ReifyRecord tools) => IO CInt
listHandlerRecord = do
  let tools = map toMCPFormat (reifyToolDefs (Proxy @tools))
  output (BSL.toStrict $ Aeson.encode tools)
  pure 0

-- | Hook handler - handles PreToolUse, SessionEnd, and SubagentStop hooks.
--
-- For stop hooks (SessionEnd, SubagentStop), runs the stop hook checks
-- that verify uncommitted changes, unpushed commits, PR status, etc.
hookHandler :: IO CInt
hookHandler = do
  inp <- input @ByteString
  case Aeson.eitherDecodeStrict inp of
    Left err -> do
      callHostVoid host_log_error (LogPayload Error ("Hook parse error: " <> T.pack err) Nothing)
      let errResp = Aeson.object ["error" Aeson..= ("Parse error: " ++ err)]
      output (BSL.toStrict $ Aeson.encode errResp)
      pure 1
    Right hookInput -> do
      let hookName = hiHookEventName hookInput
      callHostVoid host_log_info (LogPayload Info ("Hook received: " <> hookName) Nothing)

      case hookName of
        "SessionEnd" -> handleStopHook hookName
        "SubagentStop" -> handleStopHook hookName
        _ -> do
          callHostVoid host_log_info (LogPayload Info ("Allowing hook: " <> hookName) Nothing)
          let resp = allowResponse Nothing
          output (BSL.toStrict $ Aeson.encode resp)
          pure 0
  where
    handleStopHook :: Text -> IO CInt
    handleStopHook hookName = do
      -- Extract agent ID from current working directory (e.g., "gh-453-gemini")
      cwd <- getCurrentDirectory
      let agentId = T.pack $ takeFileName cwd

      -- Get current timestamp in ISO8601 format
      now <- getCurrentTime
      let timestamp = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" now

      -- Log that stop hook was called
      callHostVoid host_log_info (LogPayload Info ("Stop hook firing for agent: " <> agentId) Nothing)

      -- Emit AgentStopped event BEFORE running checks
      -- This ensures we see the event even if checks fail
      let event = Aeson.object
            [ "type" .= ("agent:stopped" :: Text)
            , "agent_id" .= agentId
            , "timestamp" .= timestamp
            ]
      callHostVoid host_emit_event event

      -- Run stop hook validation checks
      result <- runStopHookChecks

      -- Log the decision
      case decision result of
        Allow ->
          callHostVoid host_log_info (LogPayload Info ("Stop hook allowed for " <> agentId) Nothing)
        Block ->
          case reason result of
            Just r ->
              callHostVoid host_log_info (LogPayload Info ("Stop hook blocked for " <> agentId <> ": " <> r) Nothing)
            Nothing ->
              callHostVoid host_log_info (LogPayload Info ("Stop hook blocked for " <> agentId) Nothing)

      -- Return the result to the hook caller
      output (BSL.toStrict $ Aeson.encode result)
      pure 0

-- | Wrap a handler with exception handling.
wrapHandler :: IO CInt -> IO CInt
wrapHandler action = do
  res <- try @SomeException action
  case res of
    Right code -> pure code
    Left err -> do
      -- Return proper MCPCallOutput format
      let resp = MCPCallOutput
            { success = False
            , result = Nothing
            , mcpError = Just $ T.pack ("Exception in WASM handler: " <> show err)
            }
      output (BSL.toStrict $ Aeson.encode resp)
      pure 1
