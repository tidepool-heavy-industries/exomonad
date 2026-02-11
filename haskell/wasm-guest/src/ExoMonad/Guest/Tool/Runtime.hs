{-# LANGUAGE TypeApplications #-}

-- | Runtime handlers for WASM exports.
--
-- Separated from TH.hs because these use ExoMonad.PDK which cannot be
-- loaded by the TH external interpreter during cross-compilation.
module ExoMonad.Guest.Tool.Runtime
  ( mcpHandlerRecord,
    listHandlerRecord,
    hookHandler,
    handleWorkerExit,
    testHandler,
    wrapHandler,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (unless, void)
import Data.Aeson (FromJSON, ToJSON, Value, withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.Vector qualified as V
import Effects.Events qualified as ProtoEvents
import Effects.Log qualified as Log
import ExoMonad.Effect.Class (runEffect_)
import ExoMonad.Effects.Events qualified as Events
import ExoMonad.Effects.Log (LogInfo, LogError, LogEmitEvent)
import ExoMonad.Effects.Messaging (sendNote)
import ExoMonad.Guest.Tool.Class (MCPCallOutput (..), toMCPFormat)
import ExoMonad.Guest.Tool.Mode (AsHandler)
import ExoMonad.Guest.Tool.Record (DispatchRecord (..), ReifyRecord (..))
import ExoMonad.Guest.Types (HookEventType (..), HookInput (..), HookOutput, MCPCallInput (..), StopDecision (..), StopHookOutput (..), allowResponse)
import ExoMonad.Types (HookConfig (..))
import ExoMonad.PDK (input, output)
import Foreign.C.Types (CInt (..))
import GHC.Generics (Generic)
import Polysemy (Embed, Sem, runM)
import System.Directory (getCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeFileName)

-- | Helper for fire-and-forget logging via yield_effect.
logInfo_ :: Text -> IO ()
logInfo_ msg = void $ runEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = TL.fromStrict msg, Log.infoRequestFields = ""})

logError_ :: Text -> IO ()
logError_ msg = void $ runEffect_ @LogError (Log.ErrorRequest {Log.errorRequestMessage = TL.fromStrict msg, Log.errorRequestFields = ""})

emitEvent_ :: Value -> IO ()
emitEvent_ val = void $ runEffect_ @LogEmitEvent (Log.EmitEventRequest {Log.emitEventRequestEventType = "custom", Log.emitEventRequestPayload = BSL.toStrict (Aeson.encode val), Log.emitEventRequestTimestamp = 0})

-- | Test handler - allows calling any host function directly for property testing.
-- Note: testHandler is kept as a passthrough for backward compatibility.
-- In the new architecture, all effects go through yield_effect, so this
-- handler is primarily useful for integration testing the effect system.
testHandler :: IO CInt
testHandler = do
  inp <- input @ByteString
  case Aeson.eitherDecodeStrict inp of
    Left err -> do
      output (BSL.toStrict $ Aeson.encode $ TestResult @Value False Nothing (Just $ "JSON decode error: " ++ err))
      pure 1
    Right (TestCall _func _payload) -> do
      -- Test dispatch is no longer supported via direct host functions.
      -- Tests should use runEffect directly.
      output (BSL.toStrict $ Aeson.encode $ TestResult @Value False Nothing (Just $ "testHandler dispatch removed: use runEffect directly"))
      pure 1

-- | Input structure for testHandler
data TestCall = TestCall
  { functionName :: Text,
    args :: Value
  }
  deriving (Show, Generic)

instance FromJSON TestCall where
  parseJSON = withObject "TestCall" $ \v ->
    TestCall
      <$> v .: "function"
      <*> v .: "args"

-- | Output structure for testHandler
data TestResult a = TestResult
  { successTest :: Bool,
    resultTest :: Maybe a,
    errorTest :: Maybe String
  }
  deriving (Show, Generic)

instance (ToJSON a) => ToJSON (TestResult a) where
  toJSON (TestResult s r e) =
    Aeson.object
      [ "success" .= s,
        "result" .= r,
        "error" .= e
      ]

-- | MCP call handler - dispatches to tools based on a record.
mcpHandlerRecord :: forall tools. (DispatchRecord tools) => tools AsHandler -> IO CInt
mcpHandlerRecord handlers = do
  inp <- input @ByteString
  case Aeson.eitherDecodeStrict inp of
    Left err -> do
      logError_ ("MCP parse error: " <> T.pack err)
      let resp = MCPCallOutput False Nothing (Just $ "Parse error: " <> T.pack err)
      output (BSL.toStrict $ Aeson.encode resp)
      pure 0
    Right mcpCall -> do
      logInfo_ ("Dispatching tool: " <> toolName mcpCall)

      resp <- dispatchRecord handlers (toolName mcpCall) (toolArgs mcpCall)

      unless (success resp) $
        logError_ ("Tool failed: " <> fromMaybe "No error message" (mcpError resp))

      output (BSL.toStrict $ Aeson.encode resp)
      pure 0

-- | List tools handler - returns all tool definitions for a record type.
listHandlerRecord :: forall tools. (ReifyRecord tools) => IO CInt
listHandlerRecord = do
  let tools = map toMCPFormat (reifyToolDefs (Proxy @tools))
  output (BSL.toStrict $ Aeson.encode tools)
  pure 0

-- | Hook handler - handles PreToolUse, SessionEnd, and SubagentStop hooks.
--
-- Takes a HookConfig to determine hook behavior. The HookConfig comes from
-- user-defined Role.hs files, allowing roles to customize their hook logic.
--
-- For stop hooks (SessionEnd, SubagentStop), uses onStop/onSubagentStop from config.
-- For PreToolUse, uses preToolUse from config.
hookHandler :: HookConfig -> IO CInt
hookHandler config = do
  inp <- input @ByteString
  case Aeson.eitherDecodeStrict inp of
    Left err -> do
      logError_ ("Hook parse error: " <> T.pack err)
      let errResp = Aeson.object ["error" Aeson..= ("Parse error: " ++ err)]
      output (BSL.toStrict $ Aeson.encode errResp)
      pure 1
    Right hookInput -> do
      let hookType = hiHookEventName hookInput
      let hookName = case hookType of
            SessionEnd -> "SessionEnd"
            Stop -> "Stop"
            SubagentStop -> "SubagentStop"
            PreToolUse -> "PreToolUse"
            PostToolUse -> "PostToolUse"
            WorkerExit -> "WorkerExit"
      logInfo_ ("Hook received: " <> hookName)

      case hookType of
        SessionEnd -> handleStopHook hookInput (onStop config)
        Stop -> handleStopHook hookInput (onStop config)
        SubagentStop -> handleStopHook hookInput (onSubagentStop config)
        PreToolUse -> handlePreToolUse hookInput (preToolUse config)
        PostToolUse -> handlePreToolUse hookInput (postToolUse config)
        WorkerExit -> handleWorkerExit hookInput
  where
    handlePreToolUse :: HookInput -> (HookInput -> Sem '[Embed IO] HookOutput) -> IO CInt
    handlePreToolUse hookInput hook = do
      result <- runM $ hook hookInput
      output (BSL.toStrict $ Aeson.encode result)
      pure 0

    handleStopHook :: HookInput -> (HookInput -> Sem '[Embed IO] StopHookOutput) -> IO CInt
    handleStopHook hookInput hook = do
      -- Extract agent ID from current working directory (e.g., "gh-453-gemini")
      cwd <- getCurrentDirectory
      let agentId = T.pack $ takeFileName cwd

      -- Get current timestamp in ISO8601 format
      now <- getCurrentTime
      let timestamp = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" now

      -- Log that stop hook was called
      logInfo_ ("Stop hook firing for agent: " <> agentId)

      -- Emit AgentStopped event BEFORE running checks
      -- This ensures we see the event even if checks fail
      let event =
            Aeson.object
              [ "type" .= ("agent:stopped" :: Text),
                "agent_id" .= agentId,
                "timestamp" .= timestamp
              ]
      emitEvent_ event

      -- Run the hook from config (using Polysemy effects)
      result <- runM $ hook hookInput

      -- Log the decision
      case decision result of
        Allow ->
          logInfo_ ("Stop hook allowed for " <> agentId)
        Block ->
          case reason result of
            Just r ->
              logInfo_ ("Stop hook blocked for " <> agentId <> ": " <> r)
            Nothing ->
              logInfo_ ("Stop hook blocked for " <> agentId)

      -- Return the result to the hook caller
      output (BSL.toStrict $ Aeson.encode result)
      pure 0

handleWorkerExit :: HookInput -> IO CInt
handleWorkerExit _ = do
  logInfo_ "WorkerExit hook firing"
  maybeAgentId <- lookupEnv "EXOMONAD_AGENT_ID"
  maybeSessionId <- lookupEnv "EXOMONAD_SESSION_ID"

  case (maybeAgentId, maybeSessionId) of
    (Just agentIdStr, Just sessionIdStr) -> do
        let agentId = T.pack agentIdStr
        let sessionId = T.pack sessionIdStr
        logInfo_ $ "Handling exit for agent: " <> agentId <> " session: " <> sessionId
        
        -- Create the completion event
        let event = ProtoEvents.Event
              { ProtoEvents.eventEventType = Just $ ProtoEvents.EventEventTypeWorkerComplete $ ProtoEvents.WorkerComplete
                  { ProtoEvents.workerCompleteWorkerId = TL.fromStrict agentId
                  , ProtoEvents.workerCompleteStatus = "success" -- TODO: differentiate success/error
                  , ProtoEvents.workerCompleteChanges = V.empty -- TODO: capture changes
                  , ProtoEvents.workerCompleteMessage = "Worker completed"
                  }
              }
              
        -- Send event
        res <- Events.notifyEvent sessionId event
        case res of
            Left err -> logError_ ("Failed to notify completion: " <> T.pack (show err))
            Right _ -> logInfo_ "Completion notified"
            
    _ -> do
        logError_ "EXOMONAD_AGENT_ID or EXOMONAD_SESSION_ID not set in worker-exit hook"
        pure ()

  output (BSL.toStrict $ Aeson.encode $ allowResponse Nothing)
  pure 0

-- | Wrap a handler with exception handling.
wrapHandler :: IO CInt -> IO CInt
wrapHandler action = do
  res <- try @SomeException action
  case res of
    Right code -> pure code
    Left err -> do
      -- Return proper MCPCallOutput format
      let resp =
            MCPCallOutput
              { success = False,
                result = Nothing,
                mcpError = Just $ T.pack ("Exception in WASM handler: " <> show err)
              }
      output (BSL.toStrict $ Aeson.encode resp)
      pure 0
