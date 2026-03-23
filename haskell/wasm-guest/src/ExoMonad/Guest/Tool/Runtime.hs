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
    resumeHandler,
    wrapHandler,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (unless, void)
import Control.Monad.Freer (Eff, runM)
import Control.Monad.Freer.Coroutine (runC)
import Data.Aeson (ToJSON, Value, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseMaybe)
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
import ExoMonad.Effects.Events qualified as Events
import ExoMonad.Effects.Log (LogEmitEvent, LogError, LogInfo)
import ExoMonad.Guest.Continuations (retrieveContinuation)
import ExoMonad.Guest.Effects.AgentControl (runAgentControlSuspend)
import ExoMonad.Guest.Effects.FileSystem (runFileSystemSuspend)
import ExoMonad.Guest.Proto (fromText)
import ExoMonad.Guest.Tool.Class (MCPCallOutput (..), WasmResult (..), toMCPFormat)
import ExoMonad.Guest.Tool.Mode (AsHandler)
import ExoMonad.Guest.Tool.Record (DispatchRecord (..), ReifyRecord (..))
import ExoMonad.Guest.Tool.Suspend (statusToWasmResult)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect, suspendEffect_)
import ExoMonad.Guest.Types (HookEventType (..), HookInput (..), HookOutput, MCPCallInput (..), Runtime (..), StopDecision (..), StopHookOutput (..), allowResponse)
import ExoMonad.PDK (input, output)
import ExoMonad.Types (HookConfig (..), Effects)
import Foreign.C.Types (CInt (..))
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName)

-- | Run an effectful hook, converting to WasmResult.
runHookEff :: (ToJSON a) => Eff Effects a -> IO (WasmResult a)
runHookEff eff = do
  status <- runM (runC (runFileSystemSuspend (runAgentControlSuspend eff)))
  statusToWasmResult status

-- | Helper for fire-and-forget logging via yield_effect.
logInfo_ :: Text -> IO (WasmResult ())
logInfo_ msg = runHookEff $ void $ suspendEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = fromText msg, Log.infoRequestFields = ""})

logError_ :: Text -> IO (WasmResult ())
logError_ msg = runHookEff $ void $ suspendEffect_ @LogError (Log.ErrorRequest {Log.errorRequestMessage = fromText msg, Log.errorRequestFields = ""})

emitEvent_ :: Value -> IO (WasmResult ())
emitEvent_ val = runHookEff $ void $ suspendEffect_ @LogEmitEvent (Log.EmitEventRequest {Log.emitEventRequestEventType = "custom", Log.emitEventRequestPayload = BSL.toStrict (Aeson.encode val), Log.emitEventRequestTimestamp = 0})

-- | Sequential logging helper that handles suspension.
andThenLog :: IO (WasmResult ()) -> IO (WasmResult a) -> IO (WasmResult a)
andThenLog first second = do
  res <- first
  case res of
    Done () -> second
    Suspend k req -> pure $ Suspend k req

-- | Sequential logging helper that returns a Value.
andThenLogValue :: IO (WasmResult ()) -> IO Value -> IO Value
andThenLogValue first second = do
  res <- first
  case res of
    Done () -> second
    Suspend k req -> pure $ Aeson.toJSON (Suspend @Value k req)

-- | MCP call handler - dispatches to tools based on a record.
mcpHandlerRecord :: forall tools. (DispatchRecord tools) => tools AsHandler -> IO CInt
mcpHandlerRecord handlers = do
  inp <- input @ByteString
  case Aeson.eitherDecodeStrict inp of
    Left err -> do
      res <- logError_ ("MCP parse error: " <> T.pack err)
      case res of
        Done () -> do
          let resp = Done $ MCPCallOutput False Nothing (Just $ "Parse error: " <> T.pack err)
          output (BSL.toStrict $ Aeson.encode resp)
        Suspend k req -> output (BSL.toStrict $ Aeson.encode (Suspend @MCPCallOutput k req))
      pure 0
    Right mcpCall -> do
      result <- logInfo_ ("Dispatching tool: " <> toolName mcpCall) `andThenLog` do
        dispatchRecord handlers (toolName mcpCall) (toolArgs mcpCall)

      finalResult <- case result of
        Done resp -> do
          if success resp
            then pure (Done resp)
            else logError_ ("Tool failed: " <> fromMaybe "No error message" (mcpError resp)) `andThenLog` pure (Done resp)
        Suspend k req -> pure (Suspend k req)

      output (BSL.toStrict $ Aeson.encode finalResult)
      pure 0

-- | Resume handler - resumes a suspended computation.
resumeHandler :: IO CInt
resumeHandler = do
  inp <- input @ByteString
  case Aeson.eitherDecodeStrict inp of
    Left err -> do
      res <- logError_ ("Resume parse error: " <> T.pack err)
      case res of
        Done () -> do
          let resp = Done $ MCPCallOutput False Nothing (Just $ "Resume parse error: " <> T.pack err)
          output (BSL.toStrict $ Aeson.encode resp)
        Suspend k req -> output (BSL.toStrict $ Aeson.encode (Suspend @MCPCallOutput k req))
      pure 0
    Right val -> do
      case parseResumeInput val of
        Left err -> do
          res_ <- logError_ ("Resume input error: " <> err)
          case res_ of
            Done () -> do
              let resp = Done $ MCPCallOutput False Nothing (Just $ "Resume input error: " <> err)
              output (BSL.toStrict $ Aeson.encode resp)
            Suspend k req -> output (BSL.toStrict $ Aeson.encode (Suspend @MCPCallOutput k req))
          pure 0
        Right (k, res) -> do
          mCont <- retrieveContinuation k
          case mCont of
            Nothing -> do
              r <- logError_ ("Continuation not found: " <> k)
              case r of
                Done () -> do
                  let resp = Done $ MCPCallOutput False Nothing (Just $ "Continuation not found: " <> k)
                  output (BSL.toStrict $ Aeson.encode resp)
                Suspend k_ req -> output (BSL.toStrict $ Aeson.encode (Suspend @MCPCallOutput k_ req))
              pure 0
            Just cont -> do
              result <- cont res
              output (BSL.toStrict $ Aeson.encode result)
              pure 0

parseResumeInput :: Value -> Either Text (Text, Value)
parseResumeInput val = flip (maybe (Left "Invalid input format")) (parseMaybe parser val) Right
  where
    parser = Aeson.withObject "ResumeInput" $ \o -> do
      k <- o Aeson..: "continuation_id"
      res <- o Aeson..: "result"
      pure (k, res)

-- | List tools handler - returns all tool definitions for a record type.
listHandlerRecord :: forall tools. (ReifyRecord tools) => IO CInt
listHandlerRecord = do
  let tools = map toMCPFormat (reifyToolDefs (Proxy @tools))
  output (BSL.toStrict $ Aeson.encode (Done tools))
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
      res <- logError_ ("Hook parse error: " <> T.pack err)
      case res of
        Done () -> do
          let errResp = Aeson.object ["error" .= ("Parse error: " <> T.pack err)]
          output (BSL.toStrict $ Aeson.encode (Done errResp))
        Suspend k req -> output (BSL.toStrict $ Aeson.encode (Suspend @Value k req))
      pure 1
    Right hookInput -> do
      let hookType = hiHookEventName hookInput
      let hookName = case hookType of
            SessionStart -> "SessionStart" :: Text
            SessionEnd -> "SessionEnd"
            Stop -> "Stop"
            SubagentStop -> "SubagentStop"
            PreToolUse -> "PreToolUse"
            PostToolUse -> "PostToolUse"
            WorkerExit -> "WorkerExit"
      
      result <- logInfo_ ("Hook received: " <> hookName) `andThenLogValue` do
        case hookType of
          SessionStart -> handleSessionStart hookInput (onSessionStart config)
          SessionEnd -> handleStopHook hookInput (onStop config)
          Stop -> handleStopHook hookInput (onStop config)
          SubagentStop -> handleStopHook hookInput (onSubagentStop config)
          PreToolUse -> handlePreToolUse hookInput (preToolUse config)
          PostToolUse -> handlePreToolUse hookInput (postToolUse config)
          WorkerExit -> handleWorkerExit hookInput

      output (BSL.toStrict $ Aeson.encode result)
      pure 0
  where
    handleSessionStart :: HookInput -> (HookInput -> Eff Effects HookOutput) -> IO Value
    handleSessionStart hookInput hook = Aeson.toJSON <$> runHookEff (hook hookInput)

    handlePreToolUse :: HookInput -> (HookInput -> Eff Effects HookOutput) -> IO Value
    handlePreToolUse hookInput hook = Aeson.toJSON <$> runHookEff (hook hookInput)

    handleStopHook :: HookInput -> (HookInput -> Eff Effects StopHookOutput) -> IO Value
    handleStopHook hookInput hook = do
      -- Extract agent ID from hook input or fallback to current working directory (e.g., "gh-453-gemini")
      cwd <- getCurrentDirectory
      let agentId = fromMaybe (T.pack $ takeFileName cwd) (hiAgentId hookInput)

      -- Get current timestamp in ISO8601 format
      now <- getCurrentTime
      let timestamp = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" now

      -- Log that stop hook was called
      logInfo_ ("Stop hook firing for agent: " <> agentId) `andThenLogValue` do
        -- Emit AgentStopped event BEFORE running checks
        -- This ensures we see the event even if checks fail
        let event =
              Aeson.object
                [ "type" .= ("agent:stopped" :: Text),
                  "agent_id" .= agentId,
                  "timestamp" .= timestamp
                ]
        emitEvent_ event `andThenLogValue` do
          -- Run the hook from config (using Freer effects)
          result <- runHookEff (hook hookInput)

          -- Check if runtime is Gemini and override Block to Allow
          -- Fix for infinite loop: Gemini agents retry forever on block
          let isGemini = hiRuntime hookInput == Just Gemini
          let finalResult = case result of
                Done rawResult ->
                  if isGemini && decision rawResult == Block
                    then Done (rawResult {decision = Allow, reason = Nothing})
                    else Done rawResult
                Suspend k req -> Suspend k req

          -- Log the decision
          case finalResult of
            Done res -> do
              let logMsg = case decision res of
                    Allow -> "Stop hook allowed for " <> agentId
                    Block ->
                      case reason res of
                        Just r -> "Stop hook blocked for " <> agentId <> ": " <> r
                        Nothing -> "Stop hook blocked for " <> agentId
              logInfo_ logMsg `andThenLogValue` pure (Aeson.toJSON finalResult)
            Suspend k req ->
              logInfo_ ("Stop hook suspended for " <> agentId) `andThenLogValue` pure (Aeson.toJSON finalResult)

handleWorkerExit :: HookInput -> IO Value
handleWorkerExit hookInput = do
  logInfo_ "WorkerExit hook firing" `andThenLogValue` do
    res_ <- runHookEff $ do
      let maybeAgentId = hiAgentId hookInput
      case maybeAgentId of
        Just agentId -> do
          let actualStatus = fromMaybe "success" (hiExitStatus hookInput)
          let (status, statusMsg) = case actualStatus of
                "success" -> ("success", agentId <> " is idle")
                other -> ("failure", "Worker " <> agentId <> " exited with status: " <> other)
          res <- suspendEffect @Events.EventsNotifyParent
                  (ProtoEvents.NotifyParentRequest
                    { ProtoEvents.notifyParentRequestAgentId = TL.fromStrict agentId,
                      ProtoEvents.notifyParentRequestStatus = TL.fromStrict status,
                      ProtoEvents.notifyParentRequestMessage = TL.fromStrict statusMsg,
                      ProtoEvents.notifyParentRequestOverrideRecipient = Nothing
                    })
          case res of
            Left err -> void $ suspendEffect_ @LogError (Log.ErrorRequest { Log.errorRequestMessage = TL.fromStrict ("Failed to notify parent: " <> T.pack (show err)), Log.errorRequestFields = "" })
            Right _ -> void $ suspendEffect_ @LogInfo (Log.InfoRequest { Log.infoRequestMessage = TL.fromStrict ("Exit notified for " <> agentId <> " (" <> status <> ")"), Log.infoRequestFields = "" })
        Nothing -> do
          void $ suspendEffect_ @LogError (Log.ErrorRequest { Log.errorRequestMessage = "agent_id missing from hook input", Log.errorRequestFields = "" })
      pure (allowResponse Nothing)
    pure (Aeson.toJSON res_)

-- | Wrap a handler with exception handling.
wrapHandler :: IO CInt -> IO CInt
wrapHandler action = do
  res <- try @SomeException action
  case res of
    Right code -> pure code
    Left err -> do
      -- Return proper MCPCallOutput format
      let resp =
            Done $
              MCPCallOutput
                { success = False,
                  result = Nothing,
                  mcpError = Just $ T.pack ("Exception in WASM handler: " <> show err)
                }
      output (BSL.toStrict $ Aeson.encode resp)
      pure 0
