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
import Data.Aeson (Value, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.Vector qualified as V
import Effects.Events qualified as ProtoEvents
import Effects.Log qualified as Log
import ExoMonad.Effect.Class (runEffect_)
import ExoMonad.Effects.Events qualified as Events
import ExoMonad.Effects.Log (LogInfo, LogError, LogEmitEvent)
import ExoMonad.Effects.Messaging (sendNote)
import ExoMonad.Guest.Proto (fromText)
import ExoMonad.Guest.Tool.Class (MCPCallOutput (..), toMCPFormat, WasmResult (..))
import ExoMonad.Guest.Tool.Mode (AsHandler)
import ExoMonad.Guest.Tool.Record (DispatchRecord (..), ReifyRecord (..))
import ExoMonad.Guest.Types (HookEventType (..), HookInput (..), HookOutput, MCPCallInput (..), StopDecision (..), StopHookOutput (..), allowResponse)
import ExoMonad.Guest.Continuations (retrieveContinuation)
import ExoMonad.Types (HookConfig (..))
import ExoMonad.PDK (input, output)
import Foreign.C.Types (CInt (..))
import Control.Monad.Freer (Eff, runM)
import Data.Aeson.Types (parseMaybe)
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName)

-- | Helper for fire-and-forget logging via yield_effect.
logInfo_ :: Text -> IO ()
logInfo_ msg = void $ runEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = fromText msg, Log.infoRequestFields = ""})

logError_ :: Text -> IO ()
logError_ msg = void $ runEffect_ @LogError (Log.ErrorRequest {Log.errorRequestMessage = fromText msg, Log.errorRequestFields = ""})

emitEvent_ :: Value -> IO ()
emitEvent_ val = void $ runEffect_ @LogEmitEvent (Log.EmitEventRequest {Log.emitEventRequestEventType = "custom", Log.emitEventRequestPayload = BSL.toStrict (Aeson.encode val), Log.emitEventRequestTimestamp = 0})

-- | MCP call handler - dispatches to tools based on a record.
mcpHandlerRecord :: forall tools. (DispatchRecord tools) => tools AsHandler -> IO CInt
mcpHandlerRecord handlers = do
  inp <- input @ByteString
  case Aeson.eitherDecodeStrict inp of
    Left err -> do
      logError_ ("MCP parse error: " <> T.pack err)
      let resp = Done $ MCPCallOutput False Nothing (Just $ "Parse error: " <> T.pack err)
      output (BSL.toStrict $ Aeson.encode resp)
      pure 0
    Right mcpCall -> do
      logInfo_ ("Dispatching tool: " <> toolName mcpCall)

      result <- dispatchRecord handlers (toolName mcpCall) (toolArgs mcpCall)

      case result of
        Done resp ->
          unless (success resp) $
            logError_ ("Tool failed: " <> fromMaybe "No error message" (mcpError resp))
        Suspend _ _ ->
          logInfo_ ("Tool suspended: " <> toolName mcpCall)

      output (BSL.toStrict $ Aeson.encode result)
      pure 0

-- | Resume handler - resumes a suspended computation.
resumeHandler :: IO CInt
resumeHandler = do
  inp <- input @ByteString
  case Aeson.eitherDecodeStrict inp of
    Left err -> do
      logError_ ("Resume parse error: " <> T.pack err)
      let resp = Done $ MCPCallOutput False Nothing (Just $ "Resume parse error: " <> T.pack err)
      output (BSL.toStrict $ Aeson.encode resp)
      pure 0
    Right val -> do
       case parseResumeInput val of
         Left err -> do
           logError_ ("Resume input error: " <> err)
           let resp = Done $ MCPCallOutput False Nothing (Just $ "Resume input error: " <> err)
           output (BSL.toStrict $ Aeson.encode resp)
           pure 0
         Right (k, res) -> do
           mCont <- retrieveContinuation k
           case mCont of
             Nothing -> do
               logError_ ("Continuation not found: " <> k)
               let resp = Done $ MCPCallOutput False Nothing (Just $ "Continuation not found: " <> k)
               output (BSL.toStrict $ Aeson.encode resp)
               pure 0
             Just cont -> do
               logInfo_ ("Resuming continuation: " <> k)
               -- Execute the continuation
               -- Note: The continuation is responsible for handling exceptions if needed
               newResult <- cont res
               output (BSL.toStrict $ Aeson.encode newResult)
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
    handlePreToolUse :: HookInput -> (HookInput -> Eff '[IO] HookOutput) -> IO CInt
    handlePreToolUse hookInput hook = do
      result <- runM $ hook hookInput
      output (BSL.toStrict $ Aeson.encode result)
      pure 0

    handleStopHook :: HookInput -> (HookInput -> Eff '[IO] StopHookOutput) -> IO CInt
    handleStopHook hookInput hook = do
      -- Extract agent ID from hook input or fallback to current working directory (e.g., "gh-453-gemini")
      cwd <- getCurrentDirectory
      let agentId = fromMaybe (T.pack $ takeFileName cwd) (hiAgentId hookInput)

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

      -- Run the hook from config (using Freer effects)
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
handleWorkerExit hookInput = do
  logInfo_ "WorkerExit hook firing"
  let maybeAgentId = hiAgentId hookInput

  case maybeAgentId of
    Just agentId -> do
        logInfo_ $ "Handling exit for agent: " <> agentId

        let actualStatus = fromMaybe "success" (hiExitStatus hookInput)
        let statusMsg = case actualStatus of
              "success" -> "Worker " <> agentId <> " completed successfully"
              other     -> "Worker " <> agentId <> " exited with status: " <> other

        res <- try @SomeException (Events.notifyParent actualStatus statusMsg)
        case res of
            Left exc -> logError_ ("notifyParent threw exception: " <> T.pack (show exc))
            Right (Left err) -> logError_ ("Failed to notify completion to parent: " <> T.pack (show err))
            Right (Right _) -> logInfo_ ("Completion notified for " <> agentId)

    Nothing -> do
        logError_ "agent_id missing from hook input"
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
      let resp = Done $
            MCPCallOutput
              { success = False,
                result = Nothing,
                mcpError = Just $ T.pack ("Exception in WASM handler: " <> show err)
              }
      output (BSL.toStrict $ Aeson.encode resp)
      pure 0
