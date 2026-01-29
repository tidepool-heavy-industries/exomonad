{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Hook event handler.
--
-- Handles hook events from Claude Code via exomonad.
-- Hooks are passthrough, but some execute effect logic:
--
-- * SessionStart: Injects issue context when on a gh-* branch
-- * Stop: Enforces PR filing with templated guidance + auto-focus on subagent error
module ExoMonad.Control.Handler.Hook
  ( handleHook
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import System.Environment (lookupEnv)
import Control.Exception (SomeException, try, throwIO)
import Control.Monad (forM_)
import Control.Monad.Freer (Eff, Member, runM)
import Control.Monad.Freer.State (runState)
import Data.Time.Clock (getCurrentTime)
import Data.Aeson (Value(..), decode, (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import OpenTelemetry.Trace
import qualified OpenTelemetry.Context.ThreadLocal as Context

import ExoMonad.Control.Logging (Logger)
import ExoMonad.Control.Protocol
import ExoMonad.Control.Types (ServerConfig(..))
import ExoMonad.Control.OpenObserve (shipTranscript)
import ExoMonad.Control.Hook.Policy (HookDecision(..), evaluatePolicy)
import ExoMonad.Control.Hook.CircuitBreaker (CircuitBreakerMap, SessionId, withCircuitBreaker, incrementStage)
import ExoMonad.Control.ExoTools (parseIssueNumber)
import ExoMonad.Control.Hook.SessionStart (sessionStartLogic)
import ExoMonad.Control.Effects.SshExec (runSshExec)
import ExoMonad.Control.Effects.Git (runGitRemote)
import ExoMonad.Control.Effects.Cabal (runCabalRemote)
import ExoMonad.Control.Effects.Effector (runEffectorViaSsh, runEffectorIO)
import ExoMonad.Control.Interpreters.Traced (traceCabal, traceGit)
import ExoMonad.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
import ExoMonad.Effect.Types (runLog, LogLevel(..))
import ExoMonad.Effect.NodeMeta (runGraphMeta, runNodeMeta, defaultNodeMeta, GraphMetadata(..))
import ExoMonad.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo)
import ExoMonad.Git.Interpreter (runGitIO)
import ExoMonad.Effects.Zellij (Zellij, checkZellijEnv, goToTab, TabId(..))
import ExoMonad.Zellij.Interpreter (runZellijIO)
import ExoMonad.Graph.Interpret (runGraph)
import qualified ExoMonad.Control.Runtime.Paths as Paths

import ExoMonad.Control.StopHook.Types
import ExoMonad.Control.StopHook.Graph
import ExoMonad.Control.StopHook.Handlers
import ExoMonad.Control.StopHook.Templates (renderStopHookTemplate)
import ExoMonad.Control.Workflow.Store (WorkflowStore, getWorkflowState, updateWorkflowState)

-- | Handle a hook event.
--
-- Executes hook-specific logic for SessionStart and Stop.
-- Other hooks pass through with default responses.
handleHook :: Logger -> Tracer -> ServerConfig -> HookInput -> Runtime -> Role -> Maybe Text -> CircuitBreakerMap -> IO ControlResponse
handleHook logger tracer config input runtime agentRole mContainerId cbMap = do
  TIO.putStrLn $ "  session=" <> input.sessionId
  TIO.putStrLn $ "  cwd=" <> input.cwd
  TIO.putStrLn $ "  role=" <> T.pack (show agentRole)
  hFlush stdout

  let spanName = "hook." <> input.hookEventName
  
  -- Manual span management
  ctx <- Context.getContext
  traceSpan <- createSpan tracer ctx spanName defaultSpanArguments
  
  result <- try $ do
    addAttribute traceSpan "session.id" input.sessionId
    addAttribute traceSpan "jsonl.file" input.transcriptPath
    forM_ input.toolUseId $ \tid -> addAttribute traceSpan "tool_use_id" tid
    forM_ input.toolName $ \tn -> addAttribute traceSpan "tool_name" tn
    
    case input.hookEventName of
      "SessionStart" -> handleSessionStart logger tracer agentRole input
      "Stop" -> handleStop logger tracer config input runtime mContainerId cbMap
      "PreToolUse" -> handlePreToolUse config input
      "SessionEnd" -> handleTranscriptHook config agentRole input
      "SubagentStop" -> handleTranscriptHook config agentRole input
      _ -> pure $ hookSuccess $ makeResponse input.hookEventName input

  endSpan traceSpan Nothing
  
  case result of
    Left (e :: SomeException) -> do
      recordException traceSpan mempty Nothing e
      throwIO e
    Right r -> pure r

-- | Handle transcript hooks (SessionEnd, SubagentStop) by shipping to OpenObserve.
handleTranscriptHook :: ServerConfig -> Role -> HookInput -> IO ControlResponse
handleTranscriptHook config role input = do
  TIO.putStrLn $ "  [HOOK] Handling " <> input.hookEventName <> " transcript shipping..."
  hFlush stdout

  case config.openObserveConfig of
    Nothing -> do
      TIO.putStrLn "  [HOOK] OpenObserve not configured, skipping transcript shipping"
      hFlush stdout
    Just ooConfig -> do
      let path = T.unpack input.transcriptPath
      if null path
        then TIO.putStrLn "  [HOOK] No transcript path provided, skipping"
        else do
          mEvents <- readJsonl path
          case mEvents of
            Nothing -> TIO.putStrLn $ "  [HOOK] Failed to read or parse transcript at " <> T.pack path
            Just events -> do
              let enriched = map (enrichEvent role input) events
              shipTranscript ooConfig enriched
          hFlush stdout

  pure $ hookSuccess $ makeResponse input.hookEventName input

-- | Helper to read NDJSON (JSONL) file.
readJsonl :: FilePath -> IO (Maybe [Value])
readJsonl path = try @SomeException (BL.readFile path) >>= \case
  Left _ -> pure Nothing
  Right content -> do
    let jsonLines = filter (not . BL.null) $ BL.split 10 content -- Split by newline (\n = 10)
    pure $ decodeAllJsonLines jsonLines
  where
    -- Handle CRLF endings by trimming a trailing '\r' (13) if present.
    stripTrailingCR :: BL.ByteString -> BL.ByteString
    stripTrailingCR bs
      | BL.null bs = bs
      | BL.last bs == 13 = BL.init bs
      | otherwise = bs

    -- Decode all lines; fail the whole read if any line fails to parse.
    decodeAllJsonLines :: [BL.ByteString] -> Maybe [Value]
    decodeAllJsonLines = go []
      where
        go acc [] = Just (reverse acc)
        go acc (l : ls) =
          case decode (stripTrailingCR l) of
            Just v  -> go (v : acc) ls
            Nothing -> Nothing

-- | Helper to enrich transcript event with session metadata.
enrichEvent :: Role -> HookInput -> Value -> Value
enrichEvent role input (Object o) = Object $ o 
  <> KeyMap.fromList 
     [ "session_id" .= input.sessionId
     , "agent_role" .= T.pack (show role)
     , "cwd" .= input.cwd
     , "hook_event" .= input.hookEventName
     ]
enrichEvent _ _ v = v

-- | Handle PreToolUse hook using policy evaluation.
handlePreToolUse :: ServerConfig -> HookInput -> IO ControlResponse
handlePreToolUse config input = do
  let toolName = fromMaybe "unknown" input.toolName
  TIO.putStrLn $ "  [HOOK] Evaluating PreToolUse policy for tool: " <> toolName
  hFlush stdout

  let decision = evaluatePolicy config.hookPolicy toolName
  case decision of
    PolicyAllow reason modifiedInput -> do
      TIO.putStrLn $ "  [HOOK] Policy: ALLOW " <> fromMaybe "" reason
      hFlush stdout
      pure $ hookSuccess $ allowPreToolUse reason modifiedInput
    PolicyDeny reason -> do
      TIO.putStrLn $ "  [HOOK] Policy: DENY " <> reason
      hFlush stdout
      pure $ hookSuccess $ denyPreToolUse reason
    PolicyAsk reason -> do
      TIO.putStrLn $ "  [HOOK] Policy: ASK " <> fromMaybe "" reason
      hFlush stdout
      -- In Claude Code, "ask" in PreToolUse triggers its own permission prompt.
      pure $ hookSuccess defaultOutput
        { hookSpecificOutput = Just $ PreToolUseOutput "ask" reason Nothing
        }

-- | Handle SessionStart hook: inject issue context.
handleSessionStart :: Logger -> Tracer -> Role -> HookInput -> IO ControlResponse
handleSessionStart logger tracer role input = do
  TIO.putStrLn "  [HOOK] Running SessionStart context injection..."
  hFlush stdout

  -- Check if we should use SSH for execution (if EXOMONAD_CONTAINER is set)
  mContainer <- lookupEnv "EXOMONAD_CONTAINER"

  -- Get binary directory (respects EXOMONAD_BIN_DIR, defaults to /usr/local/bin)
  binDir <- Paths.dockerBinDir
  let dockerCtlPath = Paths.dockerCtlBin binDir

  result <- try $ runM
    $ runLog Debug
    $ runGitHubIO defaultGitHubConfig
    $ case mContainer of 
         Just container -> 
           runSshExec logger dockerCtlPath
           $ runGitRemote (T.pack container) "." 
           $ traceGit tracer
           $ sessionStartLogic tracer role input.cwd
         Nothing -> 
           runGitIO 
           $ traceGit tracer
           $ sessionStartLogic tracer role input.cwd

  case result of
    Left (e :: SomeException) -> do
      let errMsg = "SessionStart failed: " <> T.pack (show e)
      TIO.putStrLn $ "  [HOOK] " <> errMsg
      hFlush stdout
      -- On error, still allow session to start, just without context
      pure $ hookSuccess defaultOutput
        { hookSpecificOutput = Just $ SessionStartOutput Nothing
        }
    Right mContext -> do
      TIO.putStrLn "  [HOOK] SessionStart context injected"
      hFlush stdout
      pure $ hookSuccess defaultOutput
        { hookSpecificOutput = Just $ SessionStartOutput mContext
        }

-- | Handle Stop hook: gather state, render template, provide actionable guidance.
--
-- Uses CircuitBreaker to prevent infinite loops and concurrent execution.
handleStop :: Logger -> Tracer -> ServerConfig -> HookInput -> Runtime -> Maybe Text -> CircuitBreakerMap -> IO ControlResponse
handleStop logger tracer config input _runtime mContainerId cbMap = do
  sessionId <- getOrCreateSession input
  TIO.putStrLn $ "  [HOOK] Running Stop hook with circuit breaker for session: " <> sessionId
  hFlush stdout

  now <- getCurrentTime
  withCircuitBreaker cbMap config.circuitBreakerConfig now sessionId (runStopHookLogic logger tracer config.workflowStore input mContainerId) >>= \case
    Left err -> do
      TIO.putStrLn $ "  [HOOK] Circuit breaker blocked Stop: " <> err
      hFlush stdout
      -- Block with circuit breaker error
      pure $ HookResponse
        {
          output = defaultOutput
            { continue_ = False
            , stopReason = Just $ "Circuit breaker: " <> err
            , systemMessage = Just $ "ExoMonad circuit breaker triggered: " <> err
            , hookSpecificOutput = Nothing  -- Claude Code doesn't recognize Stop in hookSpecificOutput
            }
        , exitCode = 1
        }
    Right (templateName, context) -> do
      TIO.putStrLn $ "  [HOOK] Stop hook completed, template=" <> templateName
      hFlush stdout

      let rendered = renderStopHookTemplate templateName context

      -- Increment circuit breaker stage counter if blocking
      incrementTime <- getCurrentTime
      let shouldBlock = templateName `elem` ["fix-build-errors", "max-loops", "build-stuck", "fix-test-failures", "test-stuck"]
      let stageName = context.stage
      
      -- We always increment stage in the graph/workflow state, but CB map is external
      -- For now, just logging or tracking simple stage
      if shouldBlock
        then incrementStage cbMap sessionId stageName incrementTime
        else pure ()

      -- Auto-focus if not blocking
      if not shouldBlock
        then autoFocusOnSubagentStop logger
        else pure ()

      if shouldBlock
        then pure $ HookResponse
          {
            output = defaultOutput
              {
                continue_ = True  -- Keep running so agent can fix issues
              , decision = Just "block"
              , reason = Just rendered
              , systemMessage = Just rendered
              , hookSpecificOutput = Nothing
              }
          , exitCode = 0  -- Success exit code required for decision="block"
          }
        else pure $ hookSuccess defaultOutput
          {
            systemMessage = Just rendered
          , hookSpecificOutput = Nothing
          }

-- | Get or create session ID from hook input.
getOrCreateSession :: HookInput -> IO SessionId
getOrCreateSession input = pure input.sessionId

-- | Logic to run inside circuit breaker lock.
-- Replaces old runStopHookLogic with graph execution.
runStopHookLogic :: Logger -> Tracer -> WorkflowStore -> HookInput -> Maybe Text -> IO (TemplateName, StopHookContext)
runStopHookLogic logger tracer workflowStore input mContainerId = do
  -- Get binary directory (respects EXOMONAD_BIN_DIR, defaults to /usr/local/bin)
  binDir <- Paths.dockerBinDir
  let dockerCtlPath = Paths.dockerCtlBin binDir

  -- Fetch existing workflow state (or default if new)
  initialWorkflow <- getWorkflowState workflowStore input.sessionId

  -- We need to fetch git info first to populate AgentState
  agentState <- case mContainerId of
     Just container -> runM $ runSshExec logger dockerCtlPath $ runGitRemote container "." $ traceGit tracer $ getAgentState input
     Nothing -> runM $ runGitIO $ traceGit tracer $ getAgentState input

  (result, finalState) <- case mContainerId of
        Just container ->
          runM
          $ runSshExec logger dockerCtlPath
          $ runEffectorViaSsh container
          $ runCabalRemote (Just container)
          $ traceCabal tracer
          $ runGitRemote container "."
          $ traceGit tracer
          $ runGraphMeta (GraphMetadata "stop-hook")
          $ runNodeMeta defaultNodeMeta
          $ runState initialWorkflow
          $ runGraph stopHookHandlers agentState
        Nothing ->
          -- No container: use local cabal execution via docker-ctl --local
          runM
          $ runSshExec logger dockerCtlPath
          $ runState initialWorkflow
          $ runEffectorIO logger
          $ runCabalRemote Nothing
          $ traceCabal tracer
          $ runGitIO
          $ traceGit tracer
          $ runGraphMeta (GraphMetadata "stop-hook")
          $ runNodeMeta defaultNodeMeta
          $ runGraph stopHookHandlers agentState

  -- Persist the updated workflow state
  updateWorkflowState workflowStore input.sessionId finalState

  pure result

getAgentState :: Member Git es => HookInput -> Eff es AgentState
getAgentState input = do
  mWt <- getWorktreeInfo
  let branch = (.wiBranch) <$> mWt
      issueNum = branch >>= parseIssueNumber
  pure $ AgentState
    {
      sessionId = input.sessionId
    , cwd = T.unpack input.cwd
    , branch = branch
    , issueNum = issueNum
    }

-- | Auto-focus on subagent tab when Stop hook fires.
autoFocusOnSubagentStop :: Logger -> IO ()
autoFocusOnSubagentStop logger = do
  mAutoFocus <- lookupEnv "EXOMONAD_AUTO_FOCUS_ON_ERROR"
  let autoFocusEnabled = mAutoFocus /= Just "false"

  if not autoFocusEnabled
    then pure ()
    else do
      mContainer <- lookupEnv "EXOMONAD_CONTAINER"

      -- Get binary directory (respects EXOMONAD_BIN_DIR, defaults to /usr/local/bin)
      binDir <- Paths.dockerBinDir
      let dockerCtlPath = Paths.dockerCtlBin binDir

      result <- try $ runM
        $ runZellijIO
        $ case mContainer of
             Just container -> runSshExec logger dockerCtlPath $ runGitRemote (T.pack container) "." autoFocusLogic
             Nothing -> runGitIO autoFocusLogic

      case result of
        Left (e :: SomeException) -> do
          TIO.putStrLn $ "  [HOOK] Auto-focus failed: " <> T.pack (show e)
          hFlush stdout
        Right () -> pure ()

-- | Auto-focus logic: check Zellij, parse issue ID, switch focus.
autoFocusLogic :: (Member Zellij es, Member Git es) => Eff es ()
autoFocusLogic = do
  mZellij <- checkZellijEnv
  case mZellij of
    Nothing -> pure ()
    Just _ -> do
      mWt <- getWorktreeInfo
      case mWt of
        Nothing -> pure ()
        Just wt -> do
          let branchName = wt.wiBranch
              maybeIssueNum = parseIssueNumber branchName
              maybeIssueId = T.pack . show <$> maybeIssueNum
          case maybeIssueId of
            Nothing -> pure ()
            Just bid -> do
              -- Focus on tab with Issue ID name
              _ <- goToTab (TabId bid)
              pure ()

-- | Create appropriate response based on hook type.
makeResponse :: Text -> HookInput -> HookOutput
makeResponse eventName input = case eventName of
  "PostToolUse" -> allowPostToolUse Nothing
  "PermissionRequest" -> defaultOutput
    {
      hookSpecificOutput = Just $ PermissionRequestOutput $ Allow Nothing
    }
  "UserPromptSubmit" -> defaultOutput
    {
      hookSpecificOutput = Just $ UserPromptSubmitOutput Nothing
    }
  "SessionStart" -> defaultOutput
    {
      hookSpecificOutput = Just $ SessionStartOutput $
        Just $ "ExoMonad control server connected. Session: " <> input.sessionId
    }
  "SessionEnd" -> defaultOutput
    {
      hookSpecificOutput = Just SessionEndOutput
    }
  "Stop" -> defaultOutput
    {
      hookSpecificOutput = Nothing  -- Claude Code doesn't recognize Stop in hookSpecificOutput
    }
  "SubagentStop" -> defaultOutput
    {
      hookSpecificOutput = Nothing  -- Claude Code doesn't recognize SubagentStop in hookSpecificOutput
    }
  "Notification" -> defaultOutput
    {
      hookSpecificOutput = Just NotificationOutput
    }
  "PreCompact" -> defaultOutput
    {
      hookSpecificOutput = Just PreCompactOutput
    }
  _ -> defaultOutput

-- | Default output (continue, no specific output)
defaultOutput :: HookOutput
defaultOutput = HookOutput
  {
    continue_ = True
  , stopReason = Nothing
  , suppressOutput = Nothing
  , systemMessage = Nothing
  , hookSpecificOutput = Nothing
  , decision = Nothing
  , reason = Nothing
  }
