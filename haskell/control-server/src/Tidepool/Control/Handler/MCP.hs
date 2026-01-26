-- | MCP tool call handler.
--
-- Exposes semantic code exploration and teaching tools via LSP + LLM.
-- All tools are expressed as graph DSL nodes with MCPExport annotation.
--
-- = Tier 1: Deterministic LSP Tools (Pure LSP + Heuristics)
--
-- - **find_callers**: Find actual call sites (filter imports/type sigs)
-- - **show_fields**: Quick record field lookup
-- - **show_constructors**: Show sum type constructors
--
-- = Tier 2: LLM-Enhanced Tools
--
-- - **teach-graph**: Generate teaching documents using graph DSL + Haiku
--
-- Supports training data capture via tidepool-teaching when TEACHING_ENABLED=true.
module Tidepool.Control.Handler.MCP
  ( handleMcpTool
  ) where

import Control.Exception (SomeException, displayException, try, throwIO)
import Control.Monad (when)
import Control.Monad.Freer (Eff, LastMember, runM, sendM)
import Data.Aeson (Value, fromJSON, toJSON, Result(..), encode)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import Tidepool.Control.Logging (Logger, logInfo, logDebug, logError)
import Tidepool.Control.Protocol
import Tidepool.Control.RoleConfig
import Tidepool.Control.Types (ServerConfig(..))
import Tidepool.Control.TUITools
  ( confirmActionLogic, ConfirmArgs(..), ConfirmResult(..),
    selectOptionLogic, SelectArgs(..), SelectResult(..),
    requestGuidanceLogic, GuidanceArgs(..)
  )
import Tidepool.Control.FeedbackTools
  ( registerFeedbackLogic, RegisterFeedbackArgs(..) )
import Tidepool.Control.ExoTools
  ( exoStatusLogic, ExoStatusArgs(..)
  , spawnAgentsLogic, SpawnAgentsArgs(..), SpawnAgentsResult(..)
  , filePRLogic, FilePRArgs(..), FilePRResult(..), PRInfo(..)
  )
import Tidepool.Control.PMTools
  ( pmApproveExpansionLogic, PmApproveExpansionArgs(..), PmApproveExpansionResult(..),
    pmPrioritizeLogic, PmPrioritizeArgs(..), PmPrioritizeResult(..), PrioritizeResultItem(..)
  )
import Tidepool.Control.PMStatus
  ( pmStatusLogic, PmStatusArgs(..) )
import Tidepool.Control.PMPropose (pmProposeLogic, PMProposeArgs(..), PMProposeResult(..))
import Tidepool.Control.MailboxTools
  ( sendMessageLogic, SendRequest(..)
  , checkInboxLogic
  , readMessageLogic, ReadMessageArgs(..)
  , markReadLogic, MarkReadArgs(..)
  )
import Tidepool.Control.GHTools
  ( ghIssueListLogic, GHIssueListArgs(..), GHIssueListResult(..)
  , ghIssueShowLogic, GHIssueShowArgs(..), GHIssueShowResult(..)
  , ghIssueCreateLogic, GHIssueCreateArgs(..), GHIssueCreateResult(..)
  , ghIssueUpdateLogic, GHIssueUpdateArgs(..), GHIssueUpdateResult(..)
  , ghIssueCloseLogic, GHIssueCloseArgs(..), GHIssueCloseResult(..)
  , ghIssueReopenLogic, GHIssueReopenArgs(..), GHIssueReopenResult(..)
  )
import Tidepool.Git.Interpreter (runGitIO)
import Tidepool.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
import Tidepool.Worktree.Interpreter (runWorktreeIO, defaultWorktreeConfig)
import Tidepool.FileSystem.Interpreter (runFileSystemIO)
import Tidepool.Env.Interpreter (runEnvIO)
import Tidepool.Zellij.Interpreter (runZellijIO)
import Tidepool.DockerSpawner.Interpreter (runDockerSpawner, DockerSpawnerConfig(..))
import Tidepool.Gemini.Interpreter (runGeminiIO)
import Tidepool.Effect.TUI (TUI(..))
import Tidepool.Effect.Types (runLog, LogLevel(Debug), runReturn, runTime)
import Tidepool.Graph.Goto (unwrapSingleChoice)
import Tidepool.Control.TUIInterpreter (runTUIFifo)
import Tidepool.Control.Runtime.Paths as Paths
-- import Tidepool.Teaching.LLM (TeachingConfig, loadTeachingConfig, withTeaching, runLLMWithTeaching)
-- import Tidepool.Teaching.Teacher (teacherGuidance)
import Tidepool.Effects.Observability (SpanKind(..), SpanAttribute(..), withSpan, addSpanAttribute)
import Tidepool.Observability.Interpreter (runObservabilityWithContext)
import Tidepool.Observability.Types (TraceContext, ObservabilityConfig(..), defaultLokiConfig)
import Network.HTTP.Client (newManager, defaultManagerSettings)

-- | Run TUI interpreter using FIFO-based communication.
--
-- This shells out to tui-spawner, which handles cross-container coordination
-- via named pipes (FIFOs). No WebSocket complexity needed.
runTUIInterpreter :: LastMember IO effs => Logger -> Eff (TUI ': effs) a -> Eff effs a
runTUIInterpreter = runTUIFifo

-- | Wrap MCP tool call with tracing if enabled.
withMcpTracing 
  :: Logger 
  -> ServerConfig 
  -> TraceContext 
  -> Text 
  -> Text 
  -> Value 
  -> IO ControlResponse 
  -> IO ControlResponse
withMcpTracing logger config traceCtx reqId toolName args action = do
  case config.observabilityConfig of
    Nothing -> action
    Just obsConfig -> do
      -- We want to ensure action is ONLY called once.
      -- We use an IORef to store the result so if the tracing wrapper itself 
      -- (runM or withSpan) fails after action completes, we don't re-run action.
      resRef <- newIORef Nothing
      
      let runActionOnce = do
            mRes <- readIORef resRef
            case mRes of
              Just res -> pure res
              Nothing -> do
                res <- action
                writeIORef resRef (Just res)
                pure res

      resultOrErr <- try $ runM 
        $ runObservabilityWithContext traceCtx (fromMaybe defaultLokiConfig $ ocLoki obsConfig)
        $ withSpan ("mcp:tool:" <> toolName) SpanServer 
            [ AttrText "mcp.request_id" reqId
            , AttrText "mcp.tool_name" toolName
            , AttrInt "mcp.input_size" (fromIntegral $ LBS.length $ encode args)
            ] $ do
          -- Run the actual action (or get cached result)
          res <- sendM runActionOnce
          
          -- Add response metadata
          let resSize = case res of
                McpToolResponse _ (Just val) _ -> LBS.length $ encode val
                _ -> 0
              isError = case res of
                McpToolResponse _ _ (Just _) -> True
                _ -> False

          addSpanAttribute (AttrInt "mcp.output_size" (fromIntegral resSize))
          when isError $ addSpanAttribute (AttrText "error" "true")
          
          pure res
      
      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Tracing error: " <> T.pack (show e)
          -- Rethrow as suggested to avoid masking and potential double-runs 
          -- if this were higher up, though here runActionOnce + resRef protects us.
          throwIO e
        Right res -> pure res

-- | Handle an MCP tool call.
--
-- == Tier 1: Deterministic LSP Tools (Graph DSL)
--   - "find_callers": Find actual call sites (filters imports/type sigs)
--   - "show_fields": Quick record field lookup
--   - "show_constructors": Show sum type constructors
--
-- == Tier 2: LLM-Enhanced Tools (Graph DSL)
--   - "teach-graph": Generate teaching documents via graph DSL + Haiku
--
-- == Tier 3: External Orchestration Tools (Exo)
--   - "exo_status": Get current bead context, git status, and PR info
handleMcpTool :: Logger -> ServerConfig -> TraceContext -> Text -> Text -> Value -> IO ControlResponse
handleMcpTool logger config traceCtx reqId toolName args =
  withMcpTracing logger config traceCtx reqId toolName args $ do
    let effectiveRole = fromMaybe config.defaultRole (config.role >>= roleFromText)
    
    if not (isToolAllowed effectiveRole toolName)
      then do
        logError logger $ "[MCP:" <> reqId <> "] Tool '" <> toolName <> "' not available for role " <> T.pack (show effectiveRole)
        pure $ mcpToolError reqId PermissionDenied $
          "Tool '" <> toolName <> "' not available for role " <> T.pack (show effectiveRole) <>
          ". Available tools: " <> T.intercalate ", " (Set.toList $ fromMaybe Set.empty $ roleTools effectiveRole)
      else do
        logInfo logger $ "[MCP:" <> reqId <> "] Dispatching: " <> toolName

        let currentRole = T.toLower $ T.pack $ show effectiveRole

        case toolName of
          -- Tier 1: Deterministic LSP tools (graph-based)
          -- NOTE: LSP-backed tools (find_callers, find_callees, show_fields,
          --       show_constructors) have been removed from this dispatcher.
          --       If still exported elsewhere, they will now be reported via
          --       the generic "unknown tool" handler below.

          -- TUI-interactive tools
          "confirm_action" -> handleConfirmActionTool logger reqId args
          "select_option" -> handleSelectOptionTool logger reqId args
          "request_guidance" -> handleRequestGuidanceTool logger reqId args
          "register_feedback" -> handleRegisterFeedbackTool logger reqId args

          -- Tier 2: LLM-enhanced tools (graph-based)
          -- DISABLED: teach-graph spawns recursive LLM calls, expensive during testing
          -- Re-enable once Tier 1 tools are stable
          -- "teach-graph" -> handleTeachGraphTool logger maybeTuiHandle reqId args

          -- Tier 3: External Orchestration tools (Exo)
          -- Note: exo_complete and pre_commit_check have been folded into the Stop hook
          "exo_status" -> handleExoStatusTool logger reqId args
          "spawn_agents" -> handleSpawnAgentsTool logger reqId args
          "file_pr" -> handleFilePRTool logger reqId args
          "pm_approve_expansion" -> handlePmApproveExpansionTool logger reqId args
          "pm_prioritize" -> handlePmPrioritizeTool logger reqId args
          "pm_status" -> handlePmStatusTool logger reqId args
          "pm_propose" -> handlePMProposeTool logger reqId args

          -- Mailbox tools
          "send_message" -> handleSendMessageTool logger currentRole reqId args
          "check_inbox" -> handleCheckInboxTool logger currentRole reqId args
          "read_message" -> handleReadMessageTool logger reqId args
          "mark_read" -> handleMarkReadTool logger reqId args

          -- GitHub tools
          "gh_issue_list" -> handleGHIssueListTool logger reqId args
          "gh_issue_show" -> handleGHIssueShowTool logger reqId args
          "gh_issue_create" -> handleGHIssueCreateTool logger reqId args
          "gh_issue_update" -> handleGHIssueUpdateTool logger reqId args
          "gh_issue_close" -> handleGHIssueCloseTool logger reqId args
          "gh_issue_reopen" -> handleGHIssueReopenTool logger reqId args

          _ -> do
            logError logger $ "  (unknown tool)"
            pure $ mcpToolError reqId NotFound $
              "Tool not found: " <> toolName <>
              ". Available tools: exo_status, spawn_agents, file_pr, gh_issue_list, gh_issue_create"

-- | Handle the pm_prioritize tool.
--
-- Runs the pmPrioritizeLogic to batch update bead priorities with rationale.
handlePmPrioritizeTool :: Logger -> Text -> Value -> IO ControlResponse
handlePmPrioritizeTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid pm_prioritize arguments: " <> T.pack err

    Success ppArgs -> do
      logDebug logger $ "  updates=" <> T.pack (show $ length $ ppaUpdates ppArgs)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (pmPrioritizeLogic ppArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "pm_prioritize failed: " <> T.pack (displayException e)

        Right result -> do
          let successes = filter priSuccess $ pprResults result
          logInfo logger $ "[MCP:" <> reqId <> "] Successfully prioritized " <> T.pack (show $ length successes) <> " beads"
          pure $ mcpToolSuccess reqId (toJSON result)

-- | Handle the spawn_agents tool.
--
-- Runs the SpawnAgentsGraph logic to create worktrees for parallel agents.
handleSpawnAgentsTool :: Logger -> Text -> Value -> IO ControlResponse
handleSpawnAgentsTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid spawn_agents arguments: " <> T.pack err

    Success saArgs -> do
      logDebug logger $ "  issue_numbers=" <> T.intercalate "," saArgs.saaIssueNumbers

      -- Most interpreters use default configs which assume current dir is repo root.
      let repoRoot = "." 
      
      -- Create Docker Spawner config
      manager <- newManager defaultManagerSettings
      spawnerUrl <- Paths.dockerSpawnerUrl
      let spawnerConfig = DockerSpawnerConfig spawnerUrl manager

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGeminiIO
        $ runGitHubIO defaultGitHubConfig
        $ runGitIO
        $ runWorktreeIO (defaultWorktreeConfig repoRoot)
        $ runFileSystemIO
        $ runEnvIO
        $ runZellijIO
        $ runDockerSpawner spawnerConfig
        $ fmap unwrapSingleChoice (spawnAgentsLogic saArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "spawn_agents failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Spawned " <> T.pack (show $ length $ sarWorktrees result) <> " worktrees"
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the exo_status tool.
--
-- Runs the ExoStatusGraph logic to get development context.
handleExoStatusTool :: Logger -> Text -> Value -> IO ControlResponse
handleExoStatusTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid exo_status arguments: " <> T.pack err

    Success esArgs -> do
      logDebug logger $ "  bead_id=" <> T.pack (show esArgs.esaBeadId)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGeminiIO
        $ runGitHubIO defaultGitHubConfig
        $ runGitIO
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (exoStatusLogic esArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "exo_status failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Context retrieved successfully"
          pure $ mcpToolSuccess reqId (toJSON result)


{-
-- | Handle the teach-graph tool (graph-based exploration).
--
-- Uses the graph DSL implementation with Haiku for symbol selection.
-- Supports training data capture via tidepool-teaching when TEACHING_ENABLED=true.
handleTeachGraphTool :: Logger -> LSPSession -> Maybe TUIHandle -> Text -> Value -> IO ControlResponse
handleTeachGraphTool logger lspSession maybeTuiHandle reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId $ "Invalid teach-graph arguments: " <> T.pack err

    Success query -> do
      logDebug logger $ "  topic=" <> tqTopic query
      logDebug logger $ "  seeds=" <> T.intercalate ", " (tqSeeds query)
      logDebug logger $ "  budget=" <> T.pack (show $ tqBudget query)

      -- Check for teaching mode
      maybeConfig <- loadTeachingConfig
      case maybeConfig of
        Just config -> do
          logInfo logger "  mode=teaching (recording to JSONL)"
          runWithTeaching logger config lspSession maybeTuiHandle reqId query

        Nothing -> do
          logInfo logger "  mode=production (no recording)"
          runWithoutTeaching logger lspSession maybeTuiHandle reqId query


-- | Run graph-based exploration with teaching enabled.
--
-- Records all LLM turns to anthropic.jsonl for training data.
runWithTeaching
  :: Logger
  -> TeachingConfig
  -> LSPSession
  -> Maybe TUIHandle
  -> Text
  -> TeachQuery
  -> IO ControlResponse
runWithTeaching logger config lspSession maybeTuiHandle reqId query = do
  let guidance = teacherGuidance @ScoutGemmaEffect
  resultOrErr <- try $ withTeaching config guidance $ \env -> do
    runM
      $ runLog Debug
      $ runTUIOrMock maybeTuiHandle
      $ runLSP lspSession
      $ runGeminiIO
      $ runGraphMeta (GraphMetadata "DocGenGraph")
      $ runNodeMeta defaultNodeMeta
      $ runLLMWithTeaching env
      $ runDocGenGraph query

  case resultOrErr of
    Left (e :: SomeException) -> do
      logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
      pure $ mcpToolError reqId $ "Teach-graph exploration failed: " <> T.pack (displayException e)

    Right doc -> do
      let totalUnits = length (tdPrereqs doc) + length (tdCore doc) + length (tdSupport doc)
      logInfo logger $ "[MCP:" <> reqId <> "] Generated doc with " <> T.pack (show totalUnits) <> " teaching units"
      logInfo logger $ "[MCP:" <> reqId <> "] Training data recorded"
      pure $ mcpToolSuccess reqId (toJSON doc)


-- | Run graph-based exploration without teaching.
--
-- Uses production LLM interpreter (requires ANTHROPIC_API_KEY).
runWithoutTeaching :: Logger -> LSPSession -> Maybe TUIHandle -> Text -> TeachQuery -> IO ControlResponse
runWithoutTeaching logger _lspSession _maybeTuiHandle reqId _query = do
  -- Check for ANTHROPIC_API_KEY
  maybeKey <- lookupEnv "ANTHROPIC_API_KEY"
  case maybeKey of
    Nothing -> do
      logError logger "  error: ANTHROPIC_API_KEY not set"
      pure $ mcpToolError reqId
        "ANTHROPIC_API_KEY environment variable not set. Either set it or enable teaching mode."

    Just _key -> do
      logError logger "  error: Production mode not yet implemented"
      pure $ mcpToolError reqId $
        "Production mode for teach-graph not yet implemented. " <>
        "Set TEACHING_ENABLED=true to use teaching mode instead."
-}


-- ════════════════════════════════════════════════════════════════════════════
-- TIER 1: DETERMINISTIC LSP TOOLS (Graph DSL) - DISABLED
-- ════════════════════════════════════════════════════════════════════════════


-- | Handle the file_pr tool.
--
-- Runs the FilePRGraph logic to file a pull request with bead context.
handleFilePRTool :: Logger -> Text -> Value -> IO ControlResponse
handleFilePRTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid file_pr arguments: " <> T.pack err

    Success fpArgs -> do
      logDebug logger $ "  testing=" <> fpArgs.fpaTesting

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGitHubIO defaultGitHubConfig
        $ runGitIO
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (filePRLogic fpArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "file_pr failed: " <> T.pack (displayException e)

        Right result -> do
          case result.fprPr of
            Just pr ->
              if result.fprCreated
                then logInfo logger $ "[MCP:" <> reqId <> "] PR created: " <> pr.priUrl
                else logInfo logger $ "[MCP:" <> reqId <> "] PR exists: " <> pr.priUrl
            Nothing -> logError logger $ "[MCP:" <> reqId <> "] FilePR failed: " <> fromMaybe "unknown error" result.fprError
          pure $ mcpToolSuccess reqId (toJSON result)

-- | Handle the pm_approve_expansion tool.
--
-- Runs the PmApproveExpansionGraph logic to approve or reject expansion plans.
handlePmApproveExpansionTool :: Logger -> Text -> Value -> IO ControlResponse
handlePmApproveExpansionTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid pm_approve_expansion arguments: " <> T.pack err

    Success paeArgs -> do
      logDebug logger $ "  issue_num=" <> T.pack (show paeArgs.paeaIssueNum)
      logDebug logger $ "  decision=" <> paeArgs.paeaDecision

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (pmApproveExpansionLogic paeArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "pm_approve_expansion failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Decision processed: " <> result.paerNewStatus
          pure $ mcpToolSuccess reqId (toJSON result)

-- | Handle the pm_status tool.
--
-- Runs the PmStatusGraph logic to calculate sprint health metrics.
handlePmStatusTool :: Logger -> Text -> Value -> IO ControlResponse
handlePmStatusTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid pm_status arguments: " <> T.pack err

    Success psArgs -> do
      logDebug logger $ "  period_days=" <> T.pack (show psArgs.psaPeriodDays)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runTime
        $ runGitHubIO defaultGitHubConfig
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (pmStatusLogic psArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "pm_status failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Status metrics calculated"
          pure $ mcpToolSuccess reqId (toJSON result)

-- | Handle the pm_propose tool.
--
-- Runs the PMProposeGraph logic to propose a new bead.
handlePMProposeTool :: Logger -> Text -> Value -> IO ControlResponse
handlePMProposeTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid pm_propose arguments: " <> T.pack err

    Success ppaArgs -> do
      logDebug logger $ "  title=" <> ppaArgs.ppaTitle

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runEnvIO
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (pmProposeLogic ppaArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "pm_propose failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Proposed issue: #" <> T.pack (show result.pprIssueNum)
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the confirm_action tool.
handleConfirmActionTool :: Logger -> Text -> Value -> IO ControlResponse
handleConfirmActionTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid confirm_action arguments: " <> T.pack err

    Success caArgs -> do
      logDebug logger $ "  action=" <> caAction caArgs

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runTUIInterpreter logger
        $ runReturn (confirmActionLogic caArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "confirm_action failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Action confirmed=" <> T.pack (show $ crConfirmed result)
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the register_feedback tool.
handleRegisterFeedbackTool :: Logger -> Text -> Value -> IO ControlResponse
handleRegisterFeedbackTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid register_feedback arguments: " <> T.pack err

    Success rfArgs -> do
      logDebug logger $ "  bead_id=" <> rfArgs.rfaBeadId

      resultOrErr <- try $ runM
        $ runLog Debug
        $ fmap unwrapSingleChoice (registerFeedbackLogic rfArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "register_feedback failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Feedback registered for " <> rfArgs.rfaBeadId
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the select_option tool.
handleSelectOptionTool :: Logger -> Text -> Value -> IO ControlResponse
handleSelectOptionTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid select_option arguments: " <> T.pack err

    Success soArgs -> do
      logDebug logger $ "  prompt=" <> saPrompt soArgs

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runTUIInterpreter logger
        $ runReturn (selectOptionLogic soArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "select_option failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Option selected=" <> srSelected result
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the request_guidance tool.
handleRequestGuidanceTool :: Logger -> Text -> Value -> IO ControlResponse
handleRequestGuidanceTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid request_guidance arguments: " <> T.pack err

    Success rgArgs -> do
      logDebug logger $ "  context=" <> gaContext rgArgs

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runTUIInterpreter logger
        $ runReturn (requestGuidanceLogic rgArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "request_guidance failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Guidance received"
          pure $ mcpToolSuccess reqId (toJSON result)


-- ════════════════════════════════════════════════════════════════════════════
-- MAILBOX TOOLS
-- ════════════════════════════════════════════════════════════════════════════

-- | Handle the send_message tool.
handleSendMessageTool :: Logger -> Text -> Text -> Value -> IO ControlResponse
handleSendMessageTool logger fromRole reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid send_message arguments: " <> T.pack err

    Success req -> do
      logDebug logger $ "  to=" <> req.to <> " subject=" <> req.subject

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (sendMessageLogic fromRole req)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "send_message failed: " <> T.pack (displayException e)

        Right msgId -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Message sent: #" <> T.pack (show msgId)
          pure $ mcpToolSuccess reqId (toJSON msgId)


-- | Handle the check_inbox tool.
handleCheckInboxTool :: Logger -> Text -> Text -> Value -> IO ControlResponse
handleCheckInboxTool logger myRole reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid check_inbox arguments: " <> T.pack err

    Success ciArgs -> do
      logDebug logger $ "  role=" <> myRole

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (checkInboxLogic myRole ciArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "check_inbox failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Found " <> T.pack (show $ length result) <> " messages"
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the read_message tool.
handleReadMessageTool :: Logger -> Text -> Value -> IO ControlResponse
handleReadMessageTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid read_message arguments: " <> T.pack err

    Success rmArgs -> do
      logDebug logger $ "  message_id=" <> T.pack (show rmArgs.rmaMessageId)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (readMessageLogic rmArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "read_message failed: " <> T.pack (displayException e)

        Right result -> do
          case result of
            Just _ -> logInfo logger $ "[MCP:" <> reqId <> "] Message read successfully"
            Nothing -> logError logger $ "[MCP:" <> reqId <> "] Message not found"
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the mark_read tool.
handleMarkReadTool :: Logger -> Text -> Value -> IO ControlResponse
handleMarkReadTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid mark_read arguments: " <> T.pack err

    Success mrArgs -> do
      logDebug logger $ "  message_id=" <> T.pack (show mrArgs.mraMessageId)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (markReadLogic mrArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "mark_read failed: " <> T.pack (displayException e)

        Right () -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Message marked as read"
          pure $ mcpToolSuccess reqId (toJSON ())


-- ════════════════════════════════════════════════════════════════════════════
-- GITHUB TOOLS
-- ════════════════════════════════════════════════════════════════════════════

-- | Handle the gh_issue_list tool.
handleGHIssueListTool :: Logger -> Text -> Value -> IO ControlResponse
handleGHIssueListTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid gh_issue_list arguments: " <> T.pack err

    Success gilArgs -> do
      logDebug logger $ "  status=" <> T.pack (show gilArgs.gilaStatus)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runEnvIO
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (ghIssueListLogic gilArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "gh_issue_list failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Listed " <> T.pack (show result.gilrCount) <> " issues"
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the gh_issue_show tool.
handleGHIssueShowTool :: Logger -> Text -> Value -> IO ControlResponse
handleGHIssueShowTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid gh_issue_show arguments: " <> T.pack err

    Success gisArgs -> do
      logDebug logger $ "  number=" <> T.pack (show gisArgs.gisaNumber)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runEnvIO
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (ghIssueShowLogic gisArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "gh_issue_show failed: " <> T.pack (displayException e)

        Right result -> do
          if result.gisrFound
            then logInfo logger $ "[MCP:" <> reqId <> "] Issue found"
            else logInfo logger $ "[MCP:" <> reqId <> "] Issue not found"
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the gh_issue_create tool.
handleGHIssueCreateTool :: Logger -> Text -> Value -> IO ControlResponse
handleGHIssueCreateTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid gh_issue_create arguments: " <> T.pack err

    Success gicArgs -> do
      logDebug logger $ "  title=" <> gicArgs.gcaTitle

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runEnvIO
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (ghIssueCreateLogic gicArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "gh_issue_create failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Created issue: #" <> T.pack (show result.gcrNumber)
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the gh_issue_update tool.
handleGHIssueUpdateTool :: Logger -> Text -> Value -> IO ControlResponse
handleGHIssueUpdateTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid gh_issue_update arguments: " <> T.pack err

    Success giuArgs -> do
      logDebug logger $ "  number=" <> T.pack (show giuArgs.guaNumber)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runEnvIO
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (ghIssueUpdateLogic giuArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "gh_issue_update failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Updated issue: #" <> T.pack (show result.gurNumber)
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the gh_issue_close tool.
handleGHIssueCloseTool :: Logger -> Text -> Value -> IO ControlResponse
handleGHIssueCloseTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid gh_issue_close arguments: " <> T.pack err

    Success gicArgs -> do
      logDebug logger $ "  number=" <> T.pack (show gicArgs.gclaNumber)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runEnvIO
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (ghIssueCloseLogic gicArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "gh_issue_close failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Closed issue: #" <> T.pack (show result.gclrNumber)
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the gh_issue_reopen tool.
handleGHIssueReopenTool :: Logger -> Text -> Value -> IO ControlResponse
handleGHIssueReopenTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid gh_issue_reopen arguments: " <> T.pack err

    Success graArgs -> do
      logDebug logger $ "  number=" <> T.pack (show graArgs.graNumber)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runEnvIO
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (ghIssueReopenLogic graArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "gh_issue_reopen failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Reopened issue: #" <> T.pack (show result.grrNumber)
          pure $ mcpToolSuccess reqId (toJSON result)
