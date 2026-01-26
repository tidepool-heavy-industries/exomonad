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
import Tidepool.Control.PMReviewDAG
  ( pmReviewDagLogic, PmReviewDagArgs(..), PmReviewDagResult(..)
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
import Tidepool.Control.BDTools
  ( bdListLogic, BDListArgs(..), BDListResult(..)
  , bdShowLogic, BDShowArgs(..), BDShowResult(..)
  , bdReadyLogic, BDReadyArgs(..), BDReadyResult(..)
  , bdCreateLogic, BDCreateArgs(..), BDCreateResult(..)
  , bdUpdateLogic, BDUpdateArgs(..), BDUpdateResult(..)
  , bdCloseLogic, BDCloseArgs(..), BDCloseResult(..)
  , bdAddDepLogic, BDAddDepArgs(..), BDAddDepResult(..)
  , bdAddLabelLogic, BDAddLabelArgs(..), BDAddLabelResult(..)
  )
import Tidepool.BD.Interpreter (runBDIO, defaultBDConfig, BDConfig(..))
import Tidepool.BD.GitInterpreter (runGitIO)
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
runTUIInterpreter :: LastMember IO effs => Eff (TUI ': effs) a -> Eff effs a
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
          "pm_review_dag" -> handlePmReviewDagTool logger reqId args

          -- Mailbox tools
          "send_message" -> handleSendMessageTool logger currentRole reqId args
          "check_inbox" -> handleCheckInboxTool logger currentRole reqId args
          "read_message" -> handleReadMessageTool logger reqId args
          "mark_read" -> handleMarkReadTool logger reqId args

          -- BD (Beads) tools - centralized bead operations
          "bd_list" -> handleBDListTool logger reqId args
          "bd_show" -> handleBDShowTool logger reqId args
          "bd_ready" -> handleBDReadyTool logger reqId args
          "bd_create" -> handleBDCreateTool logger reqId args
          "bd_update" -> handleBDUpdateTool logger reqId args
          "bd_close" -> handleBDCloseTool logger reqId args
          "bd_add_dep" -> handleBDAddDepTool logger reqId args
          "bd_add_label" -> handleBDAddLabelTool logger reqId args

          _ -> do
            logError logger $ "  (unknown tool)"
            pure $ mcpToolError reqId NotFound $
              "Tool not found: " <> toolName <>
              ". Available tools: exo_status, spawn_agents, file_pr, pm_approve_expansion, pm_prioritize, pm_propose"

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
        $ runBDIO defaultBDConfig
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
      logDebug logger $ "  bead_ids=" <> T.intercalate "," saArgs.saaBeadIds

      -- Most interpreters use default configs which assume current dir is repo root.
      let repoRoot = "." 
      
      -- Create Docker Spawner config
      manager <- newManager defaultManagerSettings
      spawnerUrl <- Paths.dockerSpawnerUrl
      let spawnerConfig = DockerSpawnerConfig spawnerUrl manager

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGeminiIO
        $ runBDIO defaultBDConfig
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
        $ runBDIO defaultBDConfig
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
        $ runBDIO defaultBDConfig
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
      logDebug logger $ "  bead_id=" <> paeArgs.paeaBeadId
      logDebug logger $ "  decision=" <> paeArgs.paeaDecision

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runBDIO defaultBDConfig
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
        $ runBDIO defaultBDConfig
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
        $ runBDIO defaultBDConfig
        $ fmap unwrapSingleChoice (pmProposeLogic ppaArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "pm_propose failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Proposed bead: " <> result.pprBeadId
          pure $ mcpToolSuccess reqId (toJSON result)

-- | Handle the pm_review_dag tool.
--
-- Runs the PmReviewDagGraph logic to analyze the bead DAG.
handlePmReviewDagTool :: Logger -> Text -> Value -> IO ControlResponse
handlePmReviewDagTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid pm_review_dag arguments: " <> T.pack err

    Success prdArgs -> do
      logDebug logger $ "  focus_track=" <> T.pack (show prdArgs.prdaFocusTrack)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runTime
        $ runBDIO defaultBDConfig
        $ fmap unwrapSingleChoice (pmReviewDagLogic prdArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "pm_review_dag failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] DAG analyzed: " <> T.pack (show $ length result.prdrReady) <> " ready beads"
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
        $ runTUIInterpreter
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
        $ runTUIInterpreter
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
        $ runTUIInterpreter
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
        $ runBDIO defaultBDConfig
        $ fmap unwrapSingleChoice (sendMessageLogic fromRole req)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "send_message failed: " <> T.pack (displayException e)

        Right msgId -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Message sent: " <> msgId
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
        $ runBDIO defaultBDConfig
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
      logDebug logger $ "  message_id=" <> rmArgs.rmaMessageId

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runBDIO defaultBDConfig
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
      logDebug logger $ "  message_id=" <> mrArgs.mraMessageId

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runBDIO defaultBDConfig
        $ fmap unwrapSingleChoice (markReadLogic mrArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "mark_read failed: " <> T.pack (displayException e)

        Right () -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Message marked as read"
          pure $ mcpToolSuccess reqId (toJSON ())


-- ════════════════════════════════════════════════════════════════════════════
-- BD (BEADS) TOOLS
-- ════════════════════════════════════════════════════════════════════════════

-- | Get BD config from environment.
-- Uses BEADS_DIR if set, otherwise uses auto-discovery.
getBDConfig :: IO BDConfig
getBDConfig = do
  dir <- Paths.beadsDir
  pure $ defaultBDConfig { bcBeadsDir = dir }

-- | Handle the bd_list tool.
handleBDListTool :: Logger -> Text -> Value -> IO ControlResponse
handleBDListTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid bd_list arguments: " <> T.pack err

    Success blArgs -> do
      logDebug logger $ "  status=" <> T.pack (show blArgs.blaStatus)

      bdConfig <- getBDConfig
      resultOrErr <- try $ runM
        $ runLog Debug
        $ runBDIO bdConfig
        $ fmap unwrapSingleChoice (bdListLogic blArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "bd_list failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Listed " <> T.pack (show result.blrCount) <> " beads"
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the bd_show tool.
handleBDShowTool :: Logger -> Text -> Value -> IO ControlResponse
handleBDShowTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid bd_show arguments: " <> T.pack err

    Success bsArgs -> do
      logDebug logger $ "  bead_id=" <> bsArgs.bsaBeadId

      bdConfig <- getBDConfig
      resultOrErr <- try $ runM
        $ runLog Debug
        $ runBDIO bdConfig
        $ fmap unwrapSingleChoice (bdShowLogic bsArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "bd_show failed: " <> T.pack (displayException e)

        Right result -> do
          if result.bsrFound
            then logInfo logger $ "[MCP:" <> reqId <> "] Bead found"
            else logInfo logger $ "[MCP:" <> reqId <> "] Bead not found"
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the bd_ready tool.
handleBDReadyTool :: Logger -> Text -> Value -> IO ControlResponse
handleBDReadyTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid bd_ready arguments: " <> T.pack err

    Success brArgs -> do
      logDebug logger $ "  assignee=" <> T.pack (show brArgs.braAssignee)

      bdConfig <- getBDConfig
      resultOrErr <- try $ runM
        $ runLog Debug
        $ runBDIO bdConfig
        $ fmap unwrapSingleChoice (bdReadyLogic brArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "bd_ready failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Found " <> T.pack (show result.brrCount) <> " ready beads"
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the bd_create tool.
handleBDCreateTool :: Logger -> Text -> Value -> IO ControlResponse
handleBDCreateTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid bd_create arguments: " <> T.pack err

    Success bcArgs -> do
      logDebug logger $ "  title=" <> bcArgs.bcaTitle

      bdConfig <- getBDConfig
      resultOrErr <- try $ runM
        $ runLog Debug
        $ runBDIO bdConfig
        $ fmap unwrapSingleChoice (bdCreateLogic bcArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "bd_create failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Created bead: " <> result.bcrBeadId
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the bd_update tool.
handleBDUpdateTool :: Logger -> Text -> Value -> IO ControlResponse
handleBDUpdateTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid bd_update arguments: " <> T.pack err

    Success buArgs -> do
      logDebug logger $ "  bead_id=" <> buArgs.buaBeadId

      bdConfig <- getBDConfig
      resultOrErr <- try $ runM
        $ runLog Debug
        $ runBDIO bdConfig
        $ fmap unwrapSingleChoice (bdUpdateLogic buArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "bd_update failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Updated bead: " <> result.burBeadId
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the bd_close tool.
handleBDCloseTool :: Logger -> Text -> Value -> IO ControlResponse
handleBDCloseTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid bd_close arguments: " <> T.pack err

    Success bclArgs -> do
      logDebug logger $ "  bead_id=" <> bclArgs.bclBeadId

      bdConfig <- getBDConfig
      resultOrErr <- try $ runM
        $ runLog Debug
        $ runBDIO bdConfig
        $ fmap unwrapSingleChoice (bdCloseLogic bclArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "bd_close failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Closed bead: " <> result.bclrBeadId
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the bd_add_dep tool.
handleBDAddDepTool :: Logger -> Text -> Value -> IO ControlResponse
handleBDAddDepTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid bd_add_dep arguments: " <> T.pack err

    Success badArgs -> do
      logDebug logger $ "  from_id=" <> badArgs.badFromId <> " to_id=" <> badArgs.badToId

      bdConfig <- getBDConfig
      resultOrErr <- try $ runM
        $ runLog Debug
        $ runBDIO bdConfig
        $ fmap unwrapSingleChoice (bdAddDepLogic badArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "bd_add_dep failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Added dependency: " <> result.badrFromId <> " -> " <> result.badrToId
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the bd_add_label tool.
handleBDAddLabelTool :: Logger -> Text -> Value -> IO ControlResponse
handleBDAddLabelTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid bd_add_label arguments: " <> T.pack err

    Success balArgs -> do
      logDebug logger $ "  bead_id=" <> balArgs.balBeadId <> " label=" <> balArgs.balLabel

      bdConfig <- getBDConfig
      resultOrErr <- try $ runM
        $ runLog Debug
        $ runBDIO bdConfig
        $ fmap unwrapSingleChoice (bdAddLabelLogic balArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "bd_add_label failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Added label '" <> result.balrLabel <> "' to " <> result.balrBeadId
          pure $ mcpToolSuccess reqId (toJSON result)
