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
import Control.Monad.Freer (Eff, LastMember, interpret, runM, sendM)
import Data.Aeson (Value, fromJSON, toJSON, Result(..), encode)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)

import Tidepool.Control.Logging (Logger, logInfo, logDebug, logError)
import Tidepool.Control.Protocol
import Tidepool.Control.Types (ServerConfig(..))
import Tidepool.Control.LSPTools
import Tidepool.Control.TUITools
  ( confirmActionLogic, ConfirmArgs(..), ConfirmResult(..),
    selectOptionLogic, SelectArgs(..), SelectResult(..),
    requestGuidanceLogic, GuidanceArgs(..)
  )
import Tidepool.Control.FeedbackTools
  ( registerFeedbackLogic, RegisterFeedbackArgs(..) )
import Tidepool.Control.ExoTools
  ( exoStatusLogic, ExoStatusArgs(..)
  , exoCompleteLogic, ExoCompleteArgs(..), ExoCompleteResult(..)
  , exoReconstituteLogic, ExoReconstituteArgs(..)
  , preCommitCheckLogic, PreCommitCheckArgs(..), PreCommitCheckResult(..)
  , spawnAgentsLogic, SpawnAgentsArgs(..), SpawnAgentsResult(..)
  , filePRLogic, FilePRArgs(..), FilePRResult(..)
  , beadToPrLogic, BeadToPrArgs(..), BeadToPrResult(..), PRInfo(..)
  , prToBeadLogic, PrToBeadArgs(..), PrToBeadResult(..)
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
import Tidepool.BD.Interpreter (runBDIO, defaultBDConfig)
import Tidepool.BD.GitInterpreter (runGitIO)
import Tidepool.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
import Tidepool.Justfile.Interpreter (runJustfileIO)
import Tidepool.Worktree.Interpreter (runWorktreeIO, defaultWorktreeConfig)
import Tidepool.FileSystem.Interpreter (runFileSystemIO)
import Tidepool.Env.Interpreter (runEnvIO)
import Tidepool.Zellij.Interpreter (runZellijIO)
import Tidepool.Gemini.Interpreter (runGeminiIO)
import Tidepool.Effect.TUI (TUI(..), PopupResult(..))
import Tidepool.Effect.Types (runLog, LogLevel(Debug), runReturn, runTime)
import Tidepool.Graph.Goto (unwrapSingleChoice)
import Tidepool.LSP.Interpreter (LSPSession, runLSP)
import Tidepool.TUI.Interpreter (TUIHandle, runTUI)
-- import Tidepool.Teaching.LLM (TeachingConfig, loadTeachingConfig, withTeaching, runLLMWithTeaching)
-- import Tidepool.Teaching.Teacher (teacherGuidance)
import Tidepool.Effects.Observability (SpanKind(..), SpanAttribute(..), withSpan, addSpanAttribute)
import Tidepool.Observability.Interpreter (runObservabilityWithContext)
import Tidepool.Observability.Types (TraceContext, ObservabilityConfig(..), defaultLokiConfig)

-- | Run TUI interpreter if handle is available, otherwise run mock.
runTUIOrMock :: LastMember IO effs => Maybe TUIHandle -> Eff (TUI ': effs) a -> Eff effs a
runTUIOrMock (Just h) = runTUI h
runTUIOrMock Nothing = interpret $ \case
  ShowUI _ -> pure $ PopupResult "decline" (toJSON (mempty :: [(String, String)]))

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
handleMcpTool :: Logger -> ServerConfig -> LSPSession -> Maybe TUIHandle -> TraceContext -> Text -> Text -> Value -> IO ControlResponse
handleMcpTool logger config lspSession maybeTuiHandle traceCtx reqId toolName args = 
  withMcpTracing logger config traceCtx reqId toolName args $ do
    logInfo logger $ "[MCP:" <> reqId <> "] Dispatching: " <> toolName

    let currentRole = fromMaybe "unknown" config.role

    case toolName of
      -- Tier 1: Deterministic LSP tools (graph-based)
      "find_callers" -> handleFindCallersTool logger lspSession maybeTuiHandle reqId args
      "find_callees" -> handleFindCalleesTool logger lspSession maybeTuiHandle reqId args
      "show_fields" -> handleShowFieldsTool logger lspSession maybeTuiHandle reqId args
      "show_constructors" -> handleShowConstructorsTool logger lspSession maybeTuiHandle reqId args

      -- TUI-interactive tools
      "confirm_action" -> handleConfirmActionTool logger lspSession maybeTuiHandle reqId args
      "select_option" -> handleSelectOptionTool logger lspSession maybeTuiHandle reqId args
      "request_guidance" -> handleRequestGuidanceTool logger lspSession maybeTuiHandle reqId args
      "register_feedback" -> handleRegisterFeedbackTool logger reqId args

      -- Tier 2: LLM-enhanced tools (graph-based)
      -- DISABLED: teach-graph spawns recursive LLM calls, expensive during testing
      -- Re-enable once Tier 1 tools are stable
      -- "teach-graph" -> handleTeachGraphTool logger lspSession maybeTuiHandle reqId args

      -- Tier 3: External Orchestration tools (Exo)
      "exo_status" -> handleExoStatusTool logger lspSession reqId args
      "exo_complete" -> handleExoCompleteTool logger lspSession reqId args
      "exo_reconstitute" -> handleExoReconstituteTool logger lspSession reqId args
      "pre_commit_check" -> handlePreCommitCheckTool logger reqId args
      "spawn_agents" -> handleSpawnAgentsTool logger lspSession reqId args
      "file_pr" -> handleFilePRTool logger lspSession reqId args
      "bead_to_pr" -> handleBeadToPrTool logger reqId args
      "pr_to_bead" -> handlePrToBeadTool logger reqId args
      "pm_approve_expansion" -> handlePmApproveExpansionTool logger lspSession reqId args
      "pm_prioritize" -> handlePmPrioritizeTool logger reqId args
      "pm_status" -> handlePmStatusTool logger reqId args
      "pm_propose" -> handlePMProposeTool logger reqId args
      "pm_review_dag" -> handlePmReviewDagTool logger reqId args

      -- Mailbox tools
      "send_message" -> handleSendMessageTool logger currentRole reqId args
      "check_inbox" -> handleCheckInboxTool logger currentRole reqId args
      "read_message" -> handleReadMessageTool logger reqId args
      "mark_read" -> handleMarkReadTool logger reqId args

      _ -> do
        logError logger $ "  (unknown tool)"
        pure $ mcpToolError reqId NotFound $
          "Tool not found: " <> toolName <>
          ". Available tools: find_callers, find_callees, show_fields, show_constructors, teach-graph, exo_status, exo_complete, exo_reconstitute, pre_commit_check, spawn_agents, file_pr, pm_approve_expansion, pm_prioritize, pm_propose"

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

-- | Handle the pre_commit_check tool.
--
-- Runs a 'just' recipe (defaulting to pre-commit-fast) and returns structured results.
handlePreCommitCheckTool :: Logger -> Text -> Value -> IO ControlResponse
handlePreCommitCheckTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid pre_commit_check arguments: " <> T.pack err

    Success pccArgs -> do
      logDebug logger $ "  recipe=" <> T.pack (show pccArgs.pccaRecipe)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGeminiIO
        $ runJustfileIO
        $ fmap unwrapSingleChoice (preCommitCheckLogic pccArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "pre_commit_check failed: " <> T.pack (displayException e)

        Right result -> do
          if result.pccrSuccess
            then logInfo logger $ "[MCP:" <> reqId <> "] Pre-commit check passed"
            else logError logger $ "[MCP:" <> reqId <> "] Pre-commit check failed"
          pure $ mcpToolSuccess reqId (toJSON result)

-- | Handle the spawn_agents tool.
--
-- Runs the SpawnAgentsGraph logic to create worktrees for parallel agents.
handleSpawnAgentsTool :: Logger -> LSPSession -> Text -> Value -> IO ControlResponse
handleSpawnAgentsTool logger _lspSession reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid spawn_agents arguments: " <> T.pack err

    Success saArgs -> do
      logDebug logger $ "  bead_ids=" <> T.intercalate "," saArgs.saaBeadIds

      -- Most interpreters use default configs which assume current dir is repo root.
      let repoRoot = "." 
      
      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGeminiIO
        $ runBDIO defaultBDConfig
        $ runGitIO
        $ runWorktreeIO (defaultWorktreeConfig repoRoot)
        $ runFileSystemIO
        $ runEnvIO
        $ runZellijIO
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
handleExoStatusTool :: Logger -> LSPSession -> Text -> Value -> IO ControlResponse
handleExoStatusTool logger _lspSession reqId args = do
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


-- | Handle the exo_complete tool.
--
-- Runs the ExoCompleteGraph logic to finalize work on a bead.
handleExoCompleteTool :: Logger -> LSPSession -> Text -> Value -> IO ControlResponse
handleExoCompleteTool logger _lspSession reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid exo_complete arguments: " <> T.pack err

    Success ecArgs -> do
      logDebug logger $ "  bead_id=" <> T.pack (show ecArgs.ecaBeadId)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGeminiIO
        $ runBDIO defaultBDConfig
        $ runGitIO
        $ fmap unwrapSingleChoice (exoCompleteLogic ecArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "exo_complete failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Bead " <> result.ecrBeadId <> " completed"
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the exo_reconstitute tool.
--
-- Runs the ExoReconstituteGraph logic to sync beads and refresh context.
handleExoReconstituteTool :: Logger -> LSPSession -> Text -> Value -> IO ControlResponse
handleExoReconstituteTool logger _lspSession reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid exo_reconstitute arguments: " <> T.pack err

    Success erArgs -> do
      logDebug logger $ "  bead_id=" <> T.pack (show erArgs.eraBeadId)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGeminiIO
        $ runBDIO defaultBDConfig
        $ runGitIO
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (exoReconstituteLogic erArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "exo_reconstitute failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Beads synced and context refreshed successfully"
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
-- TIER 1: DETERMINISTIC LSP TOOLS (Graph DSL)
-- ════════════════════════════════════════════════════════════════════════════

-- | Handle the find_callers tool.
--
-- Runs the FindCallersGraph logic to find actual call sites of a function,
-- filtering out imports, type signatures, and comments.
handleFindCallersTool :: Logger -> LSPSession -> Maybe TUIHandle -> Text -> Value -> IO ControlResponse
handleFindCallersTool logger lspSession maybeTuiHandle reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid find_callers arguments: " <> T.pack err

    Success fcArgs -> do
      logDebug logger $ "  name=" <> fcaName fcArgs

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGeminiIO
        $ runTUIOrMock maybeTuiHandle
        $ runLSP lspSession
        $ runReturn (findCallersLogic fcArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "find_callers failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Found " <> T.pack (show $ length $ fcrCallSites result) <> " call sites"
          logDebug logger $ "[MCP:" <> reqId <> "] Filtered " <> T.pack (show $ fcrFilteredCount result) <> " references"
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the find_callees tool.
--
-- Runs the FindCalleesGraph logic to find outgoing calls from a function.
handleFindCalleesTool :: Logger -> LSPSession -> Maybe TUIHandle -> Text -> Value -> IO ControlResponse
handleFindCalleesTool logger lspSession maybeTuiHandle reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid find_callees arguments: " <> T.pack err

    Success fceArgs -> do
      logDebug logger $ "  name=" <> fceName fceArgs

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGeminiIO
        $ runTUIOrMock maybeTuiHandle
        $ runLSP lspSession
        $ runReturn (findCalleesLogic fceArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "find_callees failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Found " <> T.pack (show $ length $ fceCallees result) <> " callees"
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the show_fields tool.
--
-- Runs the ShowFieldsGraph logic to show fields of a Haskell record type.
handleShowFieldsTool :: Logger -> LSPSession -> Maybe TUIHandle -> Text -> Value -> IO ControlResponse
handleShowFieldsTool logger lspSession maybeTuiHandle reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid show_fields arguments: " <> T.pack err

    Success sfArgs -> do
      logDebug logger $ "  type_name=" <> sfaTypeName sfArgs

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGeminiIO
        $ runTUIOrMock maybeTuiHandle
        $ runLSP lspSession
        $ runReturn (showFieldsLogic sfArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "show_fields failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Found " <> T.pack (show $ length $ sfrFields result) <> " fields"
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the show_constructors tool.
--
-- Runs the ShowConstructorsGraph logic to show constructors of a Haskell sum type or GADT.
handleShowConstructorsTool :: Logger -> LSPSession -> Maybe TUIHandle -> Text -> Value -> IO ControlResponse
handleShowConstructorsTool logger lspSession maybeTuiHandle reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid show_constructors arguments: " <> T.pack err

    Success scArgs -> do
      logDebug logger $ "  type_name=" <> scaTypeName scArgs

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGeminiIO
        $ runTUIOrMock maybeTuiHandle
        $ runLSP lspSession
        $ runReturn (showConstructorsLogic scArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "show_constructors failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Found " <> T.pack (show $ length $ scrConstructors result) <> " constructors"
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the file_pr tool.
--
-- Runs the FilePRGraph logic to file a pull request with bead context.
handleFilePRTool :: Logger -> LSPSession -> Text -> Value -> IO ControlResponse
handleFilePRTool logger _lspSession reqId args = do
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
          case result.fprUrl of
            Just url -> logInfo logger $ "[MCP:" <> reqId <> "] PR created: " <> url
            Nothing -> logError logger $ "[MCP:" <> reqId <> "] FilePR failed: " <> fromMaybe "unknown error" result.fprError
          pure $ mcpToolSuccess reqId (toJSON result)

-- | Handle the pm_approve_expansion tool.
--
-- Runs the PmApproveExpansionGraph logic to approve or reject expansion plans.
handlePmApproveExpansionTool :: Logger -> LSPSession -> Text -> Value -> IO ControlResponse
handlePmApproveExpansionTool logger _lspSession reqId args = do
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
handleConfirmActionTool :: Logger -> LSPSession -> Maybe TUIHandle -> Text -> Value -> IO ControlResponse
handleConfirmActionTool logger _lspSession maybeTuiHandle reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid confirm_action arguments: " <> T.pack err

    Success caArgs -> do
      logDebug logger $ "  action=" <> caAction caArgs

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runTUIOrMock maybeTuiHandle
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
handleSelectOptionTool :: Logger -> LSPSession -> Maybe TUIHandle -> Text -> Value -> IO ControlResponse
handleSelectOptionTool logger _lspSession maybeTuiHandle reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid select_option arguments: " <> T.pack err

    Success soArgs -> do
      logDebug logger $ "  prompt=" <> saPrompt soArgs

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runTUIOrMock maybeTuiHandle
        $ runReturn (selectOptionLogic soArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "select_option failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Option selected=" <> srSelected result
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the request_guidance tool.
handleRequestGuidanceTool :: Logger -> LSPSession -> Maybe TUIHandle -> Text -> Value -> IO ControlResponse
handleRequestGuidanceTool logger _lspSession maybeTuiHandle reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid request_guidance arguments: " <> T.pack err

    Success rgArgs -> do
      logDebug logger $ "  context=" <> gaContext rgArgs

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runTUIOrMock maybeTuiHandle
        $ runReturn (requestGuidanceLogic rgArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "request_guidance failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Guidance received"
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the bead_to_pr tool.
handleBeadToPrTool :: Logger -> Text -> Value -> IO ControlResponse
handleBeadToPrTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid bead_to_pr arguments: " <> T.pack err

    Success btpArgs -> do
      logDebug logger $ "  bead_id=" <> btpaBeadId btpArgs

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (beadToPrLogic btpArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "bead_to_pr failed: " <> T.pack (displayException e)

        Right result -> do
          case btprPR result of
            Just pr -> logInfo logger $ "[MCP:" <> reqId <> "] Found PR: " <> T.pack (show pr.priNumber)
            Nothing -> logInfo logger $ "[MCP:" <> reqId <> "] No PR found for bead " <> btpaBeadId btpArgs
          pure $ mcpToolSuccess reqId (toJSON result)

-- | Handle the pr_to_bead tool.
handlePrToBeadTool :: Logger -> Text -> Value -> IO ControlResponse
handlePrToBeadTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid pr_to_bead arguments: " <> T.pack err

    Success ptbArgs -> do
      logDebug logger $ "  pr_number=" <> T.pack (show $ ptbaPrNumber ptbArgs)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (prToBeadLogic ptbArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "pr_to_bead failed: " <> T.pack (displayException e)

        Right result -> do
          case ptbrBeadId result of
            Just bid -> logInfo logger $ "[MCP:" <> reqId <> "] Found bead ID: " <> bid
            Nothing -> logInfo logger $ "[MCP:" <> reqId <> "] No bead ID found in PR title"
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
