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

import Control.Exception (SomeException, displayException, try)
import Control.Monad.Freer (Eff, LastMember, interpret, runM)
import Data.Aeson (Value, fromJSON, toJSON, Result(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

import Tidepool.Control.Logging (Logger, logInfo, logDebug, logError)
import Tidepool.Control.Protocol
import Tidepool.Control.Types (ServerConfig(..))
import Tidepool.Control.Scout.DocGen (TeachQuery(..), TeachingDoc(..))
import Tidepool.Control.Scout.DocGen.Teacher (ScoutGemmaEffect)
import Tidepool.Control.Scout.Graph.Runner (runDocGenGraph)
import Tidepool.Control.LSPTools
  ( findCallersLogic, FindCallersArgs(..), FindCallersResult(..)
  , showFieldsLogic, ShowFieldsArgs(..), ShowFieldsResult(..)
  , showConstructorsLogic, ShowConstructorsArgs(..), ShowConstructorsResult(..)
  )
import Tidepool.Control.TUITools
  ( confirmActionLogic, ConfirmArgs(..), ConfirmResult(..),
    selectOptionLogic, SelectArgs(..), SelectResult(..),
    requestGuidanceLogic, GuidanceArgs(..)
  )
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
import Tidepool.Control.PMTools
  ( pmApproveExpansionLogic, PmApproveExpansionArgs(..), PmApproveExpansionResult(..),
    pmPrioritizeLogic, PmPrioritizeArgs(..), PmPrioritizeResult(..), PrioritizeResultItem(..)
  )
import Tidepool.Control.PMPropose (pmProposeLogic, PMProposeArgs(..), PMProposeResult(..))
import Tidepool.Control.MailboxTools
  ( sendMessageLogic, SendRequest(..)
  , checkInboxLogic, CheckInboxArgs(..)
  , readMessageLogic, ReadMessageArgs(..)
  , markReadLogic, MarkReadArgs(..)
  )
import Tidepool.BD.Interpreter (runBDIO, defaultBDConfig)
import Tidepool.BD.GitInterpreter (runGitIO)
import Tidepool.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
import Tidepool.Justfile.Interpreter (runJustfileIO)
import Tidepool.Worktree.Interpreter (runWorktreeIO, defaultWorktreeConfig)
import Tidepool.Gemini.Interpreter (runGeminiIO)
import Tidepool.Effect.NodeMeta (runNodeMeta, runGraphMeta, defaultNodeMeta, GraphMetadata(..))
import Tidepool.Effect.TUI (TUI(..), Interaction(..))
import Tidepool.Effect.Types (runLog, LogLevel(..), runReturn)
import Tidepool.Graph.Goto (unwrapSingleChoice)
import Tidepool.LSP.Interpreter (LSPSession, runLSP)
import Tidepool.TUI.Interpreter (TUIHandle, runTUI)
import Tidepool.Teaching.LLM (TeachingConfig, loadTeachingConfig, withTeaching, runLLMWithTeaching)
import Tidepool.Teaching.Teacher (teacherGuidance)

-- | Run TUI interpreter if handle is available, otherwise run mock.
runTUIOrMock :: LastMember IO effs => Maybe TUIHandle -> Eff (TUI ': effs) a -> Eff effs a
runTUIOrMock (Just h) = runTUI h
runTUIOrMock Nothing = interpret $ \case
  ShowUI _ -> pure $ ButtonClicked "mock" "cancel"
  UpdateUI _ -> pure ()
  CloseUI -> pure ()

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
handleMcpTool :: Logger -> ServerConfig -> LSPSession -> Maybe TUIHandle -> Text -> Text -> Value -> IO ControlResponse
handleMcpTool logger config lspSession maybeTuiHandle reqId toolName args = do
  logInfo logger $ "[MCP:" <> reqId <> "] Dispatching: " <> toolName

  let currentRole = fromMaybe "unknown" config.role

  case toolName of
    -- Tier 1: Deterministic LSP tools (graph-based)
    "find_callers" -> handleFindCallersTool logger lspSession maybeTuiHandle reqId args
    "show_fields" -> handleShowFieldsTool logger lspSession maybeTuiHandle reqId args
    "show_constructors" -> handleShowConstructorsTool logger lspSession maybeTuiHandle reqId args

    -- TUI-interactive tools
    "confirm_action" -> handleConfirmActionTool logger lspSession maybeTuiHandle reqId args
    "select_option" -> handleSelectOptionTool logger lspSession maybeTuiHandle reqId args
    "request_guidance" -> handleRequestGuidanceTool logger lspSession maybeTuiHandle reqId args

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
    "pm_propose" -> handlePMProposeTool logger reqId args

    -- Mailbox tools
    "send_message" -> handleSendMessageTool logger currentRole reqId args
    "check_inbox" -> handleCheckInboxTool logger currentRole reqId args
    "read_message" -> handleReadMessageTool logger reqId args
    "mark_read" -> handleMarkReadTool logger reqId args

    _ -> do
      logError logger $ "  (unknown tool)"
      pure $ mcpToolError reqId $
        "Tool not found: " <> toolName <>
        ". Available tools: find_callers, show_fields, show_constructors, teach-graph, exo_status, exo_complete, exo_reconstitute, pre_commit_check, spawn_agents, file_pr, pm_approve_expansion, pm_prioritize, pm_propose"

-- | Handle the pm_prioritize tool.
--
-- Runs the pmPrioritizeLogic to batch update bead priorities with rationale.
handlePmPrioritizeTool :: Logger -> Text -> Value -> IO ControlResponse
handlePmPrioritizeTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId $ "Invalid pm_prioritize arguments: " <> T.pack err

    Success ppArgs -> do
      logDebug logger $ "  updates=" <> T.pack (show $ length $ ppaUpdates ppArgs)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runBDIO defaultBDConfig
        $ fmap unwrapSingleChoice (pmPrioritizeLogic ppArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId $ "pm_prioritize failed: " <> T.pack (displayException e)

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
      pure $ mcpToolError reqId $ "Invalid pre_commit_check arguments: " <> T.pack err

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
          pure $ mcpToolError reqId $ "pre_commit_check failed: " <> T.pack (displayException e)

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
      pure $ mcpToolError reqId $ "Invalid spawn_agents arguments: " <> T.pack err

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
        $ fmap unwrapSingleChoice (spawnAgentsLogic saArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId $ "spawn_agents failed: " <> T.pack (displayException e)

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
      pure $ mcpToolError reqId $ "Invalid exo_status arguments: " <> T.pack err

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
          pure $ mcpToolError reqId $ "exo_status failed: " <> T.pack (displayException e)

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
      pure $ mcpToolError reqId $ "Invalid exo_complete arguments: " <> T.pack err

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
          pure $ mcpToolError reqId $ "exo_complete failed: " <> T.pack (displayException e)

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
      pure $ mcpToolError reqId $ "Invalid exo_reconstitute arguments: " <> T.pack err

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
          pure $ mcpToolError reqId $ "exo_reconstitute failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Beads synced and context refreshed successfully"
          pure $ mcpToolSuccess reqId (toJSON result)


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
      pure $ mcpToolError reqId $ "Invalid find_callers arguments: " <> T.pack err

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
          pure $ mcpToolError reqId $ "find_callers failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Found " <> T.pack (show $ length $ fcrCallSites result) <> " call sites"
          logDebug logger $ "[MCP:" <> reqId <> "] Filtered " <> T.pack (show $ fcrFilteredCount result) <> " references"
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the show_fields tool.
--
-- Runs the ShowFieldsGraph logic to show fields of a Haskell record type.
handleShowFieldsTool :: Logger -> LSPSession -> Maybe TUIHandle -> Text -> Value -> IO ControlResponse
handleShowFieldsTool logger lspSession maybeTuiHandle reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId $ "Invalid show_fields arguments: " <> T.pack err

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
          pure $ mcpToolError reqId $ "show_fields failed: " <> T.pack (displayException e)

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
      pure $ mcpToolError reqId $ "Invalid show_constructors arguments: " <> T.pack err

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
          pure $ mcpToolError reqId $ "show_constructors failed: " <> T.pack (displayException e)

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
      pure $ mcpToolError reqId $ "Invalid file_pr arguments: " <> T.pack err

    Success fpArgs -> do
      logDebug logger $ "  bead_id=" <> T.pack (show fpArgs.fpaBeadId)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runBDIO defaultBDConfig
        $ runGitIO
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (filePRLogic fpArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId $ "file_pr failed: " <> T.pack (displayException e)

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
      pure $ mcpToolError reqId $ "Invalid pm_approve_expansion arguments: " <> T.pack err

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
          pure $ mcpToolError reqId $ "pm_approve_expansion failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Decision processed: " <> result.paerNewStatus
          pure $ mcpToolSuccess reqId (toJSON result)

-- | Handle the pm_propose tool.
--
-- Runs the PMProposeGraph logic to propose a new bead.
handlePMProposeTool :: Logger -> Text -> Value -> IO ControlResponse
handlePMProposeTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId $ "Invalid pm_propose arguments: " <> T.pack err

    Success ppaArgs -> do
      logDebug logger $ "  title=" <> ppaArgs.ppaTitle

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runBDIO defaultBDConfig
        $ fmap unwrapSingleChoice (pmProposeLogic ppaArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId $ "pm_propose failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Proposed bead: " <> result.pprBeadId
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the confirm_action tool.
handleConfirmActionTool :: Logger -> LSPSession -> Maybe TUIHandle -> Text -> Value -> IO ControlResponse
handleConfirmActionTool logger _lspSession maybeTuiHandle reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId $ "Invalid confirm_action arguments: " <> T.pack err

    Success caArgs -> do
      logDebug logger $ "  action=" <> caAction caArgs

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runTUIOrMock maybeTuiHandle
        $ runReturn (confirmActionLogic caArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId $ "confirm_action failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Action confirmed=" <> T.pack (show $ crConfirmed result)
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the select_option tool.
handleSelectOptionTool :: Logger -> LSPSession -> Maybe TUIHandle -> Text -> Value -> IO ControlResponse
handleSelectOptionTool logger _lspSession maybeTuiHandle reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId $ "Invalid select_option arguments: " <> T.pack err

    Success soArgs -> do
      logDebug logger $ "  prompt=" <> saPrompt soArgs

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runTUIOrMock maybeTuiHandle
        $ runReturn (selectOptionLogic soArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId $ "select_option failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Option selected=" <> srSelected result
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the request_guidance tool.
handleRequestGuidanceTool :: Logger -> LSPSession -> Maybe TUIHandle -> Text -> Value -> IO ControlResponse
handleRequestGuidanceTool logger _lspSession maybeTuiHandle reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId $ "Invalid request_guidance arguments: " <> T.pack err

    Success rgArgs -> do
      logDebug logger $ "  context=" <> gaContext rgArgs

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runTUIOrMock maybeTuiHandle
        $ runReturn (requestGuidanceLogic rgArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId $ "request_guidance failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Guidance received"
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the bead_to_pr tool.
handleBeadToPrTool :: Logger -> Text -> Value -> IO ControlResponse
handleBeadToPrTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId $ "Invalid bead_to_pr arguments: " <> T.pack err

    Success btpArgs -> do
      logDebug logger $ "  bead_id=" <> btpaBeadId btpArgs

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (beadToPrLogic btpArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId $ "bead_to_pr failed: " <> T.pack (displayException e)

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
      pure $ mcpToolError reqId $ "Invalid pr_to_bead arguments: " <> T.pack err

    Success ptbArgs -> do
      logDebug logger $ "  pr_number=" <> T.pack (show $ ptbaPrNumber ptbArgs)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGitHubIO defaultGitHubConfig
        $ fmap unwrapSingleChoice (prToBeadLogic ptbArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId $ "pr_to_bead failed: " <> T.pack (displayException e)

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
      pure $ mcpToolError reqId $ "Invalid send_message arguments: " <> T.pack err

    Success req -> do
      logDebug logger $ "  to=" <> req.to <> " subject=" <> req.subject

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runBDIO defaultBDConfig
        $ fmap unwrapSingleChoice (sendMessageLogic fromRole req)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId $ "send_message failed: " <> T.pack (displayException e)

        Right msgId -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Message sent: " <> msgId
          pure $ mcpToolSuccess reqId (toJSON msgId)


-- | Handle the check_inbox tool.
handleCheckInboxTool :: Logger -> Text -> Text -> Value -> IO ControlResponse
handleCheckInboxTool logger myRole reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId $ "Invalid check_inbox arguments: " <> T.pack err

    Success ciArgs -> do
      logDebug logger $ "  role=" <> myRole

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runBDIO defaultBDConfig
        $ fmap unwrapSingleChoice (checkInboxLogic myRole ciArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId $ "check_inbox failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Found " <> T.pack (show $ length result) <> " messages"
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the read_message tool.
handleReadMessageTool :: Logger -> Text -> Value -> IO ControlResponse
handleReadMessageTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId $ "Invalid read_message arguments: " <> T.pack err

    Success rmArgs -> do
      logDebug logger $ "  message_id=" <> rmArgs.rmaMessageId

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runBDIO defaultBDConfig
        $ fmap unwrapSingleChoice (readMessageLogic rmArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId $ "read_message failed: " <> T.pack (displayException e)

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
      pure $ mcpToolError reqId $ "Invalid mark_read arguments: " <> T.pack err

    Success mrArgs -> do
      logDebug logger $ "  message_id=" <> mrArgs.mraMessageId

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runBDIO defaultBDConfig
        $ fmap unwrapSingleChoice (markReadLogic mrArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId $ "mark_read failed: " <> T.pack (displayException e)

        Right () -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Message marked as read"
          pure $ mcpToolSuccess reqId (toJSON ())
