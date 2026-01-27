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
-- Supports training data capture via exomonad-teaching when TEACHING_ENABLED=true.
module ExoMonad.Control.Handler.MCP
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

import ExoMonad.Control.Logging (Logger, logInfo, logDebug, logError)
import ExoMonad.Control.Protocol
import ExoMonad.Control.RoleConfig
import ExoMonad.Control.Types (ServerConfig(..))
import ExoMonad.Control.TUITools
  ( popupLogic, PopupArgs(..)
  )
import ExoMonad.Control.FeedbackTools
  ( registerFeedbackLogic, RegisterFeedbackArgs(..) )
import ExoMonad.Control.ExoTools
  ( exoStatusLogic, ExoStatusArgs(..)
  , spawnAgentsLogic, SpawnAgentsArgs(..), SpawnAgentsResult(..)
  , cleanupAgentsLogic, CleanupAgentsArgs(..), CleanupAgentsResult(..)
  , filePRLogic, FilePRArgs(..), FilePRResult(..), PRInfo(..)
  )

import ExoMonad.Control.PMTools
  ( pmApproveExpansionLogic, PmApproveExpansionArgs(..), PmApproveExpansionResult(..),
    pmPrioritizeLogic, PmPrioritizeArgs(..), PmPrioritizeResult(..), PrioritizeResultItem(..)
  )
import ExoMonad.Control.PMStatus
  ( pmStatusLogic, PmStatusArgs(..) )
import ExoMonad.Control.PMPropose (pmProposeLogic, PMProposeArgs(..), PMProposeResult(..))
import ExoMonad.Control.GHTools
  ( ghIssueListLogic, GHIssueListArgs(..), GHIssueListResult(..)
  , ghIssueShowLogic, GHIssueShowArgs(..), GHIssueShowResult(..)
  , ghIssueCreateLogic, GHIssueCreateArgs(..), GHIssueCreateResult(..)
  , ghIssueUpdateLogic, GHIssueUpdateArgs(..), GHIssueUpdateResult(..)
  , ghIssueCloseLogic, GHIssueCloseArgs(..), GHIssueCloseResult(..)
  , ghIssueReopenLogic, GHIssueReopenArgs(..), GHIssueReopenResult(..)
  )
import ExoMonad.Git.Interpreter (runGitIO)
import ExoMonad.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
import ExoMonad.Worktree.Interpreter (runWorktreeIO, defaultWorktreeConfig)
import ExoMonad.FileSystem.Interpreter (runFileSystemIO)
import ExoMonad.Env.Interpreter (runEnvIO)
import ExoMonad.Zellij.Interpreter (runZellijIO)
import ExoMonad.Control.Effects.DockerCtl (runDockerCtl)
import ExoMonad.Gemini.Interpreter (runGeminiIO)
import ExoMonad.Effect.TUI (TUI(..))
import ExoMonad.Effect.Types (runLog, LogLevel(Debug), runReturn, runTime)
import ExoMonad.Graph.Goto (unwrapSingleChoice)
import ExoMonad.Control.TUIInterpreter (runTUIFifo)
import ExoMonad.Control.Runtime.Paths as Paths
-- import ExoMonad.Teaching.LLM (TeachingConfig, loadTeachingConfig, withTeaching, runLLMWithTeaching)
-- import ExoMonad.Teaching.Teacher (teacherGuidance)
import ExoMonad.Effects.Observability (SpanKind(..), SpanAttribute(..), withSpan, addSpanAttribute)
import ExoMonad.Observability.Interpreter (runObservabilityWithContext)
import ExoMonad.Observability.Types (TraceContext, ObservabilityConfig(..), defaultLokiConfig)

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

        case toolName of
          -- Tier 1: Deterministic LSP tools (graph-based)
          -- NOTE: LSP-backed tools (find_callers, find_callees, show_fields,
          --       show_constructors) have been removed from this dispatcher.
          --       If still exported elsewhere, they will now be reported via
          --       the generic "unknown tool" handler below.

          -- TUI-interactive tools
          "popup" -> handlePopupTool logger reqId args
          "register_feedback" -> handleRegisterFeedbackTool logger reqId args

          -- Tier 2: LLM-enhanced tools (graph-based)
          -- DISABLED: teach-graph spawns recursive LLM calls, expensive during testing
          -- Re-enable once Tier 1 tools are stable
          -- "teach-graph" -> handleTeachGraphTool logger maybeTuiHandle reqId args

          -- Tier 3: External Orchestration tools (Exo)
          -- Note: exo_complete and pre_commit_check have been folded into the Stop hook
          "exo_status" -> handleExoStatusTool logger reqId args
          "spawn_agents" -> handleSpawnAgentsTool logger reqId args
          "cleanup_agents" -> handleCleanupAgentsTool logger reqId args
          "file_pr" -> handleFilePRTool logger reqId args
          "pm_approve_expansion" -> handlePmApproveExpansionTool logger reqId args
          "pm_prioritize" -> handlePmPrioritizeTool logger reqId args
          "pm_status" -> handlePmStatusTool logger reqId args
          "pm_propose" -> handlePMProposeTool logger reqId args

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
              ". Available tools: exo_status, spawn_agents, cleanup_agents, file_pr, gh_issue_list, gh_issue_create"

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

      -- Get binary directory (respects EXOMONAD_BIN_DIR env var, defaults to /usr/local/bin)
      -- In Docker: binaries are at /usr/local/bin
      -- In local dev: EXOMONAD_BIN_DIR should point to hangar runtime/bin
      binDir <- Paths.dockerBinDir
      let dockerCtlPath = Paths.dockerCtlBin binDir

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGeminiIO
        $ runGitHubIO defaultGitHubConfig
        $ runGitIO
        $ runWorktreeIO (defaultWorktreeConfig repoRoot)
        $ runFileSystemIO
        $ runEnvIO
        $ runZellijIO
        $ runDockerCtl dockerCtlPath
        $ fmap unwrapSingleChoice (spawnAgentsLogic saArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "spawn_agents failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Spawned " <> T.pack (show $ length $ sarWorktrees result) <> " worktrees"
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the cleanup_agents tool.
--
-- Runs the CleanupAgentsGraph logic to stop containers and remove worktrees.
handleCleanupAgentsTool :: Logger -> Text -> Value -> IO ControlResponse
handleCleanupAgentsTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid cleanup_agents arguments: " <> T.pack err

    Success caArgs -> do
      logDebug logger $ "  issue_numbers=" <> T.intercalate "," caArgs.caaIssueNumbers

      -- Most interpreters use default configs which assume current dir is repo root.
      let repoRoot = "."

      -- Get binary directory (respects EXOMONAD_BIN_DIR env var, defaults to /usr/local/bin)
      binDir <- Paths.dockerBinDir
      let dockerCtlPath = Paths.dockerCtlBin binDir

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runGitHubIO defaultGitHubConfig
        $ runGitIO
        $ runWorktreeIO (defaultWorktreeConfig repoRoot)
        $ runFileSystemIO
        $ runEnvIO
        $ runZellijIO
        $ runDockerCtl dockerCtlPath
        $ fmap unwrapSingleChoice (cleanupAgentsLogic caArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "cleanup_agents failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Cleaned up " <> T.pack (show $ length $ carCleaned result) <> " agents"
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
-- Supports training data capture via exomonad-teaching when TEACHING_ENABLED=true.
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


-- | Handle the popup tool.
handlePopupTool :: Logger -> Text -> Value -> IO ControlResponse
handlePopupTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid popup arguments: " <> T.pack err

    Success popupArgs -> do
      logDebug logger $ "  elements=" <> T.pack (show $ length $ paElements popupArgs)

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runTUIInterpreter logger
        $ runReturn (popupLogic popupArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "popup failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Popup completed with status=" <> T.pack (show result)
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the register_feedback tool.
handleRegisterFeedbackTool :: Logger -> Text -> Value -> IO ControlResponse
handleRegisterFeedbackTool logger reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId InvalidInput $ "Invalid register_feedback arguments: " <> T.pack err

    Success rfArgs -> do
      logDebug logger $ "  issue_id=" <> rfArgs.rfaIssueId

      resultOrErr <- try $ runM
        $ runLog Debug
        $ fmap unwrapSingleChoice (registerFeedbackLogic rfArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId ExternalFailure $ "register_feedback failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Feedback registered for " <> rfArgs.rfaIssueId
          pure $ mcpToolSuccess reqId (toJSON result)


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
