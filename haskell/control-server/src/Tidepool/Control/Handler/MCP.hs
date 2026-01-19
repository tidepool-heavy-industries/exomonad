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
import Control.Monad.Freer (runM)
import Data.Aeson (Value, fromJSON, toJSON, Result(..))
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)

import Tidepool.Control.Logging (Logger, logInfo, logDebug, logError)
import Tidepool.Control.Protocol
import Tidepool.Control.Scout.DocGen (TeachQuery(..), TeachingDoc(..))
import Tidepool.Control.Scout.DocGen.Teacher (ScoutGemmaEffect)
import Tidepool.Control.Scout.Graph.Runner (runDocGenGraph)
import Tidepool.Control.LSPTools
  ( findCallersLogic, FindCallersArgs(..), FindCallersResult(..)
  , showFieldsLogic, ShowFieldsArgs(..), ShowFieldsResult(..)
  , showConstructorsLogic, ShowConstructorsArgs(..), ShowConstructorsResult(..)
  )
import Tidepool.Control.ExoTools
  ( exoStatusLogic, ExoStatusArgs(..), ExoStatusResult(..)
  )
import Tidepool.BD.Interpreter (runBDIO, defaultBDConfig)
import Tidepool.BD.GitInterpreter (runGitIO)
import Tidepool.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
import Tidepool.Effect.NodeMeta (runNodeMeta, runGraphMeta, defaultNodeMeta, GraphMetadata(..))
import Tidepool.Effect.Types (runLog, LogLevel(..))
import Tidepool.Graph.Goto (unwrapSingleChoice)
import Tidepool.LSP.Interpreter (LSPSession, runLSP)
import Tidepool.Teaching.LLM (TeachingConfig, loadTeachingConfig, withTeaching, runLLMWithTeaching)
import Tidepool.Teaching.Teacher (teacherGuidance)

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
handleMcpTool :: Logger -> LSPSession -> Text -> Text -> Value -> IO ControlResponse
handleMcpTool logger lspSession reqId toolName args = do
  logInfo logger $ "[MCP:" <> reqId <> "] Dispatching: " <> toolName

  case toolName of
    -- Tier 1: Deterministic LSP tools (graph-based)
    "find_callers" -> handleFindCallersTool logger lspSession reqId args
    "show_fields" -> handleShowFieldsTool logger lspSession reqId args
    "show_constructors" -> handleShowConstructorsTool logger lspSession reqId args

    -- Tier 2: LLM-enhanced tools (graph-based)
    "teach-graph" -> handleTeachGraphTool logger lspSession reqId args

    -- Tier 3: External Orchestration tools (Exo)
    "exo_status" -> handleExoStatusTool logger lspSession reqId args

    _ -> do
      logError logger $ "  (unknown tool)"
      pure $ mcpToolError reqId $
        "Tool not found: " <> toolName <>
        ". Available tools: find_callers, show_fields, show_constructors, teach-graph, exo_status"

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


-- | Handle the teach-graph tool (graph-based exploration).
--
-- Uses the graph DSL implementation with Haiku for symbol selection.
-- Supports training data capture via tidepool-teaching when TEACHING_ENABLED=true.
handleTeachGraphTool :: Logger -> LSPSession -> Text -> Value -> IO ControlResponse
handleTeachGraphTool logger lspSession reqId args = do
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
          runWithTeaching logger config lspSession reqId query

        Nothing -> do
          logInfo logger "  mode=production (no recording)"
          runWithoutTeaching logger lspSession reqId query


-- | Run graph-based exploration with teaching enabled.
--
-- Records all LLM turns to anthropic.jsonl for training data.
runWithTeaching
  :: Logger
  -> TeachingConfig
  -> LSPSession
  -> Text
  -> TeachQuery
  -> IO ControlResponse
runWithTeaching logger config lspSession reqId query = do
  let guidance = teacherGuidance @ScoutGemmaEffect
  resultOrErr <- try $ withTeaching config guidance $ \env -> do
    runM
      $ runLog Debug
      $ runLSP lspSession
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
runWithoutTeaching :: Logger -> LSPSession -> Text -> TeachQuery -> IO ControlResponse
runWithoutTeaching logger _lspSession reqId _query = do
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
handleFindCallersTool :: Logger -> LSPSession -> Text -> Value -> IO ControlResponse
handleFindCallersTool logger lspSession reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId $ "Invalid find_callers arguments: " <> T.pack err

    Success fcArgs -> do
      logDebug logger $ "  name=" <> fcaName fcArgs

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runLSP lspSession
        $ fmap unwrapSingleChoice (findCallersLogic fcArgs)

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
handleShowFieldsTool :: Logger -> LSPSession -> Text -> Value -> IO ControlResponse
handleShowFieldsTool logger lspSession reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId $ "Invalid show_fields arguments: " <> T.pack err

    Success sfArgs -> do
      logDebug logger $ "  type_name=" <> sfaTypeName sfArgs

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runLSP lspSession
        $ fmap unwrapSingleChoice (showFieldsLogic sfArgs)

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
handleShowConstructorsTool :: Logger -> LSPSession -> Text -> Value -> IO ControlResponse
handleShowConstructorsTool logger lspSession reqId args = do
  case fromJSON args of
    Error err -> do
      logError logger $ "  parse error: " <> T.pack err
      pure $ mcpToolError reqId $ "Invalid show_constructors arguments: " <> T.pack err

    Success scArgs -> do
      logDebug logger $ "  type_name=" <> scaTypeName scArgs

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runLSP lspSession
        $ fmap unwrapSingleChoice (showConstructorsLogic scArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          logError logger $ "[MCP:" <> reqId <> "] Error: " <> T.pack (displayException e)
          pure $ mcpToolError reqId $ "show_constructors failed: " <> T.pack (displayException e)

        Right result -> do
          logInfo logger $ "[MCP:" <> reqId <> "] Found " <> T.pack (show $ length $ scrConstructors result) <> " constructors"
          pure $ mcpToolSuccess reqId (toJSON result)
