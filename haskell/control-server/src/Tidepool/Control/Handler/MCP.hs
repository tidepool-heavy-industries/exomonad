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

import Control.Exception (SomeException, try)
import Control.Monad.Freer (runM)
import Data.Aeson (Value, fromJSON, toJSON, Result(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)

import Tidepool.Control.Protocol
import Tidepool.Control.Scout.DocGen (TeachQuery(..), TeachingDoc(..))
import Tidepool.Control.Scout.DocGen.Teacher (ScoutGemmaEffect)
import Tidepool.Control.Scout.Graph.Runner (runDocGenGraph)
import Tidepool.Control.LSPTools
  ( findCallersLogic, FindCallersArgs(..), FindCallersResult(..)
  , showFieldsLogic, ShowFieldsArgs(..), ShowFieldsResult(..)
  , showConstructorsLogic, ShowConstructorsArgs(..), ShowConstructorsResult(..)
  )
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
handleMcpTool :: LSPSession -> Text -> Text -> Value -> IO ControlResponse
handleMcpTool lspSession reqId toolName args = do
  TIO.putStrLn $ "  tool=" <> toolName
  hFlush stdout

  case toolName of
    -- Tier 1: Deterministic LSP tools (graph-based)
    "find_callers" -> handleFindCallersTool lspSession reqId args
    "show_fields" -> handleShowFieldsTool lspSession reqId args
    "show_constructors" -> handleShowConstructorsTool lspSession reqId args

    -- Tier 2: LLM-enhanced tools (graph-based)
    "teach-graph" -> handleTeachGraphTool lspSession reqId args

    _ -> do
      TIO.putStrLn $ "  (unknown tool)"
      hFlush stdout
      pure $ mcpToolError reqId $
        "Tool not found: " <> toolName <>
        ". Available tools: find_callers, show_fields, show_constructors, teach-graph"


-- | Handle the teach-graph tool (graph-based exploration).
--
-- Uses the graph DSL implementation with Haiku for symbol selection.
-- Supports training data capture via tidepool-teaching when TEACHING_ENABLED=true.
handleTeachGraphTool :: LSPSession -> Text -> Value -> IO ControlResponse
handleTeachGraphTool lspSession reqId args = do
  case fromJSON args of
    Error err -> do
      TIO.putStrLn $ "  parse error: " <> T.pack err
      hFlush stdout
      pure $ mcpToolError reqId $ "Invalid teach-graph arguments: " <> T.pack err

    Success query -> do
      TIO.putStrLn $ "  topic=" <> tqTopic query
      TIO.putStrLn $ "  seeds=" <> T.intercalate ", " (tqSeeds query)
      TIO.putStrLn $ "  budget=" <> T.pack (show $ tqBudget query)
      hFlush stdout

      -- Check for teaching mode
      maybeConfig <- loadTeachingConfig
      case maybeConfig of
        Just config -> do
          TIO.putStrLn "  mode=teaching (recording to JSONL)"
          hFlush stdout
          runWithTeaching config lspSession reqId query

        Nothing -> do
          TIO.putStrLn "  mode=production (no recording)"
          hFlush stdout
          runWithoutTeaching lspSession reqId query


-- | Run graph-based exploration with teaching enabled.
--
-- Records all LLM turns to anthropic.jsonl for training data.
runWithTeaching
  :: TeachingConfig
  -> LSPSession
  -> Text
  -> TeachQuery
  -> IO ControlResponse
runWithTeaching config lspSession reqId query = do
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
      TIO.putStrLn $ "  error: " <> T.pack (show e)
      hFlush stdout
      pure $ mcpToolError reqId $ "Teach-graph exploration failed: " <> T.pack (show e)

    Right doc -> do
      let totalUnits = length (tdPrereqs doc) + length (tdCore doc) + length (tdSupport doc)
      TIO.putStrLn $ "  generated doc with " <> T.pack (show totalUnits) <> " teaching units"
      TIO.putStrLn "  training data recorded"
      hFlush stdout
      pure $ mcpToolSuccess reqId (toJSON doc)


-- | Run graph-based exploration without teaching.
--
-- Uses production LLM interpreter (requires ANTHROPIC_API_KEY).
runWithoutTeaching :: LSPSession -> Text -> TeachQuery -> IO ControlResponse
runWithoutTeaching _lspSession reqId _query = do
  -- Check for ANTHROPIC_API_KEY
  maybeKey <- lookupEnv "ANTHROPIC_API_KEY"
  case maybeKey of
    Nothing -> do
      TIO.putStrLn "  error: ANTHROPIC_API_KEY not set"
      hFlush stdout
      pure $ mcpToolError reqId
        "ANTHROPIC_API_KEY environment variable not set. Either set it or enable teaching mode."

    Just _key -> do
      -- TODO: Use production LLM interpreter
      -- For now, return an error explaining the limitation
      TIO.putStrLn "  error: Production mode not yet implemented"
      hFlush stdout
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
handleFindCallersTool :: LSPSession -> Text -> Value -> IO ControlResponse
handleFindCallersTool lspSession reqId args = do
  case fromJSON args of
    Error err -> do
      TIO.putStrLn $ "  parse error: " <> T.pack err
      hFlush stdout
      pure $ mcpToolError reqId $ "Invalid find_callers arguments: " <> T.pack err

    Success fcArgs -> do
      TIO.putStrLn $ "  name=" <> fcaName fcArgs
      hFlush stdout

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runLSP lspSession
        $ fmap unwrapSingleChoice (findCallersLogic fcArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          TIO.putStrLn $ "  error: " <> T.pack (show e)
          hFlush stdout
          pure $ mcpToolError reqId $ "find_callers failed: " <> T.pack (show e)

        Right result -> do
          TIO.putStrLn $ "  found " <> T.pack (show $ length $ fcrCallSites result) <> " call sites"
          TIO.putStrLn $ "  filtered " <> T.pack (show $ fcrFilteredCount result) <> " references"
          hFlush stdout
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the show_fields tool.
--
-- Runs the ShowFieldsGraph logic to show fields of a Haskell record type.
handleShowFieldsTool :: LSPSession -> Text -> Value -> IO ControlResponse
handleShowFieldsTool lspSession reqId args = do
  case fromJSON args of
    Error err -> do
      TIO.putStrLn $ "  parse error: " <> T.pack err
      hFlush stdout
      pure $ mcpToolError reqId $ "Invalid show_fields arguments: " <> T.pack err

    Success sfArgs -> do
      TIO.putStrLn $ "  type_name=" <> sfaTypeName sfArgs
      hFlush stdout

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runLSP lspSession
        $ fmap unwrapSingleChoice (showFieldsLogic sfArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          TIO.putStrLn $ "  error: " <> T.pack (show e)
          hFlush stdout
          pure $ mcpToolError reqId $ "show_fields failed: " <> T.pack (show e)

        Right result -> do
          TIO.putStrLn $ "  found " <> T.pack (show $ length $ sfrFields result) <> " fields"
          hFlush stdout
          pure $ mcpToolSuccess reqId (toJSON result)


-- | Handle the show_constructors tool.
--
-- Runs the ShowConstructorsGraph logic to show constructors of a Haskell sum type or GADT.
handleShowConstructorsTool :: LSPSession -> Text -> Value -> IO ControlResponse
handleShowConstructorsTool lspSession reqId args = do
  case fromJSON args of
    Error err -> do
      TIO.putStrLn $ "  parse error: " <> T.pack err
      hFlush stdout
      pure $ mcpToolError reqId $ "Invalid show_constructors arguments: " <> T.pack err

    Success scArgs -> do
      TIO.putStrLn $ "  type_name=" <> scaTypeName scArgs
      hFlush stdout

      resultOrErr <- try $ runM
        $ runLog Debug
        $ runLSP lspSession
        $ fmap unwrapSingleChoice (showConstructorsLogic scArgs)

      case resultOrErr of
        Left (e :: SomeException) -> do
          TIO.putStrLn $ "  error: " <> T.pack (show e)
          hFlush stdout
          pure $ mcpToolError reqId $ "show_constructors failed: " <> T.pack (show e)

        Right result -> do
          TIO.putStrLn $ "  found " <> T.pack (show $ length $ scrConstructors result) <> " constructors"
          hFlush stdout
          pure $ mcpToolSuccess reqId (toJSON result)
