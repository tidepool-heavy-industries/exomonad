-- | MCP tool call handler.
--
-- Exposes semantic code exploration and teaching tools via LSP + LLM.
--
-- = Available Tools
--
-- - **teach**: Generate teaching documents using BFS + Ollama (legacy)
-- - **teach-graph**: Generate teaching documents using graph DSL + Haiku (new)
--
-- The teach-graph tool supports training data capture via tidepool-teaching.
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
import Tidepool.Control.Scout.DocGen (scout, defaultTeachConfig, TeachQuery(..), TeachingDoc(..))
import Tidepool.Control.Scout.DocGen.Gemma (runScoutGemmaHTTP)
import Tidepool.Control.Scout.DocGen.Teacher (ScoutGemmaEffect)
import Tidepool.Control.Scout.Graph.Runner (runDocGenGraph)
import Tidepool.Effect.NodeMeta (runNodeMeta, runGraphMeta, defaultNodeMeta, GraphMetadata(..))
import Tidepool.Effect.Types (runLog, LogLevel(..))
import Tidepool.LSP.Interpreter (LSPSession, runLSP)
import Tidepool.Teaching.LLM (TeachingConfig, loadTeachingConfig, withTeaching, runLLMWithTeaching)
import Tidepool.Teaching.Teacher (teacherGuidance)

-- | Handle an MCP tool call.
--
-- Currently supports:
--   - "teach": Generate teaching documents via BFS + Ollama (legacy)
--   - "teach-graph": Generate teaching documents via graph DSL + Haiku (new)
handleMcpTool :: LSPSession -> Text -> Text -> Value -> IO ControlResponse
handleMcpTool lspSession reqId toolName args = do
  TIO.putStrLn $ "  tool=" <> toolName
  hFlush stdout

  case toolName of
    "teach" -> handleTeachTool lspSession reqId args
    "teach-graph" -> handleTeachGraphTool lspSession reqId args
    _ -> do
      TIO.putStrLn $ "  (unknown tool)"
      hFlush stdout
      pure $ mcpToolError reqId $
        "Tool not found: " <> toolName <> ". Available tools: teach, teach-graph"


-- | Handle the teach tool.
--
-- Generates teaching documents from LSP + Gemma that explain a topic
-- in prerequisite order. Uses Ollama/FunctionGemma for scoring.
handleTeachTool :: LSPSession -> Text -> Value -> IO ControlResponse
handleTeachTool lspSession reqId args = do
  case fromJSON args of
    Error err -> do
      TIO.putStrLn $ "  parse error: " <> T.pack err
      hFlush stdout
      pure $ mcpToolError reqId $ "Invalid teach arguments: " <> T.pack err

    Success query -> do
      TIO.putStrLn $ "  topic=" <> tqTopic query
      TIO.putStrLn $ "  seeds=" <> T.intercalate ", " (tqSeeds query)
      TIO.putStrLn $ "  budget=" <> T.pack (show $ tqBudget query)
      hFlush stdout

      -- Require GEMMA_ENDPOINT
      maybeEndpoint <- lookupEnv "GEMMA_ENDPOINT"
      case maybeEndpoint of
        Nothing -> do
          TIO.putStrLn "  error: GEMMA_ENDPOINT not set"
          hFlush stdout
          pure $ mcpToolError reqId "GEMMA_ENDPOINT environment variable not set"

        Just ep -> do
          TIO.putStrLn $ "  gemma=" <> T.pack ep
          hFlush stdout

          -- Production mode: use Ollama/FunctionGemma
          resultOrErr <- try $ runM $ runLog Debug $
            runScoutGemmaHTTP (T.pack ep) $
            runLSP lspSession $
            scout defaultTeachConfig query

          case resultOrErr of
            Left (e :: SomeException) -> do
              TIO.putStrLn $ "  error: " <> T.pack (show e)
              hFlush stdout
              pure $ mcpToolError reqId $ "Teach exploration failed: " <> T.pack (show e)

            Right doc -> do
              let totalUnits = length (tdPrereqs doc) + length (tdCore doc) + length (tdSupport doc)
              TIO.putStrLn $ "  generated doc with " <> T.pack (show totalUnits) <> " teaching units"
              hFlush stdout
              pure $ mcpToolSuccess reqId (toJSON doc)


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
