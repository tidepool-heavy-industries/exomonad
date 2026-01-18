-- | MCP tool call handler.
--
-- Exposes semantic code exploration and teaching tools via LSP + FunctionGemma.
module Tidepool.Control.Handler.MCP
  ( handleMcpTool
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Freer (runM)
import Data.Aeson (Value, fromJSON, toJSON, Result(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.UUID.V4 as UUID
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)

import Tidepool.Control.Protocol
import Tidepool.Control.Types (TeachingSettings(..))
import Tidepool.Control.Scout.Teach (teach, defaultTeachConfig, TeachQuery(..), TeachingDoc(..))
import Tidepool.Control.Scout.Teach.Gemma (runTeachGemmaHTTP)
import Tidepool.Control.Scout.Teach.Teaching (runTeachGemmaWithTeaching)
import Tidepool.Effect.Types (runLog, LogLevel(..))
import Tidepool.LSP.Interpreter (LSPSession, runLSP)

-- | Handle an MCP tool call.
--
-- Currently supports:
--   - "teach": Generate teaching documents in prerequisite order
handleMcpTool :: LSPSession -> Maybe TeachingSettings -> Text -> Text -> Value -> IO ControlResponse
handleMcpTool lspSession maybeTeachSettings reqId toolName args = do
  TIO.putStrLn $ "  tool=" <> toolName
  hFlush stdout

  case toolName of
    "teach" -> handleTeachTool lspSession maybeTeachSettings reqId args
    _ -> do
      TIO.putStrLn $ "  (unknown tool)"
      hFlush stdout
      pure $ mcpToolError reqId $
        "Tool not found: " <> toolName <> ". Available tools: teach"


-- | Handle the teach tool.
--
-- Generates teaching documents from LSP + Gemma that explain a topic
-- in prerequisite order.
--
-- When teaching mode is enabled, uses Haiku and records training data.
handleTeachTool :: LSPSession -> Maybe TeachingSettings -> Text -> Value -> IO ControlResponse
handleTeachTool lspSession maybeTeachSettings reqId args = do
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

          -- Choose interpreter (SINGLE decision point)
          resultOrErr <- case maybeTeachSettings of
            Nothing -> do
              -- Production mode: use Ollama/FunctionGemma
              TIO.putStrLn "  mode=production (Ollama)"
              hFlush stdout
              try $ runM $ runLog Debug $
                runTeachGemmaHTTP (T.pack ep) $
                runLSP lspSession $
                teach defaultTeachConfig query

            Just settings -> do
              -- Teaching mode: use Haiku and record training data
              TIO.putStrLn "  mode=teaching (Haiku + recording)"
              TIO.putStrLn $ "  output=" <> T.pack (tsOutputDir settings)
              hFlush stdout
              try $ runM $ runLog Debug $
                runTeachGemmaWithTeaching
                  (tsOutputDir settings)
                  (tsAnthropicKey settings) $
                runLSP lspSession $
                teach defaultTeachConfig query

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
