-- | MCP tool call handler.
--
-- Exposes the semantic scout tool for code exploration via LSP + FunctionGemma.
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
import Tidepool.Control.Scout.Types (ScoutQuery(..), ScoutResponse(..))
import Tidepool.Control.Scout.Explore (exploreEff, defaultExploreConfig)
import Tidepool.Control.Scout.Gemma (runGemmaHTTP)
import Tidepool.LSP.Interpreter (LSPSession, runLSP)

-- | Handle an MCP tool call.
--
-- Currently supports:
--   - "scout": Semantic code exploration using LSP + FunctionGemma
handleMcpTool :: LSPSession -> Text -> Text -> Value -> IO ControlResponse
handleMcpTool lspSession reqId toolName args = do
  TIO.putStrLn $ "  tool=" <> toolName
  hFlush stdout

  case toolName of
    "scout" -> handleScoutTool lspSession reqId args
    _ -> do
      TIO.putStrLn $ "  (unknown tool)"
      hFlush stdout
      pure $ mcpToolError reqId $
        "Tool not found: " <> toolName <> ". Available tools: scout"


-- | Handle the scout tool.
--
-- Runs semantic code exploration using:
--   - LSP for code navigation (hover, references, workspace symbols)
--   - FunctionGemma (heuristic fallback) for relevance scoring
handleScoutTool :: LSPSession -> Text -> Value -> IO ControlResponse
handleScoutTool lspSession reqId args = do
  case fromJSON args of
    Error err -> do
      TIO.putStrLn $ "  parse error: " <> T.pack err
      hFlush stdout
      pure $ mcpToolError reqId $ "Invalid scout arguments: " <> T.pack err

    Success query -> do
      TIO.putStrLn $ "  query=" <> sqQuery query
      TIO.putStrLn $ "  symbols=" <> T.intercalate ", " (sqSymbols query)
      TIO.putStrLn $ "  depth=" <> T.pack (show $ sqDepth query)
      hFlush stdout

      -- Require GEMMA_ENDPOINT - no fallback, fail if not set or if inference fails
      maybeEndpoint <- lookupEnv "GEMMA_ENDPOINT"
      case maybeEndpoint of
        Nothing -> do
          TIO.putStrLn "  error: GEMMA_ENDPOINT not set"
          hFlush stdout
          pure $ mcpToolError reqId "GEMMA_ENDPOINT environment variable not set"

        Just ep -> do
          TIO.putStrLn $ "  gemma=" <> T.pack ep
          hFlush stdout

          -- No try/catch - let HTTP and parsing errors propagate
          resultOrErr <- try $ runM $ runGemmaHTTP (T.pack ep) $ runLSP lspSession $
            exploreEff defaultExploreConfig query

          case resultOrErr of
            Left (e :: SomeException) -> do
              TIO.putStrLn $ "  error: " <> T.pack (show e)
              hFlush stdout
              pure $ mcpToolError reqId $ "Scout exploration failed: " <> T.pack (show e)

            Right result -> do
              TIO.putStrLn $ "  found " <> T.pack (show $ srNodesVisited result) <> " locations"
              hFlush stdout
              pure $ mcpToolSuccess reqId (toJSON result)
