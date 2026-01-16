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
import System.IO (hFlush, stdout)

import Tidepool.Control.Protocol
import Tidepool.Control.Scout.Types (ScoutQuery(..), ScoutResponse(..))
import Tidepool.Control.Scout.Explore (exploreEff, defaultExploreConfig)
import Tidepool.Control.Scout.Gemma (runGemmaHeuristic)
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

      -- Run exploration with LSP + heuristic Gemma
      -- Note: Uses heuristic scorer for now; real FunctionGemma coming later
      -- Wrap in try to catch any LSP or exploration errors
      resultOrErr <- try $ runM $ runGemmaHeuristic $ runLSP lspSession $
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
