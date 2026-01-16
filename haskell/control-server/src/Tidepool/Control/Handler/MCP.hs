-- | MCP tool call handler.
--
-- Initial implementation: stub (no tools available).
-- This will be extended to expose Tidepool agents as MCP tools.
module Tidepool.Control.Handler.MCP
  ( handleMcpTool
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)

import Tidepool.Control.Protocol

-- | Handle an MCP tool call.
--
-- Current behavior: return error (no tools available).
-- TODO: Wire to Tidepool agents to expose as MCP tools.
handleMcpTool :: Text -> Text -> Value -> IO ControlResponse
handleMcpTool reqId toolName _args = do
  TIO.putStrLn $ "  tool=" <> toolName
  TIO.putStrLn $ "  (no tools available - stub implementation)"
  hFlush stdout

  pure $ mcpToolError reqId $
    "Tool not found: " <> toolName <> ". No tools are currently available."
