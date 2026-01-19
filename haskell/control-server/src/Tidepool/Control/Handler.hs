-- | Message routing for control server.
module Tidepool.Control.Handler
  ( handleMessage
  ) where

import qualified Data.Text as T

import Tidepool.Control.Logging (Logger, logInfo)
import Tidepool.Control.Protocol
import Tidepool.Control.Export (exportMCPTools)
import Tidepool.Control.Handler.Hook (handleHook)
import Tidepool.Control.Handler.MCP (handleMcpTool)
import Tidepool.LSP.Interpreter (LSPSession)

-- | Route a control message to the appropriate handler.
handleMessage :: Logger -> LSPSession -> ControlMessage -> IO ControlResponse
handleMessage logger lspSession = \case
  HookEvent input -> handleHook input
  McpToolCall reqId name args -> handleMcpTool logger lspSession reqId name args
  ToolsListRequest -> handleToolsList logger

-- | Handle tool discovery request.
handleToolsList :: Logger -> IO ControlResponse
handleToolsList logger = do
  logInfo logger "[MCP] Handling ToolsListRequest"
  tools <- exportMCPTools logger
  logInfo logger $ "[MCP] Returning " <> T.pack (show (length tools)) <> " tools"
  pure $ ToolsListResponse tools
