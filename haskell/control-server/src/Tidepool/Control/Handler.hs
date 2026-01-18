-- | Message routing for control server.
module Tidepool.Control.Handler
  ( handleMessage
  ) where

import Tidepool.Control.Protocol
import Tidepool.Control.Export (exportMCPTools)
import Tidepool.Control.Handler.Hook (handleHook)
import Tidepool.Control.Handler.MCP (handleMcpTool)
import Tidepool.LSP.Interpreter (LSPSession)

-- | Route a control message to the appropriate handler.
handleMessage :: LSPSession -> ControlMessage -> IO ControlResponse
handleMessage lspSession = \case
  HookEvent input -> handleHook input
  McpToolCall reqId name args -> handleMcpTool lspSession reqId name args
  ToolsListRequest -> handleToolsList

-- | Handle tool discovery request.
handleToolsList :: IO ControlResponse
handleToolsList = do
  tools <- exportMCPTools
  pure $ ToolsListResponse tools
