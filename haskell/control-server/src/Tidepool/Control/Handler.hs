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
import Tidepool.TUI.Interpreter (TUIHandle)

-- | Route a control message to the appropriate handler.
handleMessage :: Logger -> LSPSession -> Maybe TUIHandle -> ControlMessage -> IO ControlResponse
handleMessage logger lspSession maybeTuiHandle = \case
  HookEvent input r -> handleHook input r
  McpToolCall reqId name args -> handleMcpTool logger lspSession maybeTuiHandle reqId name args
  ToolsListRequest -> handleToolsList logger
  Ping -> pure Pong

-- | Handle tool discovery request.
handleToolsList :: Logger -> IO ControlResponse
handleToolsList logger = do
  logInfo logger "[MCP] Handling ToolsListRequest"
  tools <- exportMCPTools logger
  logInfo logger $ "[MCP] Returning " <> T.pack (show (length tools)) <> " tools"
  pure $ ToolsListResponse tools
