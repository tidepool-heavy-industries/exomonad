-- | Message routing for control server.
module Tidepool.Control.Handler
  ( handleMessage
  ) where

import qualified Data.Text as T

import Tidepool.Control.Logging (Logger, logInfo)
import Tidepool.Control.Protocol
import Tidepool.Control.Types (ServerConfig(..))
import Tidepool.Control.Export (exportMCPTools)
import Tidepool.Control.Handler.Hook (handleHook)
import Tidepool.Control.Handler.MCP (handleMcpTool)
import Tidepool.LSP.Interpreter (LSPSession)
import Tidepool.TUI.Interpreter (TUIHandle)
import Tidepool.Observability.Types (TraceContext)

-- | Route a control message to the appropriate handler.
handleMessage :: Logger -> ServerConfig -> Maybe LSPSession -> Maybe TUIHandle -> TraceContext -> ControlMessage -> IO ControlResponse
handleMessage logger config maybeLspSession maybeTuiHandle traceCtx = \case
  HookEvent input r -> handleHook input r
  McpToolCall reqId name args -> case maybeLspSession of
    Just lspSession -> handleMcpTool logger config lspSession maybeTuiHandle traceCtx reqId name args
    Nothing -> do
      logInfo logger $ "[MCP:" <> reqId <> "] Request received before LSP ready"
      pure $ mcpToolError reqId InvalidInput "Server starting, LSP initializing... please retry in a few seconds."
  ToolsListRequest -> handleToolsList logger
  Ping -> pure Pong

-- | Handle tool discovery request.
handleToolsList :: Logger -> IO ControlResponse
handleToolsList logger = do
  logInfo logger "[MCP] Handling ToolsListRequest"
  tools <- exportMCPTools logger
  logInfo logger $ "[MCP] Returning " <> T.pack (show (length tools)) <> " tools"
  pure $ ToolsListResponse tools
