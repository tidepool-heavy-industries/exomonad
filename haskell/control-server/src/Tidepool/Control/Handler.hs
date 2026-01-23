-- | Message routing for control server.
module Tidepool.Control.Handler
  ( handleMessage
  ) where

import qualified Data.Text as T

import Tidepool.Control.Logging (Logger, logInfo)
import Tidepool.Control.Protocol
import Tidepool.Control.Types (ServerConfig(..))
import Tidepool.Control.TUIState (TUIState)
import Tidepool.Control.Export (exportMCPTools)
import Tidepool.Control.Handler.Hook (handleHook)
import Tidepool.Control.Handler.MCP (handleMcpTool)
import Tidepool.Observability.Types (TraceContext)

-- | Route a control message to the appropriate handler.
handleMessage :: Logger -> ServerConfig -> TraceContext -> TUIState -> ControlMessage -> IO ControlResponse
handleMessage logger config traceCtx tuiState = \case
  HookEvent input r rl -> handleHook input r rl
  McpToolCall reqId name args ->
    handleMcpTool logger config traceCtx tuiState reqId name args
  ToolsListRequest -> handleToolsList logger
  Ping -> pure Pong

-- | Handle tool discovery request.
handleToolsList :: Logger -> IO ControlResponse
handleToolsList logger = do
  logInfo logger "[MCP] Handling ToolsListRequest"
  tools <- exportMCPTools logger
  logInfo logger $ "[MCP] Returning " <> T.pack (show (length tools)) <> " tools"
  pure $ ToolsListResponse tools
