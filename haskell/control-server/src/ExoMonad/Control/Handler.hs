-- | Message routing for control server.
module ExoMonad.Control.Handler
  ( handleMessage
  ) where

import qualified Data.Text as T

import OpenTelemetry.Trace (Tracer)
import ExoMonad.Control.Logging (Logger, logInfo)
import ExoMonad.Control.Protocol
import ExoMonad.Control.Types (ServerConfig(..))
import ExoMonad.Control.Hook.CircuitBreaker (CircuitBreakerMap)
import ExoMonad.Control.Export (exportMCPTools)
import ExoMonad.Control.Handler.Hook (handleHook)
import ExoMonad.Control.Handler.MCP (handleMcpTool)
import ExoMonad.Observability.Types (TraceContext)

-- | Route a control message to the appropriate handler.
handleMessage :: Logger -> ServerConfig -> Tracer -> TraceContext -> CircuitBreakerMap -> ControlMessage -> IO ControlResponse
handleMessage logger config tracer traceCtx cbMap = \case
  HookEvent input r rl -> handleHook tracer config input r rl cbMap
  McpToolCall reqId name args ->
    handleMcpTool logger config traceCtx reqId name args
  ToolsListRequest -> handleToolsList logger
  Ping -> pure Pong

-- | Handle tool discovery request.
handleToolsList :: Logger -> IO ControlResponse
handleToolsList logger = do
  logInfo logger "[MCP] Handling ToolsListRequest"
  tools <- exportMCPTools logger
  logInfo logger $ "[MCP] Returning " <> T.pack (show (length tools)) <> " tools"
  pure $ ToolsListResponse tools
