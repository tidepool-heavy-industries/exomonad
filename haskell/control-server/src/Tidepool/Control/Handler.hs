-- | Message routing for control server.
module Tidepool.Control.Handler
  ( handleMessage
  ) where

import Tidepool.Control.Protocol
import Tidepool.Control.Handler.Hook (handleHook)
import Tidepool.Control.Handler.MCP (handleMcpTool)

-- | Route a control message to the appropriate handler.
handleMessage :: ControlMessage -> IO ControlResponse
handleMessage = \case
  HookEvent input -> handleHook input
  McpToolCall reqId name args -> handleMcpTool reqId name args
