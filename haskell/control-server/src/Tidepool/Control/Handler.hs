-- | Message routing for control server.
module Tidepool.Control.Handler
  ( handleMessage
  ) where

import Tidepool.Control.Protocol
import Tidepool.Control.Types (TeachingSettings)
import Tidepool.Control.Handler.Hook (handleHook)
import Tidepool.Control.Handler.MCP (handleMcpTool)
import Tidepool.LSP.Interpreter (LSPSession)

-- | Route a control message to the appropriate handler.
handleMessage :: LSPSession -> Maybe TeachingSettings -> ControlMessage -> IO ControlResponse
handleMessage lspSession maybeTeachSettings = \case
  HookEvent input -> handleHook input
  McpToolCall reqId name args -> handleMcpTool lspSession maybeTeachSettings reqId name args
