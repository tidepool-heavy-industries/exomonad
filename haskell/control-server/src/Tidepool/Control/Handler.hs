-- | Message routing for control server.
module Tidepool.Control.Handler
  ( handleMessage
  ) where

import Tidepool.Control.Protocol
import Tidepool.Control.Types (TeachingConfig)
import Tidepool.Control.Handler.Hook (handleHook)
import Tidepool.Control.Handler.MCP (handleMcpTool)
import Tidepool.LSP.Interpreter (LSPSession)

-- | Route a control message to the appropriate handler.
handleMessage :: LSPSession -> Maybe TeachingConfig -> ControlMessage -> IO ControlResponse
handleMessage lspSession maybeTeachConfig = \case
  HookEvent input -> handleHook input
  McpToolCall reqId name args -> handleMcpTool lspSession maybeTeachConfig reqId name args
