{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Tidepool.Control.API
  ( TidepoolControlAPI
  , McpToolCallRequest(..)
  ) where

import Data.Text (Text)
import Data.Aeson (Value, FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant.API
import Tidepool.Control.Protocol
import Tidepool.Effect.TUI (PopupDefinition, PopupResult)

-- | Request for MCP tool call.
data McpToolCallRequest = McpToolCallRequest
  { mcpId :: Text
  , toolName :: Text
  , arguments :: Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

type TidepoolControlAPI =
       -- | Hook event: (Input, Runtime) -> (Output, ExitCode)
       "hook" :> ReqBody '[JSON] (HookInput, Runtime) :> Post '[JSON] (HookOutput, Int)
       -- | MCP tool call: Request -> Response
  :<|> "mcp" :> "call" :> ReqBody '[JSON] McpToolCallRequest :> Post '[JSON] ControlResponse
       -- | MCP tool list
  :<|> "mcp" :> "tools" :> Get '[JSON] [ToolDefinition]
       -- | TUI spawn: Definition -> Result (Interaction)
  :<|> "tui" :> "spawn" :> ReqBody '[JSON] PopupDefinition :> Post '[JSON] PopupResult
       -- | Health check
  :<|> "ping" :> Get '[JSON] Text
