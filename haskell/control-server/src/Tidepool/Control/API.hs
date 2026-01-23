{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Tidepool.Control.API
  ( TidepoolControlAPI
  , McpToolCallRequest(..)
  ) where

import Data.Text (Text)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), object, (.=), (.:), withObject)
import GHC.Generics (Generic)
import Servant.API
import Servant.API.WebSocket (WebSocket)
import Tidepool.Control.Protocol

-- | Request for MCP tool call.
data McpToolCallRequest = McpToolCallRequest
  { mcpId :: Text
  , toolName :: Text
  , arguments :: Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON McpToolCallRequest where
  parseJSON = withObject "McpToolCallRequest" $ \o -> McpToolCallRequest
    <$> o .: "id"
    <*> o .: "tool_name"
    <*> o .: "arguments"

instance ToJSON McpToolCallRequest where
  toJSON r = object
    [ "id" .= r.mcpId
    , "tool_name" .= r.toolName
    , "arguments" .= r.arguments
    ]

type TidepoolControlAPI =
       -- | Hook event: (Input, Runtime, Role) -> (Output, ExitCode)
       "hook" :> ReqBody '[JSON] (HookInput, Runtime, Role) :> Post '[JSON] (HookOutput, Int)
       -- | MCP tool call: Request -> Response
  :<|> "mcp" :> "call" :> ReqBody '[JSON] McpToolCallRequest :> Post '[JSON] ControlResponse
       -- | MCP tool list
  :<|> "mcp" :> "tools" :> Get '[JSON] [ToolDefinition]
       -- | TUI WebSocket: bidirectional popup communication
  :<|> "tui" :> "ws" :> WebSocket
       -- | Health check
  :<|> "ping" :> Get '[JSON] Text
       -- | Role-based MCP tool list
  :<|> "role" :> Capture "slug" Text :> "mcp" :> "tools" :> Header "Mcp-Session-Id" Text :> Get '[JSON] [ToolDefinition]
       -- | Role-based MCP tool call
  :<|> "role" :> Capture "slug" Text :> "mcp" :> "call" :> Header "Mcp-Session-Id" Text :> ReqBody '[JSON] McpToolCallRequest :> Post '[JSON] ControlResponse
