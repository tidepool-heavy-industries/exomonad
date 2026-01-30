{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.API
  ( ExoMonadControlAPI,
    McpToolCallRequest (..),
    McpJsonRpcRequest (..),
    McpJsonRpcResponse (..),
    McpJsonRpcError (..),
    McpInitializeParams (..),
    McpInitializeResult (..),
    McpServerCapabilities (..),
    McpServerInfo (..),
    McpToolsListResult (..),
    McpToolCallParams (..),
    McpToolCallResult (..),
    McpContentItem (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import ExoMonad.Control.Protocol
import GHC.Generics (Generic)
import Servant.API

-- | Request for MCP tool call.
data McpToolCallRequest = McpToolCallRequest
  { mcpId :: Text,
    toolName :: Text,
    arguments :: Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON McpToolCallRequest where
  parseJSON = withObject "McpToolCallRequest" $ \o ->
    McpToolCallRequest
      <$> o .: "id"
      <*> o .: "tool_name"
      <*> o .: "arguments"

instance ToJSON McpToolCallRequest where
  toJSON r =
    object
      [ "id" .= r.mcpId,
        "tool_name" .= r.toolName,
        "arguments" .= r.arguments
      ]

-- | MCP JSON-RPC 2.0 request envelope.
-- Used by Claude Code's HTTP MCP transport.
data McpJsonRpcRequest = McpJsonRpcRequest
  { jrpcId :: Maybe Value, -- Nothing for notifications
    jrpcMethod :: Text,
    jrpcParams :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON McpJsonRpcRequest where
  parseJSON = withObject "McpJsonRpcRequest" $ \o ->
    McpJsonRpcRequest
      <$> o .:? "id"
      <*> o .: "method"
      <*> o .:? "params"

-- | MCP JSON-RPC 2.0 response envelope.
data McpJsonRpcResponse = McpJsonRpcResponse
  { jrpcRespId :: Maybe Value,
    jrpcResult :: Maybe Value,
    jrpcError :: Maybe McpJsonRpcError
  }
  deriving stock (Show, Eq, Generic)

data McpJsonRpcError = McpJsonRpcError
  { jrpcErrCode :: Int,
    jrpcErrMessage :: Text,
    jrpcErrData :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON McpJsonRpcResponse where
  toJSON r =
    object $
      [ "jsonrpc" .= ("2.0" :: Text),
        "id" .= r.jrpcRespId
      ]
        ++ maybe [] (\res -> ["result" .= res]) r.jrpcResult
        ++ maybe [] (\err -> ["error" .= err]) r.jrpcError

instance ToJSON McpJsonRpcError where
  toJSON e =
    object
      [ "code" .= e.jrpcErrCode,
        "message" .= e.jrpcErrMessage,
        "data" .= e.jrpcErrData
      ]

-- | Parameters for MCP initialize request.
data McpInitializeParams = McpInitializeParams
  { mipProtocolVersion :: Text,
    mipCapabilities :: Value,
    mipClientInfo :: Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON McpInitializeParams where
  parseJSON = withObject "McpInitializeParams" $ \o ->
    McpInitializeParams
      <$> o .: "protocolVersion"
      <*> o .: "capabilities"
      <*> o .: "clientInfo"

-- | Result for MCP initialize response.
data McpInitializeResult = McpInitializeResult
  { mirProtocolVersion :: Text,
    mirCapabilities :: McpServerCapabilities,
    mirServerInfo :: McpServerInfo
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON McpInitializeResult where
  toJSON r =
    object
      [ "protocolVersion" .= r.mirProtocolVersion,
        "capabilities" .= r.mirCapabilities,
        "serverInfo" .= r.mirServerInfo
      ]

-- | Server capabilities for MCP.
data McpServerCapabilities = McpServerCapabilities
  { mscTools :: Maybe Value -- e.g., {"listChanged": true}
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON McpServerCapabilities where
  toJSON c =
    object $
      maybe [] (\t -> ["tools" .= t]) c.mscTools

-- | Server info for MCP.
data McpServerInfo = McpServerInfo
  { msiName :: Text,
    msiVersion :: Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON McpServerInfo where
  toJSON i =
    object
      [ "name" .= i.msiName,
        "version" .= i.msiVersion
      ]

-- | Result for tools/list.
data McpToolsListResult = McpToolsListResult
  { mtlrTools :: [ToolDefinition]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON McpToolsListResult where
  toJSON r = object ["tools" .= r.mtlrTools]

-- | Parameters for tools/call.
data McpToolCallParams = McpToolCallParams
  { mtcpName :: Text,
    mtcpArguments :: Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON McpToolCallParams where
  parseJSON = withObject "McpToolCallParams" $ \o ->
    McpToolCallParams
      <$> o .: "name"
      <*> o .: "arguments"

-- | Result for tools/call.
data McpToolCallResult = McpToolCallResult
  { mtcrContent :: [McpContentItem],
    mtcrIsError :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON McpToolCallResult where
  toJSON r =
    object
      [ "content" .= r.mtcrContent,
        "isError" .= r.mtcrIsError
      ]

-- | Content item in tool call result.
data McpContentItem = McpContentItem
  { mciType :: Text, -- "text" or "image"
    mciText :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON McpContentItem where
  toJSON c =
    object $
      [ "type" .= c.mciType
      ]
        ++ maybe [] (\t -> ["text" .= t]) c.mciText

type ExoMonadControlAPI =
  -- | Hook event: (Input, Runtime, Role, ContainerId) -> (Output, ExitCode)
  "hook" :> ReqBody '[JSON] (HookInput, Runtime, Role, Maybe Text) :> Post '[JSON] ControlResponse
    -- \| MCP tool call: Request -> Response
    :<|> "mcp" :> "call" :> ReqBody '[JSON] McpToolCallRequest :> Post '[JSON] ControlResponse
    -- \| MCP tool list
    :<|> "mcp" :> "tools" :> Get '[JSON] [ToolDefinition]
    -- \| Health check
    :<|> "ping" :> Get '[JSON] Text
    -- \| Role-based MCP tool list (legacy REST endpoint)
    :<|> "role" :> Capture "slug" Text :> "mcp" :> "tools" :> Header "Mcp-Session-Id" Text :> Get '[JSON] [ToolDefinition]
    -- \| Role-based MCP tool call (legacy REST endpoint)
    :<|> "role" :> Capture "slug" Text :> "mcp" :> "call" :> Header "Mcp-Session-Id" Text :> ReqBody '[JSON] McpToolCallRequest :> Post '[JSON] ControlResponse
    -- \| Unified MCP JSON-RPC endpoint (for Claude Code HTTP transport)
    -- Handles: initialize, notifications/initialized, tools/list, tools/call, ping
    :<|> "role" :> Capture "slug" Text :> "mcp" :> Header "Mcp-Session-Id" Text :> ReqBody '[JSON] McpJsonRpcRequest :> Post '[JSON] McpJsonRpcResponse
    -- \| Agent status dashboard
    :<|> "api" :> "agents" :> Get '[JSON] AgentsResponse
    -- \| Agent logs
    :<|> "api" :> "agents" :> Capture "id" Text :> "logs" :> Get '[JSON] Text
    -- \| Stop agent
    :<|> "api" :> "agents" :> Capture "id" Text :> "stop" :> Post '[JSON] ()
