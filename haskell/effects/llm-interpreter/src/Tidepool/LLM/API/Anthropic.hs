-- | Anthropic Messages API client using servant-client.
--
-- Type-safe HTTP client for the Anthropic Messages API.
--
-- = Usage
--
-- @
-- import Tidepool.LLM.API.Anthropic (anthropicComplete, AnthropicRequest(..))
-- import Servant.Client (runClientM, mkClientEnv)
--
-- env <- mkClientEnv manager (BaseUrl Https "api.anthropic.com" 443 "")
-- result <- runClientM (anthropicComplete apiKey req) env
-- @
module Tidepool.LLM.API.Anthropic
  ( -- * API Type
    AnthropicAPI

    -- * Request Types
  , AnthropicRequest(..)
  , AnthropicMessage(..)
  , ToolChoice(..)
  , ThinkingConfig(..)

    -- * Client Function
  , anthropicComplete

    -- * Re-exports
  , module Tidepool.Effects.LLMProvider
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object, (.=), withObject, (.:), (.:?))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Servant.Client (ClientM, client)

-- Response types from the effect module
import Tidepool.Effects.LLMProvider (AnthropicResponse(..), AnthropicConfig(..))


-- ════════════════════════════════════════════════════════════════════════════
-- REQUEST TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Anthropic Messages API request body.
data AnthropicRequest = AnthropicRequest
  { arModel       :: Text
  , arMaxTokens   :: Int
  , arMessages    :: [AnthropicMessage]
  , arSystem      :: Maybe Text
  , arTools       :: Maybe [Value]
  , arToolChoice  :: Maybe ToolChoice
  , arThinking    :: Maybe ThinkingConfig
  }
  deriving stock (Eq, Show, Generic)

-- | Tool choice configuration for controlling tool use behavior.
--
-- Note: 'ToolChoiceAny' and 'ToolChoiceTool' are NOT compatible with
-- extended thinking. If you need forced tool use, disable thinking.
data ToolChoice
  = ToolChoiceAuto              -- ^ Let model decide (default, compatible with thinking)
  | ToolChoiceNone              -- ^ Prevent tool use (compatible with thinking)
  | ToolChoiceAny               -- ^ Must use some tool (NOT compatible with thinking)
  | ToolChoiceTool Text         -- ^ Must use specific tool by name (NOT compatible with thinking)
  deriving stock (Eq, Show, Generic)

instance ToJSON ToolChoice where
  toJSON ToolChoiceAuto = object ["type" .= ("auto" :: Text)]
  toJSON ToolChoiceNone = object ["type" .= ("none" :: Text)]
  toJSON ToolChoiceAny = object ["type" .= ("any" :: Text)]
  toJSON (ToolChoiceTool name) = object
    [ "type" .= ("tool" :: Text)
    , "name" .= name
    ]

instance ToJSON AnthropicRequest where
  toJSON req = object $ filter ((/= Null) . snd)
    [ "model"      .= req.arModel
    , "max_tokens" .= req.arMaxTokens
    , "messages"   .= req.arMessages
    , "system"     .= req.arSystem
    , "tools"      .= req.arTools
    , "tool_choice" .= req.arToolChoice
    , "thinking"   .= req.arThinking
    ]

-- | A message in the conversation.
data AnthropicMessage = AnthropicMessage
  { amRole    :: Text
  , amContent :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON AnthropicMessage where
  toJSON msg = object
    [ "role"    .= msg.amRole
    , "content" .= msg.amContent
    ]

instance FromJSON AnthropicMessage where
  parseJSON = withObject "AnthropicMessage" $ \v ->
    AnthropicMessage <$> v .: "role" <*> v .: "content"

-- | Thinking/extended thinking configuration.
data ThinkingConfig = ThinkingConfig
  { tcType        :: Text  -- ^ "enabled"
  , tcBudgetTokens :: Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ThinkingConfig where
  toJSON cfg = object
    [ "type"          .= cfg.tcType
    , "budget_tokens" .= cfg.tcBudgetTokens
    ]


-- ════════════════════════════════════════════════════════════════════════════
-- SERVANT API TYPE
-- ════════════════════════════════════════════════════════════════════════════

-- | Anthropic Messages API endpoint.
--
-- @POST /v1/messages@
--
-- Headers:
-- - @x-api-key@: API key
-- - @anthropic-version@: API version (e.g., "2023-06-01")
type AnthropicAPI =
  "v1" :> "messages"
    :> Header' '[Required, Strict] "x-api-key" Text
    :> Header' '[Required, Strict] "anthropic-version" Text
    :> ReqBody '[JSON] AnthropicRequest
    :> Post '[JSON] AnthropicResponse


-- ════════════════════════════════════════════════════════════════════════════
-- CLIENT FUNCTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Call the Anthropic Messages API.
--
-- Uses Anthropic API version @2023-06-01@, which is the stable version
-- supporting messages, tools, and extended thinking. This version is
-- hardcoded because:
--
-- 1. API versions are rarely updated (last major version was 2023-06-01)
-- 2. Version changes require testing against actual API behavior
-- 3. Newer versions may have breaking changes in request/response format
--
-- If you need a different API version, you'll need to update this constant
-- and verify compatibility with the request/response types.
--
-- @
-- result <- runClientM (anthropicComplete apiKey req) env
-- @
anthropicComplete
  :: Text              -- ^ API key
  -> AnthropicRequest  -- ^ Request body
  -> ClientM AnthropicResponse
anthropicComplete apiKey req = client (Proxy @AnthropicAPI) apiKey anthropicApiVersion req

-- | Anthropic API version.
--
-- This is the stable version supporting messages, tools, and extended thinking.
-- See: https://docs.anthropic.com/en/api/versioning
anthropicApiVersion :: Text
anthropicApiVersion = "2023-06-01"
