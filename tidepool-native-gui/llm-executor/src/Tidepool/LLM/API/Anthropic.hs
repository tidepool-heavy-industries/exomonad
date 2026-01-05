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
  , arThinking    :: Maybe ThinkingConfig
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON AnthropicRequest where
  toJSON req = object $ filter ((/= Null) . snd)
    [ "model"      .= req.arModel
    , "max_tokens" .= req.arMaxTokens
    , "messages"   .= req.arMessages
    , "system"     .= req.arSystem
    , "tools"      .= req.arTools
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
-- @
-- result <- runClientM (anthropicComplete apiKey req) env
-- @
anthropicComplete
  :: Text              -- ^ API key
  -> AnthropicRequest  -- ^ Request body
  -> ClientM AnthropicResponse
anthropicComplete apiKey req = client (Proxy @AnthropicAPI) apiKey "2023-06-01" req
