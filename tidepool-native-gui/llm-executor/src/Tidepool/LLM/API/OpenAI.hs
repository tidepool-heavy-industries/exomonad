-- | OpenAI Chat Completions API client using servant-client.
--
-- Type-safe HTTP client for the OpenAI Chat Completions API.
-- Also works with OpenAI-compatible APIs (e.g., Cloudflare AI).
--
-- = Usage
--
-- @
-- import Tidepool.LLM.API.OpenAI (openaiComplete, OpenAIRequest(..))
-- import Servant.Client (runClientM, mkClientEnv)
--
-- env <- mkClientEnv manager (BaseUrl Https "api.openai.com" 443 "")
-- result <- runClientM (openaiComplete apiKey Nothing req) env
-- @
module Tidepool.LLM.API.OpenAI
  ( -- * API Type
    OpenAIAPI

    -- * Request Types
  , OpenAIRequest(..)
  , OpenAIMessage(..)

    -- * Client Function
  , openaiComplete

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
import Tidepool.Effects.LLMProvider (OpenAIResponse(..), OpenAIConfig(..))


-- ════════════════════════════════════════════════════════════════════════════
-- REQUEST TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | OpenAI Chat Completions API request body.
data OpenAIRequest = OpenAIRequest
  { orModel       :: Text
  , orMaxTokens   :: Int
  , orMessages    :: [OpenAIMessage]
  , orTemperature :: Maybe Double
  , orTools       :: Maybe [Value]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON OpenAIRequest where
  toJSON req = object $ filter ((/= Null) . snd)
    [ "model"       .= req.orModel
    , "max_tokens"  .= req.orMaxTokens
    , "messages"    .= req.orMessages
    , "temperature" .= req.orTemperature
    , "tools"       .= req.orTools
    ]

-- | A message in the conversation.
data OpenAIMessage = OpenAIMessage
  { omRole    :: Text
  , omContent :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON OpenAIMessage where
  toJSON msg = object $ filter ((/= Null) . snd)
    [ "role"    .= msg.omRole
    , "content" .= msg.omContent
    ]

instance FromJSON OpenAIMessage where
  parseJSON = withObject "OpenAIMessage" $ \v ->
    OpenAIMessage <$> v .: "role" <*> v .:? "content"


-- ════════════════════════════════════════════════════════════════════════════
-- SERVANT API TYPE
-- ════════════════════════════════════════════════════════════════════════════

-- | OpenAI Chat Completions API endpoint.
--
-- @POST /v1/chat/completions@
--
-- Headers:
-- - @Authorization@: Bearer token
-- - @OpenAI-Organization@: Optional organization ID
type OpenAIAPI =
  "v1" :> "chat" :> "completions"
    :> Header' '[Required, Strict] "Authorization" Text
    :> Header "OpenAI-Organization" Text
    :> ReqBody '[JSON] OpenAIRequest
    :> Post '[JSON] OpenAIResponse


-- ════════════════════════════════════════════════════════════════════════════
-- CLIENT FUNCTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Call the OpenAI Chat Completions API.
--
-- @
-- result <- runClientM (openaiComplete apiKey Nothing req) env
-- @
openaiComplete
  :: Text              -- ^ API key (will be prefixed with "Bearer ")
  -> Maybe Text        -- ^ Optional organization ID
  -> OpenAIRequest     -- ^ Request body
  -> ClientM OpenAIResponse
openaiComplete apiKey orgId req =
  client (Proxy @OpenAIAPI) ("Bearer " <> apiKey) orgId req
