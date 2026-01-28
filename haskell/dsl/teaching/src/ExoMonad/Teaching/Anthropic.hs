{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
-- | Anthropic Messages API client using servant-client.
--
-- Internal for teaching infrastructure.
module ExoMonad.Teaching.Anthropic
  ( -- * API Type
    AnthropicAPI

    -- * Client Function
  , anthropicComplete
  ) where

import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Servant.API
import Servant.Client (ClientM, client)

import ExoMonad.Effects.LLMProvider (AnthropicResponse)
import ExoMonad.LLM.Types (AnthropicRequest)

-- | Anthropic Messages API endpoint.
type AnthropicAPI =
  "v1" :> "messages"
    :> Header' '[Required, Strict] "x-api-key" Text
    :> Header' '[Required, Strict] "anthropic-version" Text
    :> ReqBody '[JSON] AnthropicRequest
    :> Post '[JSON] AnthropicResponse

-- | Call the Anthropic Messages API.
anthropicComplete
  :: Text              -- ^ API key
  -> AnthropicRequest  -- ^ Request body
  -> ClientM AnthropicResponse
anthropicComplete apiKey req = client (Proxy @AnthropicAPI) apiKey anthropicApiVersion req

-- | Anthropic API version.
anthropicApiVersion :: Text
anthropicApiVersion = "2023-06-01"
