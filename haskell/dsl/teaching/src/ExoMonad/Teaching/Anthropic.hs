{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Anthropic Messages API client using servant-client.
--
-- Internal for teaching infrastructure.
module ExoMonad.Teaching.Anthropic
  ( -- * API Type
    AnthropicAPI,

    -- * Client Function
    anthropicComplete,
  )
where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import ExoMonad.Effects.LLMProvider (AnthropicResponse)
import ExoMonad.LLM.Interpreter.Types (AnthropicRequest)
import Servant.API
import Servant.Client (ClientM, client)

-- | Anthropic Messages API endpoint.
type AnthropicAPI =
  "v1"
    :> "messages"
    :> Header' '[Required, Strict] "x-api-key" Text
    :> Header' '[Required, Strict] "anthropic-version" Text
    :> ReqBody '[JSON] AnthropicRequest
    :> Post '[JSON] AnthropicResponse

-- | Call the Anthropic Messages API.
anthropicComplete ::
  -- | API key
  Text ->
  -- | Request body
  AnthropicRequest ->
  ClientM AnthropicResponse
anthropicComplete apiKey req = client (Proxy @AnthropicAPI) apiKey anthropicApiVersion req

-- | Anthropic API version.
anthropicApiVersion :: Text
anthropicApiVersion = "2023-06-01"
