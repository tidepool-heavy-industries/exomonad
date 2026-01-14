-- | LLM interpreter types - configuration and environment.
--
-- Provides configuration for Anthropic and OpenAI API clients.
--
-- = See Also
--
-- * "Tidepool.Effects.LLMProvider" - Effect definition and 'LLMError' type
-- * "Tidepool.Tool.Wire" - Wire format types for tools
--
-- = Tool Schema Formats
--
-- Anthropic and OpenAI use different tool definition formats:
--
-- __Anthropic__ (used by native interpreter):
--
-- @
-- { "name": "search",
--   "description": "Search the web",
--   "input_schema": { "type": "object", "properties": {...} }
-- }
-- @
--
-- __OpenAI__ (used by CF AI, see "Tidepool.Tool.Wire"):
--
-- @
-- { "type": "function",
--   "function": { "name": "search", "description": "...", "parameters": {...} }
-- }
-- @
--
-- Tool wire types are now consolidated in "Tidepool.Tool.Wire" and re-exported here.
module Tidepool.LLM.Types
  ( -- * Configuration
    LLMConfig(..)
  , AnthropicSecrets(..)
  , OpenAISecrets(..)
  , defaultAnthropicConfig

    -- * Credential Newtypes (type-safe credentials)
  , ApiKey(..)
  , getApiKey
  , BaseUrl(..)
  , getBaseUrl

    -- * Environment
  , LLMEnv(..)
  , mkLLMEnv

    -- * Error Types (re-exported from "Tidepool.Effects.LLMProvider")
  , LLMError(..)

    -- * Tool Definitions (re-exported from "Tidepool.Tool.Wire")
  , AnthropicTool(..)
  , anthropicToolToJSON
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- Re-export error types from effect module
import Tidepool.Effects.LLMProvider (LLMError(..))

-- Re-export wire types
import Tidepool.Tool.Wire (AnthropicTool(..), anthropicToolToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- CREDENTIAL NEWTYPES (Type-Safe Credentials)
-- ════════════════════════════════════════════════════════════════════════════

-- | Type-safe wrapper for API keys.
--
-- Prevents accidental confusion between API keys and other Text values
-- (e.g., swapping API key and base URL).
newtype ApiKey = ApiKey Text
  deriving stock (Eq, Generic)

-- Show instance redacts the key for safety
instance Show ApiKey where
  show (ApiKey _) = "ApiKey <redacted>"

-- | Unwrap an API key to get the underlying text.
getApiKey :: ApiKey -> Text
getApiKey (ApiKey t) = t

-- | Type-safe wrapper for API base URLs.
--
-- Prevents accidental confusion between URLs and other credentials.
newtype BaseUrl = BaseUrl Text
  deriving stock (Eq, Show, Generic)

-- | Unwrap a base URL to get the underlying text.
getBaseUrl :: BaseUrl -> Text
getBaseUrl (BaseUrl t) = t


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | LLM API configuration with secrets for both providers.
data LLMConfig = LLMConfig
  { lcAnthropicSecrets :: Maybe AnthropicSecrets
  , lcOpenAISecrets    :: Maybe OpenAISecrets
  }
  deriving stock (Eq, Show, Generic)

-- | Anthropic API secrets.
data AnthropicSecrets = AnthropicSecrets
  { asApiKey  :: ApiKey   -- ^ Type-safe API key (x-api-key header)
  , asBaseUrl :: BaseUrl  -- ^ Type-safe base URL (default: https://api.anthropic.com)
  }
  deriving stock (Eq, Show, Generic)

-- | OpenAI API secrets.
data OpenAISecrets = OpenAISecrets
  { osApiKey  :: ApiKey      -- ^ Type-safe API key (Authorization: Bearer)
  , osBaseUrl :: BaseUrl     -- ^ Type-safe base URL (default: https://api.openai.com)
  , osOrgId   :: Maybe Text  -- ^ Optional organization ID
  }
  deriving stock (Eq, Show, Generic)

-- | Default Anthropic configuration.
-- API key must be filled in.
defaultAnthropicConfig :: ApiKey -> AnthropicSecrets
defaultAnthropicConfig apiKey = AnthropicSecrets
  { asApiKey  = apiKey
  , asBaseUrl = BaseUrl "https://api.anthropic.com"
  }


-- ════════════════════════════════════════════════════════════════════════════
-- ENVIRONMENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Runtime environment with HTTP manager.
data LLMEnv = LLMEnv
  { leConfig  :: LLMConfig
  , leManager :: Manager
  }

-- | Create a new environment (creates TLS manager).
mkLLMEnv :: LLMConfig -> IO LLMEnv
mkLLMEnv config = do
  manager <- newManager tlsManagerSettings
  pure LLMEnv { leConfig = config, leManager = manager }


