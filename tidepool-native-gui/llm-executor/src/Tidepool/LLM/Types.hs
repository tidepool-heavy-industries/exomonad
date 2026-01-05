-- | LLM executor types - configuration and environment.
--
-- Provides configuration for Anthropic and OpenAI API clients.
--
-- = Tool Schema Formats
--
-- Anthropic and OpenAI use different tool definition formats:
--
-- __Anthropic__ (used by native executor):
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

    -- * Environment
  , LLMEnv(..)
  , mkLLMEnv

    -- * Error Types
  , LLMError(..)

    -- * Tool Definitions (re-exported from "Tidepool.Tool.Wire")
  , AnthropicTool(..)
  , anthropicToolToJSON
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- Re-export wire types
import Tidepool.Tool.Wire (AnthropicTool(..), anthropicToolToJSON)


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
  { asApiKey  :: Text  -- ^ Anthropic API key (x-api-key header)
  , asBaseUrl :: Text  -- ^ API base URL (default: https://api.anthropic.com)
  }
  deriving stock (Eq, Show, Generic)

-- | OpenAI API secrets.
data OpenAISecrets = OpenAISecrets
  { osApiKey  :: Text  -- ^ OpenAI API key (Authorization: Bearer)
  , osBaseUrl :: Text  -- ^ API base URL (default: https://api.openai.com)
  , osOrgId   :: Maybe Text  -- ^ Optional organization ID
  }
  deriving stock (Eq, Show, Generic)

-- | Default Anthropic configuration.
-- API key must be filled in.
defaultAnthropicConfig :: Text -> AnthropicSecrets
defaultAnthropicConfig apiKey = AnthropicSecrets
  { asApiKey  = apiKey
  , asBaseUrl = "https://api.anthropic.com"
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


-- ════════════════════════════════════════════════════════════════════════════
-- ERROR TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | LLM API errors.
data LLMError
  = LLMHttpError Text           -- ^ Network/HTTP error
  | LLMParseError Text          -- ^ JSON parsing failed
  | LLMRateLimited              -- ^ Rate limit hit (429)
  | LLMUnauthorized             -- ^ Invalid API key (401)
  | LLMContextTooLong           -- ^ Input too long (400 with specific error)
  | LLMOverloaded               -- ^ Service overloaded (529)
  | LLMApiError Text Text       -- ^ API error (type, message)
  | LLMNoProviderConfigured     -- ^ No provider secrets configured
  deriving stock (Eq, Show, Generic)


