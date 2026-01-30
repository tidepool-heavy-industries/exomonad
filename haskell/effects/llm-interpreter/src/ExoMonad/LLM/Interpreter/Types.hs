-- | LLM interpreter types - configuration and environment.
--
-- Provides configuration for Anthropic API clients.
--
-- = See Also
--
-- * "ExoMonad.Effects.LLMProvider" - Effect definition and 'LLMError' type
-- * "ExoMonad.Tool.Wire" - Wire format types for tools
--
-- = Tool Schema Formats
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
-- Tool wire types are now consolidated in "ExoMonad.Tool.Wire" and re-exported here.
module ExoMonad.LLM.Interpreter.Types
  ( -- * Configuration
    LLMConfig (..),
    AnthropicSecrets (..),
    defaultAnthropicConfig,

    -- * Credential Newtypes (type-safe credentials)
    ApiKey (..),
    getApiKey,
    BaseUrl (..),
    getBaseUrl,
    Scheme (..),
    ParsedBaseUrl (..),

    -- * Environment
    LLMEnv (..),
    mkLLMEnv,

    -- * Error Types (re-exported from "ExoMonad.Effects.LLMProvider")
    LLMError (..),

    -- * Tool Definitions (re-exported from "ExoMonad.Tool.Wire")
    AnthropicTool (..),
    anthropicToolToJSON,

    -- * Anthropic Wire Types
    AnthropicRequest (..),
    AnthropicMessage (..),
    ToolChoice (..),
    ThinkingConfig (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.=))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
-- Re-export error types from effect module
import ExoMonad.Effects.LLMProvider (LLMError (..))
-- Re-export wire types
import ExoMonad.Tool.Wire (AnthropicTool (..), anthropicToolToJSON)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

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

-- | HTTP scheme.
data Scheme = Http | Https
  deriving stock (Eq, Show, Generic)

-- | Parsed base URL.
data ParsedBaseUrl = ParsedBaseUrl
  { pbuScheme :: Scheme,
    pbuHost :: String,
    pbuPort :: Int,
    pbuPath :: String
  }
  deriving stock (Eq, Show, Generic)

-- | Type-safe wrapper for API base URLs.
newtype BaseUrl = BaseUrl Text
  deriving stock (Eq, Show, Generic)

-- | Unwrap a base URL to get the underlying text.
getBaseUrl :: BaseUrl -> Text
getBaseUrl (BaseUrl t) = t

-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | LLM API configuration supporting Unix Sockets.
data LLMConfig
  = LLMSocketConfig
  { lcSocketPath :: FilePath
  }
  deriving stock (Eq, Show, Generic)

-- | Anthropic API secrets.
data AnthropicSecrets = AnthropicSecrets
  { -- | Type-safe API key (x-api-key header)
    asApiKey :: ApiKey,
    -- | Type-safe base URL (default: https://api.anthropic.com)
    asBaseUrl :: BaseUrl
  }
  deriving stock (Eq, Show, Generic)

-- | Default Anthropic configuration.
-- API key must be filled in.
defaultAnthropicConfig :: ApiKey -> AnthropicSecrets
defaultAnthropicConfig apiKey =
  AnthropicSecrets
    { asApiKey = apiKey,
      asBaseUrl = BaseUrl "https://api.anthropic.com"
    }

-- ════════════════════════════════════════════════════════════════════════════
-- ENVIRONMENT
-- ════════════════════════════════════════════════════════════════════════════

-- | Runtime environment with HTTP manager.
data LLMEnv = LLMEnv
  { leConfig :: LLMConfig,
    leManager :: Manager
  }

-- | Create a new environment.
mkLLMEnv :: LLMConfig -> IO LLMEnv
mkLLMEnv config = case config of
  LLMSocketConfig {} -> do
    manager <- newManager tlsManagerSettings
    pure LLMEnv {leConfig = config, leManager = manager}

-- ════════════════════════════════════════════════════════════════════════════
-- ANTHROPIC WIRE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Anthropic Messages API request body.
data AnthropicRequest = AnthropicRequest
  { arModel :: Text,
    arMaxTokens :: Int,
    arMessages :: NonEmpty AnthropicMessage,
    arSystem :: Maybe Text,
    arTools :: Maybe [Value],
    arToolChoice :: Maybe ToolChoice,
    arThinking :: Maybe ThinkingConfig
  }
  deriving stock (Eq, Show, Generic)

-- | Tool choice configuration for controlling tool use behavior.
data ToolChoice
  = ToolChoiceAuto
  | ToolChoiceNone
  | ToolChoiceAny
  | ToolChoiceTool Text
  deriving stock (Eq, Show, Generic)

instance ToJSON ToolChoice where
  toJSON ToolChoiceAuto = object ["type" .= ("auto" :: Text)]
  toJSON ToolChoiceNone = object ["type" .= ("none" :: Text)]
  toJSON ToolChoiceAny = object ["type" .= ("any" :: Text)]
  toJSON (ToolChoiceTool name) =
    object
      [ "type" .= ("tool" :: Text),
        "name" .= name
      ]

instance ToJSON AnthropicRequest where
  toJSON req =
    object $
      filter
        ((/= Null) . snd)
        [ "model" .= req.arModel,
          "max_tokens" .= req.arMaxTokens,
          "messages" .= req.arMessages,
          "system" .= req.arSystem,
          "tools" .= req.arTools,
          "tool_choice" .= req.arToolChoice,
          "thinking" .= req.arThinking
        ]

-- | A message in the conversation.
--
-- Content can be either simple text (String) or structured content blocks (Array).
-- This matches Anthropic's wire format where content is polymorphic.
data AnthropicMessage = AnthropicMessage
  { amRole :: Text,
    amContent :: Value -- Can be String (simple) or Array (content blocks)
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON AnthropicMessage where
  toJSON msg =
    object
      [ "role" .= msg.amRole,
        "content" .= msg.amContent
      ]

instance FromJSON AnthropicMessage where
  parseJSON = withObject "AnthropicMessage" $ \v ->
    AnthropicMessage <$> v .: "role" <*> v .: "content"

-- | Thinking/extended thinking configuration.
data ThinkingConfig = ThinkingConfig
  { -- | "enabled"
    tcType :: Text,
    tcBudgetTokens :: Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ThinkingConfig where
  toJSON cfg =
    object
      [ "type" .= cfg.tcType,
        "budget_tokens" .= cfg.tcBudgetTokens
      ]
