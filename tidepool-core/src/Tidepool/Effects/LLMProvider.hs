{-# LANGUAGE UndecidableInstances #-}
-- | LLM provider types with type-level provider switching.
--
-- Effect type only - executors live in tidepool-llm-executor.
-- The type-level provider switch determines request/response schema.
module Tidepool.Effects.LLMProvider
  ( -- * Provider Type
    LLMProvider(..)
  , SProvider(..)

    -- * Provider-Specific Config
  , LLMProviderConfig(..)
  , AnthropicConfig(..)
  , OpenAIConfig(..)

    -- * Provider-Specific Response
  , LLMProviderResponse(..)
  , AnthropicResponse(..)
  , OpenAIResponse(..)

    -- * Response Component Types
  , ContentBlock(..)
  , Usage(..)
  , Choice(..)
  , Message(..)
  , ToolCall(..)
  , FunctionCall(..)

    -- * Effect
  , LLMComplete(..)
  , complete
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Freer (Eff, Member, send)
import Data.Aeson (Value, ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), (.:?))
import Data.Aeson.Types qualified as Aeson
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)


-- ════════════════════════════════════════════════════════════════════════════
-- PROVIDER TYPE (TYPE-LEVEL)
-- ════════════════════════════════════════════════════════════════════════════

-- | LLM provider at type level.
data LLMProvider = Anthropic | OpenAI
  deriving (Show, Eq)

-- | Singleton for provider - brings type-level to term-level.
data SProvider (p :: LLMProvider) where
  SAnthropic :: SProvider 'Anthropic
  SOpenAI :: SProvider 'OpenAI


-- ════════════════════════════════════════════════════════════════════════════
-- PROVIDER-SPECIFIC CONFIG
-- ════════════════════════════════════════════════════════════════════════════

-- | Type family mapping provider to its config type.
type family LLMProviderConfig (p :: LLMProvider) :: Type where
  LLMProviderConfig 'Anthropic = AnthropicConfig
  LLMProviderConfig 'OpenAI = OpenAIConfig

-- | Anthropic-specific configuration.
data AnthropicConfig = AnthropicConfig
  { acModel :: Text           -- e.g., "claude-sonnet-4-20250514"
  , acMaxTokens :: Int
  , acThinkingBudget :: Maybe Int
  , acSystemPrompt :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | OpenAI-specific configuration.
data OpenAIConfig = OpenAIConfig
  { oaModel :: Text           -- e.g., "gpt-4o"
  , oaMaxTokens :: Int
  , oaTemperature :: Maybe Double
  , oaSystemPrompt :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- PROVIDER-SPECIFIC RESPONSE
-- ════════════════════════════════════════════════════════════════════════════

-- | Type family mapping provider to its response type.
type family LLMProviderResponse (p :: LLMProvider) :: Type where
  LLMProviderResponse 'Anthropic = AnthropicResponse
  LLMProviderResponse 'OpenAI = OpenAIResponse

-- | Anthropic API response (content blocks style).
data AnthropicResponse = AnthropicResponse
  { arContent :: [ContentBlock]
  , arStopReason :: Text
  , arUsage :: Usage
  }
  deriving (Show, Eq, Generic)

instance ToJSON AnthropicResponse where
  toJSON r = object
    [ "content" .= r.arContent
    , "stop_reason" .= r.arStopReason
    , "usage" .= r.arUsage
    ]

instance FromJSON AnthropicResponse where
  parseJSON = withObject "AnthropicResponse" $ \v ->
    AnthropicResponse <$> v .: "content" <*> v .: "stop_reason" <*> v .: "usage"

-- | OpenAI API response (choices style).
data OpenAIResponse = OpenAIResponse
  { orChoices :: [Choice]
  , orUsage :: Usage
  }
  deriving (Show, Eq, Generic)

instance ToJSON OpenAIResponse where
  toJSON r = object
    [ "choices" .= r.orChoices
    , "usage" .= r.orUsage
    ]

instance FromJSON OpenAIResponse where
  parseJSON = withObject "OpenAIResponse" $ \v ->
    OpenAIResponse <$> v .: "choices" <*> v .: "usage"

-- | Content block (Anthropic-style).
data ContentBlock
  = TextContent Text
  | ToolUseContent Text Value  -- tool_name, input
  deriving (Show, Eq, Generic)

instance ToJSON ContentBlock where
  toJSON (TextContent t) = object ["type" .= ("text" :: Text), "text" .= t]
  toJSON (ToolUseContent name input_) = object
    ["type" .= ("tool_use" :: Text), "name" .= name, "input" .= input_]

instance FromJSON ContentBlock where
  parseJSON = withObject "ContentBlock" $ \v -> do
    ty <- v .: "type" :: Aeson.Parser Text
    case ty of
      "text" -> TextContent <$> v .: "text"
      "tool_use" -> ToolUseContent <$> v .: "name" <*> v .: "input"
      _ -> fail $ "Unknown content block type: " ++ show ty

-- | OpenAI choice.
data Choice = Choice
  { cMessage :: Message
  , cFinishReason :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Choice where
  toJSON c = object
    [ "message" .= c.cMessage
    , "finish_reason" .= c.cFinishReason
    ]

instance FromJSON Choice where
  parseJSON = withObject "Choice" $ \v ->
    Choice <$> v .: "message" <*> v .:? "finish_reason"

-- | OpenAI message.
data Message = Message
  { mRole :: Text
  , mContent :: Maybe Text
  , mToolCalls :: Maybe [ToolCall]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Message where
  toJSON m = object
    [ "role" .= m.mRole
    , "content" .= m.mContent
    , "tool_calls" .= m.mToolCalls
    ]

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v ->
    Message <$> v .: "role" <*> v .:? "content" <*> v .:? "tool_calls"

-- | OpenAI tool call.
data ToolCall = ToolCall
  { tcId :: Text
  , tcType :: Text
  , tcFunction :: FunctionCall
  }
  deriving (Show, Eq, Generic)

instance ToJSON ToolCall where
  toJSON tc = object
    [ "id" .= tc.tcId
    , "type" .= tc.tcType
    , "function" .= tc.tcFunction
    ]

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \v ->
    ToolCall <$> v .: "id" <*> v .: "type" <*> v .: "function"

-- | OpenAI function call.
data FunctionCall = FunctionCall
  { fcName :: Text
  , fcArguments :: Text  -- JSON string
  }
  deriving (Show, Eq, Generic)

instance ToJSON FunctionCall where
  toJSON fc = object
    [ "name" .= fc.fcName
    , "arguments" .= fc.fcArguments
    ]

instance FromJSON FunctionCall where
  parseJSON = withObject "FunctionCall" $ \v ->
    FunctionCall <$> v .: "name" <*> v .: "arguments"

-- | Token usage.
data Usage = Usage
  { uPromptTokens :: Int
  , uCompletionTokens :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON Usage where
  toJSON u = object
    [ "prompt_tokens" .= u.uPromptTokens
    , "completion_tokens" .= u.uCompletionTokens
    ]

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \v ->
    -- Handle both Anthropic (input_tokens) and OpenAI (prompt_tokens) field names
    Usage
      <$> (v .: "input_tokens" <|> v .: "prompt_tokens")
      <*> (v .: "output_tokens" <|> v .: "completion_tokens")


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | LLM completion effect with type-level provider.
--
-- The provider parameter determines:
-- - Request schema (Anthropic messages API vs OpenAI chat completions)
-- - Response parsing
-- - Auth mechanism
data LLMComplete r where
  Complete
    :: SProvider p
    -> LLMProviderConfig p
    -> Text                      -- user message
    -> Maybe [Value]             -- optional tools
    -> LLMComplete (LLMProviderResponse p)


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Call an LLM with provider-specific config.
complete
  :: forall p effs. Member LLMComplete effs
  => SProvider p
  -> LLMProviderConfig p
  -> Text
  -> Maybe [Value]
  -> Eff effs (LLMProviderResponse p)
complete provider config msg tools = send (Complete provider config msg tools)
