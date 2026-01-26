{-# LANGUAGE UndecidableInstances #-}
-- | LLM provider types with type-level provider switching.
--
-- Effect type only - interpreters live in exomonad-llm-interpreter.
-- The type-level provider switch determines request/response schema.
module ExoMonad.Effects.LLMProvider
  ( -- * Provider Type
    LLMProvider(..)
  , SProvider(..)

    -- * Provider-Specific Config
  , LLMProviderConfig
  , AnthropicConfig(..)
  , OpenAIConfig(..)
  , ThinkingBudget(..)

    -- * Provider-Specific Response
  , LLMProviderResponse
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
  , completeTry

    -- * Error Types
  , LLMError(..)
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Freer (Eff, Member, send)
import Data.Aeson (Value, ToJSON(..), FromJSON(..), object, (.=), withObject, (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types qualified as AesonTypes
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

-- | Extended thinking budget configuration.
--
-- Sum type makes the enabled/disabled state explicit - no more "Nothing means disabled".
data ThinkingBudget
  = ThinkingDisabled           -- ^ Extended thinking is disabled
  | ThinkingEnabled Int        -- ^ Extended thinking enabled with token budget
  deriving (Show, Eq, Generic)

instance ToJSON ThinkingBudget where
  toJSON ThinkingDisabled = Aeson.Null
  toJSON (ThinkingEnabled n) = toJSON n

instance FromJSON ThinkingBudget where
  parseJSON Aeson.Null = pure ThinkingDisabled
  parseJSON v = ThinkingEnabled <$> parseJSON v

-- | Type family mapping provider to its config type.
type family LLMProviderConfig (p :: LLMProvider) :: Type where
  LLMProviderConfig 'Anthropic = AnthropicConfig
  LLMProviderConfig 'OpenAI = OpenAIConfig

-- | Anthropic-specific configuration.
data AnthropicConfig = AnthropicConfig
  { acModel :: Text           -- e.g., "claude-sonnet-4-20250514"
  , acMaxTokens :: Int
  , acThinking :: ThinkingBudget  -- ^ Extended thinking configuration
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
--
-- Supports text, tool use, and thinking blocks from extended thinking.
data ContentBlock
  = TextContent Text
  | ToolUseContent Text Value  -- tool_name, input
  | ThinkingContent Text       -- thinking text (extended thinking feature)
  | RedactedThinkingContent    -- encrypted thinking block (no content exposed)
  deriving (Show, Eq, Generic)

instance ToJSON ContentBlock where
  toJSON (TextContent t) = object ["type" .= ("text" :: Text), "text" .= t]
  toJSON (ToolUseContent name input_) = object
    ["type" .= ("tool_use" :: Text), "name" .= name, "input" .= input_]
  toJSON (ThinkingContent t) = object
    ["type" .= ("thinking" :: Text), "thinking" .= t]
  toJSON RedactedThinkingContent = object
    ["type" .= ("redacted_thinking" :: Text)]

instance FromJSON ContentBlock where
  parseJSON = withObject "ContentBlock" $ \v -> do
    ty <- v .: "type" :: AesonTypes.Parser Text
    case ty of
      "text" -> TextContent <$> v .: "text"
      "tool_use" -> ToolUseContent <$> v .: "name" <*> v .: "input"
      "thinking" -> ThinkingContent <$> v .: "thinking"
      "redacted_thinking" -> pure RedactedThinkingContent
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
-- ERROR TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | LLM API errors.
--
-- These are returned by 'completeTry' and can be pattern-matched to handle
-- specific error conditions.
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


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | LLM completion effect with type-level provider.
--
-- The provider parameter determines:
-- - Request schema (Anthropic messages API vs OpenAI chat completions)
-- - Response parsing
-- - Auth mechanism
--
-- = Error Handling
--
-- The effect provides two variants:
--
-- * 'Complete' - throws on error (use when errors are fatal)
-- * 'CompleteTry' - returns @Either LLMError@ (use when you want to handle errors)
--
-- @
-- -- Throwing variant (crashes on error)
-- response <- complete SAnthropic config msg tools
--
-- -- Try variant (returns Either)
-- result <- completeTry SAnthropic config msg tools
-- case result of
--   Left err -> handleError err
--   Right response -> processResponse response
-- @
data LLMComplete r where
  Complete
    :: SProvider p
    -> LLMProviderConfig p
    -> Text                      -- user message
    -> Maybe [Value]             -- optional tools
    -> LLMComplete (LLMProviderResponse p)

  CompleteTry
    :: SProvider p
    -> LLMProviderConfig p
    -> Text                      -- user message
    -> Maybe [Value]             -- optional tools
    -> LLMComplete (Either LLMError (LLMProviderResponse p))


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Call an LLM with provider-specific config.
--
-- This variant throws on error. For error handling, use 'completeTry'.
complete
  :: forall p effs. Member LLMComplete effs
  => SProvider p
  -> LLMProviderConfig p
  -> Text
  -> Maybe [Value]
  -> Eff effs (LLMProviderResponse p)
complete provider config msg tools = send (Complete provider config msg tools)

-- | Call an LLM with provider-specific config, returning errors as 'Either'.
--
-- Use this when you want to handle errors gracefully:
--
-- @
-- result <- completeTry SAnthropic config msg tools
-- case result of
--   Left LLMRateLimited -> do
--     liftIO $ threadDelay 1000000
--     completeTry SAnthropic config msg tools  -- retry
--   Left err -> logError $ "LLM failed: " <> show err
--   Right response -> processResponse response
-- @
completeTry
  :: forall p effs. Member LLMComplete effs
  => SProvider p
  -> LLMProviderConfig p
  -> Text
  -> Maybe [Value]
  -> Eff effs (Either LLMError (LLMProviderResponse p))
completeTry provider config msg tools = send (CompleteTry provider config msg tools)
