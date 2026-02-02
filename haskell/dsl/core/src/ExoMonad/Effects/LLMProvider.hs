{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | LLM provider types with type-level provider switching.
--
-- Effect type only - interpreters live in exomonad-llm-interpreter.
-- The type-level provider switch determines request/response schema.
module ExoMonad.Effects.LLMProvider
  ( -- * Provider Type
    LLMProvider (..),
    SProvider (..),

    -- * Provider-Specific Config
    LLMProviderConfig,
    AnthropicConfig (..),
    ThinkingBudget (..),

    -- * Provider-Specific Response
    LLMProviderResponse,
    AnthropicResponse (..),

    -- * Response Component Types
    ContentBlock (..),
    Usage (..),

    -- * Message Types
    Message (..),
    Role (..),

    -- * Effect
    LLMComplete (..),
    complete,
    completeTry,
    completeConversation,
    completeConversationTry,

    -- * Error Types
    LLMError (..),
  )
where

import Polysemy (Sem, Member, makeSem)
import Polysemy.Internal (send)
import Data.Kind (Type)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonTypes
import ExoMonad.Effect.Types (LLMConfig, LlmError)
import GHC.Generics (Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- PROVIDER TYPE (TYPE-LEVEL)
-- ════════════════════════════════════════════════════════════════════════════

-- | LLM provider at type level.
data LLMProvider = Anthropic
  deriving (Show, Eq)

-- | Singleton for provider - brings type-level to term-level.
data SProvider (p :: LLMProvider) where
  SAnthropic :: SProvider 'Anthropic

-- ════════════════════════════════════════════════════════════════════════════
-- PROVIDER-SPECIFIC CONFIG
-- ════════════════════════════════════════════════════════════════════════════

-- | Extended thinking budget configuration.
--
-- Sum type makes the enabled/disabled state explicit - no more "Nothing means disabled".
data ThinkingBudget
  = -- | Extended thinking is disabled
    ThinkingDisabled
  | -- | Extended thinking enabled with token budget
    ThinkingEnabled Int
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

-- | Anthropic-specific configuration.
data AnthropicConfig = AnthropicConfig
  { acModel :: Text, -- e.g., "claude-sonnet-4-20250514"
    acMaxTokens :: Int,
    -- | Extended thinking configuration
    acThinking :: ThinkingBudget,
    acSystemPrompt :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- ════════════════════════════════════════════════════════════════════════════
-- MESSAGE TYPES (MULTI-TURN CONVERSATIONS)
-- ════════════════════════════════════════════════════════════════════════════

-- | Role in a conversation.
data Role = User | Assistant
  deriving (Show, Eq, Generic)

instance ToJSON Role where
  toJSON User = "user"
  toJSON Assistant = "assistant"

instance FromJSON Role where
  parseJSON = Aeson.withText "Role" $ \case
    "user" -> pure User
    "assistant" -> pure Assistant
    other -> fail $ "Unknown role: " ++ show other

-- | A message in a multi-turn conversation.
data Message = Message
  { msgRole :: Role,
    msgContent :: [ContentBlock]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Message where
  toJSON msg =
    object
      [ "role" .= msg.msgRole,
        "content" .= msg.msgContent
      ]

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v ->
    Message <$> v .: "role" <*> v .: "content"

-- ════════════════════════════════════════════════════════════════════════════
-- PROVIDER-SPECIFIC RESPONSE
-- ════════════════════════════════════════════════════════════════════════════

-- | Type family mapping provider to its response type.
type family LLMProviderResponse (p :: LLMProvider) :: Type where
  LLMProviderResponse 'Anthropic = AnthropicResponse

-- | Anthropic API response (content blocks style).
data AnthropicResponse = AnthropicResponse
  { arContent :: NonEmpty ContentBlock,
    arStopReason :: Text,
    arUsage :: Usage
  }
  deriving (Show, Eq, Generic)

instance ToJSON AnthropicResponse where
  toJSON r =
    object
      [ "content" .= r.arContent,
        "stop_reason" .= r.arStopReason,
        "usage" .= r.arUsage
      ]

instance FromJSON AnthropicResponse where
  parseJSON = withObject "AnthropicResponse" $ \v ->
    AnthropicResponse <$> v .: "content" <*> v .: "stop_reason" <*> v .: "usage"

-- | Content block (Anthropic-style).
--
-- Supports text, tool use, tool results, and thinking blocks from extended thinking.
data ContentBlock
  = TextContent Text
  | ToolUseContent Text Text Value -- tool_use_id, tool_name, input
  | ToolResultContent Text Value -- tool_use_id, result
  | ThinkingContent Text -- thinking text (extended thinking feature)
  | RedactedThinkingContent -- encrypted thinking block (no content exposed)
  deriving (Show, Eq, Generic)

instance ToJSON ContentBlock where
  toJSON (TextContent t) = object ["type" .= ("text" :: Text), "text" .= t]
  toJSON (ToolUseContent toolUseId name input_) =
    object
      [ "type" .= ("tool_use" :: Text),
        "id" .= toolUseId,
        "name" .= name,
        "input" .= input_
      ]
  toJSON (ToolResultContent toolUseId result) =
    object
      [ "type" .= ("tool_result" :: Text),
        "tool_use_id" .= toolUseId,
        "content" .= result
      ]
  toJSON (ThinkingContent t) =
    object
      ["type" .= ("thinking" :: Text), "thinking" .= t]
  toJSON RedactedThinkingContent =
    object
      ["type" .= ("redacted_thinking" :: Text)]

instance FromJSON ContentBlock where
  parseJSON = withObject "ContentBlock" $ \v -> do
    ty <- v .: "type" :: AesonTypes.Parser Text
    case ty of
      "text" -> TextContent <$> v .: "text"
      "tool_use" -> ToolUseContent <$> v .: "id" <*> v .: "name" <*> v .: "input"
      "tool_result" -> ToolResultContent <$> v .: "tool_use_id" <*> v .: "content"
      "thinking" -> ThinkingContent <$> v .: "thinking"
      "redacted_thinking" -> pure RedactedThinkingContent
      _ -> fail $ "Unknown content block type: " ++ show ty

-- | Token usage.
data Usage = Usage
  { uPromptTokens :: Int,
    uCompletionTokens :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON Usage where
  toJSON u =
    object
      [ "prompt_tokens" .= u.uPromptTokens,
        "completion_tokens" .= u.uCompletionTokens
      ]

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \v ->
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
  = -- | Network/HTTP error
    LLMHttpError Text
  | -- | JSON parsing failed
    LLMParseError Text
  | -- | Rate limit hit (429)
    LLMRateLimited
  | -- | Invalid API key (401)
    LLMUnauthorized
  | -- | Input too long (400 with specific error)
    LLMContextTooLong
  | -- | Service overloaded (529)
    LLMOverloaded
  | -- | API error (type, message)
    LLMApiError Text Text
  | -- | No provider secrets configured
    LLMNoProviderConfigured
  deriving stock (Eq, Show, Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | LLM completion effect with type-level provider.
--
-- The provider parameter determines:
-- - Request schema (Anthropic messages API)
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
--
-- = Multi-Turn Conversations
--
-- For tool use and multi-turn conversations, use the conversation variants:
--
-- @
-- -- Build conversation history
-- let messages =
--       [ Message User [TextContent "What's the weather?"]
--       , Message Assistant [ToolUseContent "toolu_123" "get_weather" weatherArgs]
--       , Message User [ToolResultContent "toolu_123" weatherResult]
--       ]
-- response <- completeConversation SAnthropic config messages tools
-- @
--
-- Note: The provider appears both at the effect level (@p :: LLMProvider@) and
-- as an explicit singleton argument (@SProvider p@) to each operation. The type
-- parameter @p@ is used to select and statically fix the provider in the effect
-- stack (so different providers inhabit different effects), while the
-- @SProvider p@ value is available at runtime for interpreters and callers that
-- need to pattern match on the provider, perform provider-specific logging, or
-- recover the provider when type applications are not convenient. This
-- duplication is intentional and required by the architecture; it is not an
-- accidental redundancy.
data LLMComplete (p :: LLMProvider) m a where
  Complete ::
    SProvider p ->
    LLMProviderConfig p ->
    Text -> -- user message
    Maybe [Value] -> -- optional tools
    LLMComplete p m (LLMProviderResponse p)
  CompleteTry ::
    SProvider p ->
    LLMProviderConfig p ->
    Text -> -- user message
    Maybe [Value] -> -- optional tools
    LLMComplete p m (Either LLMError (LLMProviderResponse p))
  CompleteConversation ::
    SProvider p ->
    LLMProviderConfig p ->
    [Message] -> -- conversation history
    Maybe [Value] -> -- optional tools
    LLMComplete p m (LLMProviderResponse p)
  CompleteConversationTry ::
    SProvider p ->
    LLMProviderConfig p ->
    [Message] -> -- conversation history
    Maybe [Value] -> -- optional tools
    LLMComplete p m (Either LLMError (LLMProviderResponse p))

makeSem ''LLMComplete

