{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core types for Anthropic Messages API
-- These are pure data types that can be used in WASM builds
module ExoMonad.Anthropic.Types
  ( -- * Request Types
    MessagesRequest (..),
    Message (..),
    Role (..),
    ContentBlock (..),
    ImageSource (..),
    ToolUse (..),
    ToolResult (..),
    ToolChoice (..),
    OutputFormat (..),
    ThinkingConfig (..),
    ThinkingContent (..),
    RedactedThinking (..),

    -- * ID Newtypes (type-safe IDs)
    ToolUseId (..),
    ToolResultId (..),
    toolUseToResultId,

    -- * Response Types
    MessagesResponse (..),
    StopReason (..),
    Usage (..),

    -- * Errors
    ApiError (..),
  )
where

import Data.Aeson (Value)
import Deriving.Aeson

-- ══════════════════════════════════════════════════════════════
-- ID NEWTYPES (Type-Safe IDs)
-- ══════════════════════════════════════════════════════════════

-- | Type-safe wrapper for tool use IDs.
newtype ToolUseId = ToolUseId {unToolUseId :: Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, IsString)

-- | Type-safe wrapper for tool result IDs.
newtype ToolResultId = ToolResultId {unToolResultId :: Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, IsString)

-- | Convert a ToolUseId to a ToolResultId.
toolUseToResultId :: ToolUseId -> ToolResultId
toolUseToResultId (ToolUseId t) = ToolResultId t

-- ══════════════════════════════════════════════════════════════
-- REQUEST TYPES
-- ══════════════════════════════════════════════════════════════

-- | A complete Messages API request
data MessagesRequest = MessagesRequest
  { model :: Text,
    messages :: NonEmpty Message,
    maxTokens :: Int,
    system :: Maybe Text,
    tools :: Maybe [Value],
    toolChoice :: Maybe ToolChoice,
    outputFormat :: Maybe OutputFormat,
    thinking :: Maybe ThinkingConfig
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[FieldLabelModifier CamelToSnake, OmitNothingFields] MessagesRequest

-- | Extended thinking configuration
data ThinkingConfig = ThinkingConfig
  { type_ :: Text,
    budgetTokens :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[FieldLabelModifier '[StripSuffix "_", CamelToSnake]] ThinkingConfig

-- | Structured output format specification
data OutputFormat = OutputFormat
  { type_ :: Text,
    schema :: Value
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[FieldLabelModifier '[StripSuffix "_"]] OutputFormat

-- | A message in the conversation
data Message = Message
  { role :: Role,
    content :: NonEmpty ContentBlock
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[] Message

-- | Message role
data Role = User | Assistant
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[ConstructorTagModifier CamelToSnake] Role

-- | Image source for vision - either base64-encoded or URL
data ImageSource
  = Base64
      { mediaType :: Text,
        data_ :: Text
      }
  | Url
      { url :: Text
      }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[SumTaggedObject "type" "", ConstructorTagModifier CamelToSnake, FieldLabelModifier '[StripSuffix "_", CamelToSnake]] ImageSource

-- | Content block - can be text, image, tool use, tool result, thinking, or json
data ContentBlock
  = Text {text :: Text}
  | Image {source :: ImageSource}
  | ToolUse {id :: ToolUseId, name :: Text, input :: Value}
  | ToolResult {toolUseId :: ToolResultId, content :: Text, isError :: Bool}
  | Thinking {thinking :: Text, signature :: Text}
  | RedactedThinking {data_ :: Text}
  | Json {json :: Value}
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[SumTaggedObject "type" "", ConstructorTagModifier CamelToSnake, FieldLabelModifier '[StripSuffix "_", CamelToSnake]] ContentBlock

-- | A tool use request from the model
data ToolUse = ToolUseData
  { id :: ToolUseId,
    name :: Text,
    input :: Value
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[] ToolUse

-- | A tool result to send back to the model
data ToolResult = ToolResultData
  { toolUseId :: ToolResultId,
    content :: Text,
    isError :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[FieldLabelModifier CamelToSnake] ToolResult

-- | Thinking content from extended thinking
data ThinkingContent = ThinkingContentData
  { thinking :: Text,
    signature :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[] ThinkingContent

-- | Redacted thinking (encrypted, for safety)
data RedactedThinking = RedactedThinkingData
  { data_ :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[FieldLabelModifier '[StripSuffix "_", CamelToSnake]] RedactedThinking

-- | Tool choice configuration
data ToolChoice
  = Auto
  | Any
  | None
  | Tool {name :: Text}
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[SumTaggedObject "type" "", ConstructorTagModifier CamelToSnake] ToolChoice

-- ══════════════════════════════════════════════════════════════
-- RESPONSE TYPES
-- ══════════════════════════════════════════════════════════════

-- | A complete Messages API response
data MessagesResponse = MessagesResponse
  { id :: Text,
    content :: NonEmpty ContentBlock,
    stopReason :: StopReason,
    usage :: Usage
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[FieldLabelModifier CamelToSnake] MessagesResponse

-- | Why the model stopped generating
data StopReason
  = EndTurn
  | ToolUseStop
  | MaxTokens
  | StopSequence
  | Refusal
  | PauseTurn
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[ConstructorTagModifier '[StripSuffix "Stop", CamelToSnake]] StopReason

-- | Token usage information
data Usage = Usage
  { inputTokens :: Int,
    outputTokens :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[FieldLabelModifier CamelToSnake] Usage

-- ══════════════════════════════════════════════════════════════
-- ERROR TYPES
-- ══════════════════════════════════════════════════════════════

-- | API errors
data ApiError
  = HttpError Text
  | ParseError Text
  | ApiErrorResponse
      { errorType :: Text,
        errorMessage :: Text
      }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON) via CustomJSON '[SumTaggedObject "type" "", FieldLabelModifier CamelToSnake] ApiError