{-# LANGUAGE OverloadedStrings #-}
-- | Core types for Anthropic Messages API
-- These are pure data types that can be used in WASM builds
module ExoMonad.Anthropic.Types
  ( -- * Request Types
    MessagesRequest(..)
  , Message(..)
  , Role(..)
  , ContentBlock(..)
  , ImageSource(..)
  , ToolUse(..)
  , ToolResult(..)
  , ToolChoice(..)
  , OutputFormat(..)
  , ThinkingConfig(..)
  , ThinkingContent(..)
  , RedactedThinking(..)

    -- * ID Newtypes (type-safe IDs)
  , ToolUseId(..)
  , ToolResultId(..)
  , toolUseToResultId

    -- * Response Types
  , MessagesResponse(..)
  , StopReason(..)
  , Usage(..)

    -- * Errors
  , ApiError(..)
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.String (IsString)

-- ══════════════════════════════════════════════════════════════
-- ID NEWTYPES (Type-Safe IDs)
-- ══════════════════════════════════════════════════════════════

-- | Type-safe wrapper for tool use IDs.
--
-- Prevents accidental confusion between tool use IDs and other Text values.
newtype ToolUseId = ToolUseId { unToolUseId :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, IsString)

-- | Type-safe wrapper for tool result IDs.
--
-- Must match a corresponding 'ToolUseId' from a tool use request.
newtype ToolResultId = ToolResultId { unToolResultId :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, IsString)

-- | Convert a ToolUseId to a ToolResultId.
--
-- When responding to a tool use request, the result must reference
-- the original tool use by ID. This converts the type safely.
toolUseToResultId :: ToolUseId -> ToolResultId
toolUseToResultId (ToolUseId t) = ToolResultId t


-- ══════════════════════════════════════════════════════════════
-- REQUEST TYPES
-- ══════════════════════════════════════════════════════════════

-- | A complete Messages API request
data MessagesRequest = MessagesRequest
  { model :: Text
  , messages :: [Message]
  , maxTokens :: Int
  , system :: Maybe Text
  , tools :: Maybe [Value]              -- Tool definitions as raw JSON
  , toolChoice :: Maybe ToolChoice
  , outputFormat :: Maybe OutputFormat  -- Structured output schema
  , thinking :: Maybe ThinkingConfig    -- Extended thinking configuration
  }
  deriving (Show, Eq, Generic)

instance ToJSON MessagesRequest where
  toJSON req = object $ filter ((/= Null) . snd)
    [ "model" .= req.model
    , "messages" .= req.messages
    , "max_tokens" .= req.maxTokens
    , "system" .= req.system
    , "tools" .= req.tools
    , "tool_choice" .= req.toolChoice
    , "output_format" .= req.outputFormat
    , "thinking" .= req.thinking
    ]

-- | Extended thinking configuration
data ThinkingConfig = ThinkingConfig
  { thinkingType :: Text       -- "enabled"
  , budgetTokens :: Int        -- Token budget for thinking
  }
  deriving (Show, Eq, Generic)

instance ToJSON ThinkingConfig where
  toJSON cfg = object
    [ "type" .= cfg.thinkingType
    , "budget_tokens" .= cfg.budgetTokens
    ]

-- | Structured output format specification
data OutputFormat = OutputFormat
  { outputType :: Text      -- Always "json_schema"
  , outputSchema :: Value   -- The JSON Schema
  }
  deriving (Show, Eq, Generic)

instance ToJSON OutputFormat where
  toJSON fmt = object
    [ "type" .= fmt.outputType
    , "schema" .= fmt.outputSchema
    ]

-- | A message in the conversation
data Message = Message
  { role :: Role
  , content :: [ContentBlock]
  }
  deriving (Show, Eq, Generic)

instance ToJSON Message where
  toJSON msg = object
    [ "role" .= msg.role
    , "content" .= msg.content
    ]

instance FromJSON Message where
  parseJSON = withObject "Message" $ \v -> Message
    <$> v .: "role"
    <*> v .: "content"

-- | Message role
data Role = User | Assistant
  deriving (Show, Eq, Generic)

instance ToJSON Role where
  toJSON User = String "user"
  toJSON Assistant = String "assistant"

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "user" -> pure User
    "assistant" -> pure Assistant
    other -> fail $ "Unknown role: " <> T.unpack other

-- | Content block - can be text, image, tool use, tool result, thinking, or json
data ContentBlock
  = TextBlock Text
  | ImageBlock ImageSource
  | ToolUseBlock ToolUse
  | ToolResultBlock ToolResult
  | ThinkingBlock ThinkingContent
  | RedactedThinkingBlock RedactedThinking
  | JsonBlock Value  -- Structured output from output_format
  deriving (Show, Eq, Generic)

-- | Thinking content from extended thinking
data ThinkingContent = ThinkingContent
  { thinkingText :: Text      -- The thinking content (may be summarized)
  , thinkingSignature :: Text -- Encrypted signature for verification
  }
  deriving (Show, Eq, Generic)

-- | Redacted thinking (encrypted, for safety)
data RedactedThinking = RedactedThinking
  { redactedData :: Text      -- Encrypted thinking data
  }
  deriving (Show, Eq, Generic)

-- | Image source for vision - either base64-encoded or URL
--
-- Base64 format:
-- @
-- { "type": "base64", "media_type": "image/jpeg", "data": "..." }
-- @
--
-- URL format:
-- @
-- { "type": "url", "url": "https://..." }
-- @
data ImageSource
  = Base64Image
      { isMediaType :: Text  -- ^ MIME type: "image/jpeg", "image/png", "image/gif", "image/webp"
      , isData :: Text       -- ^ Base64-encoded image data
      }
  | UrlImage
      { isUrl :: Text        -- ^ HTTPS URL to image
      }
  deriving (Show, Eq, Generic)

instance ToJSON ImageSource where
  toJSON (Base64Image mediaType imgData) = object
    [ "type" .= ("base64" :: Text)
    , "media_type" .= mediaType
    , "data" .= imgData
    ]
  toJSON (UrlImage url) = object
    [ "type" .= ("url" :: Text)
    , "url" .= url
    ]

instance FromJSON ImageSource where
  parseJSON = withObject "ImageSource" $ \v -> do
    srcType <- v .: "type" :: Parser Text
    case srcType of
      "base64" -> Base64Image <$> v .: "media_type" <*> v .: "data"
      "url" -> UrlImage <$> v .: "url"
      other -> fail $ "Unknown image source type: " <> T.unpack other

instance ToJSON ContentBlock where
  toJSON (TextBlock text) = object
    [ "type" .= ("text" :: Text)
    , "text" .= text
    ]
  toJSON (ImageBlock source) = object
    [ "type" .= ("image" :: Text)
    , "source" .= source
    ]
  toJSON (ToolUseBlock use) = object
    [ "type" .= ("tool_use" :: Text)
    , "id" .= use.toolUseId
    , "name" .= use.toolName
    , "input" .= use.toolInput
    ]
  toJSON (ToolResultBlock result) = object
    [ "type" .= ("tool_result" :: Text)
    , "tool_use_id" .= result.toolResultId
    , "content" .= result.toolResultContent
    , "is_error" .= result.toolResultIsError
    ]
  toJSON (ThinkingBlock thinking) = object
    [ "type" .= ("thinking" :: Text)
    , "thinking" .= thinking.thinkingText
    , "signature" .= thinking.thinkingSignature
    ]
  toJSON (RedactedThinkingBlock redacted) = object
    [ "type" .= ("redacted_thinking" :: Text)
    , "data" .= redacted.redactedData
    ]
  toJSON (JsonBlock jsonVal) = object
    [ "type" .= ("json" :: Text)
    , "json" .= jsonVal
    ]

instance FromJSON ContentBlock where
  parseJSON = withObject "ContentBlock" $ \v -> do
    blockType <- v .: "type" :: Parser Text
    case blockType of
      "text" -> TextBlock <$> v .: "text"
      "image" -> ImageBlock <$> v .: "source"
      "tool_use" -> do
        tuId <- v .: "id"
        tuName <- v .: "name"
        tuInput <- v .: "input"
        pure $ ToolUseBlock ToolUse
          { toolUseId = tuId
          , toolName = tuName
          , toolInput = tuInput
          }
      "tool_result" -> do
        trId <- v .: "tool_use_id"
        trContent <- v .: "content"
        trIsError <- v .:? "is_error" .!= False
        pure $ ToolResultBlock ToolResult
          { toolResultId = trId
          , toolResultContent = trContent
          , toolResultIsError = trIsError
          }
      "thinking" -> do
        thText <- v .: "thinking"
        thSig <- v .: "signature"
        pure $ ThinkingBlock ThinkingContent
          { thinkingText = thText
          , thinkingSignature = thSig
          }
      "redacted_thinking" -> do
        rData <- v .: "data"
        pure $ RedactedThinkingBlock RedactedThinking
          { redactedData = rData
          }
      "json" -> do
        jsonVal <- v .: "json"
        pure $ JsonBlock jsonVal
      other -> fail $ "Unknown content block type: " <> T.unpack other

-- | A tool use request from the model
data ToolUse = ToolUse
  { toolUseId :: ToolUseId    -- ^ Type-safe tool use ID
  , toolName :: Text
  , toolInput :: Value
  }
  deriving (Show, Eq, Generic)

-- | A tool result to send back to the model
data ToolResult = ToolResult
  { toolResultId :: ToolResultId  -- ^ Must match a ToolUseId from request
  , toolResultContent :: Text
  , toolResultIsError :: Bool
  }
  deriving (Show, Eq, Generic)

-- | Tool choice configuration
data ToolChoice
  = ToolChoiceAuto
  | ToolChoiceAny
  | ToolChoiceNone
  | ToolChoiceTool Text  -- specific tool name
  deriving (Show, Eq, Generic)

instance ToJSON ToolChoice where
  toJSON ToolChoiceAuto = object ["type" .= ("auto" :: Text)]
  toJSON ToolChoiceAny = object ["type" .= ("any" :: Text)]
  toJSON ToolChoiceNone = object ["type" .= ("none" :: Text)]
  toJSON (ToolChoiceTool name) = object
    [ "type" .= ("tool" :: Text)
    , "name" .= name
    ]

-- ══════════════════════════════════════════════════════════════
-- RESPONSE TYPES
-- ══════════════════════════════════════════════════════════════

-- | A complete Messages API response
data MessagesResponse = MessagesResponse
  { responseId :: Text
  , responseContent :: [ContentBlock]
  , responseStopReason :: StopReason
  , responseUsage :: Usage
  }
  deriving (Show, Eq, Generic)

instance FromJSON MessagesResponse where
  parseJSON = withObject "MessagesResponse" $ \v -> MessagesResponse
    <$> v .: "id"
    <*> v .: "content"
    <*> v .: "stop_reason"
    <*> v .: "usage"

-- | Why the model stopped generating
data StopReason
  = EndTurn        -- Normal completion
  | ToolUseStop    -- Wants to use a tool
  | MaxTokens      -- Hit token limit
  | StopSequence   -- Hit a stop sequence
  | Refusal        -- Safety refusal
  | PauseTurn      -- Server tool pause (web search etc)
  deriving (Show, Eq, Generic)

instance FromJSON StopReason where
  parseJSON = withText "StopReason" $ \case
    "end_turn" -> pure EndTurn
    "tool_use" -> pure ToolUseStop
    "max_tokens" -> pure MaxTokens
    "stop_sequence" -> pure StopSequence
    "refusal" -> pure Refusal
    "pause_turn" -> pure PauseTurn
    other -> fail $ "Unknown stop reason: " <> T.unpack other

-- | Token usage information
data Usage = Usage
  { inputTokens :: Int
  , outputTokens :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON Usage where
  parseJSON = withObject "Usage" $ \v -> Usage
    <$> v .: "input_tokens"
    <*> v .: "output_tokens"

-- ══════════════════════════════════════════════════════════════
-- ERROR TYPES
-- ══════════════════════════════════════════════════════════════

-- | API errors
data ApiError
  = HttpError Text          -- Network/HTTP error
  | ParseError Text         -- JSON parsing failed
  | ApiErrorResponse        -- API returned an error
      { errorType :: Text
      , errorMessage :: Text
      }
  deriving (Show, Eq, Generic)
