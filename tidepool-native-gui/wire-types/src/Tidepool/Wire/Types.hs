-- | Wire format types for tidepool native GUI.
--
-- Defines UIState (server → client) and UserAction (client → server).
-- See PROTOCOL.md for WebSocket lifecycle.
module Tidepool.Wire.Types
  ( -- * Server → Client
    UIState(..)
  , ChatMessage(..)
  , MessageRole(..)
  , TextInputConfig(..)
  , PhotoUploadConfig(..)
  , ChoiceOption(..)
  , ChoiceConfig(..)
  , ButtonConfig

    -- * Client → Server
  , UserAction(..)
  ) where

import Data.Text (Text)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withObject, withText, (.:), (.:?))
import qualified Data.Aeson.Types as Aeson
import GHC.Generics (Generic)


-- ════════════════════════════════════════════════════════════════════════════
-- UIState (Server → Client)
-- ════════════════════════════════════════════════════════════════════════════

-- | UI state sent from server to client after each graph step.
data UIState = UIState
  { usMessages :: [ChatMessage]
  , usTextInput :: Maybe TextInputConfig
  , usPhotoUpload :: Maybe PhotoUploadConfig
  , usChoices :: Maybe ChoiceConfig
  , usGraphNode :: Text
  , usThinking :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON UIState where
  toJSON s = object
    [ "messages" .= usMessages s
    , "textInput" .= usTextInput s
    , "photoUpload" .= usPhotoUpload s
    , "choices" .= usChoices s
    , "graphNode" .= usGraphNode s
    , "thinking" .= usThinking s
    ]

instance FromJSON UIState where
  parseJSON = withObject "UIState" $ \v ->
    UIState
      <$> v .: "messages"
      <*> v .:? "textInput"
      <*> v .:? "photoUpload"
      <*> v .:? "choices"
      <*> v .: "graphNode"
      <*> v .: "thinking"

-- | Chat message in conversation history.
data ChatMessage = ChatMessage
  { cmRole :: MessageRole
  , cmContent :: Text
  , cmTimestamp :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ChatMessage where
  toJSON m = object
    [ "role" .= cmRole m
    , "content" .= cmContent m
    , "timestamp" .= cmTimestamp m
    ]

instance FromJSON ChatMessage where
  parseJSON = withObject "ChatMessage" $ \v ->
    ChatMessage <$> v .: "role" <*> v .: "content" <*> v .: "timestamp"

-- | Message role.
data MessageRole = User | Assistant | System
  deriving (Show, Eq, Generic)

instance ToJSON MessageRole where
  toJSON User = "user"
  toJSON Assistant = "assistant"
  toJSON System = "system"

instance FromJSON MessageRole where
  parseJSON = withText "MessageRole" $ \case
    "user" -> pure User
    "assistant" -> pure Assistant
    "system" -> pure System
    t -> fail $ "Unknown role: " ++ show t

-- | Text input configuration.
data TextInputConfig = TextInputConfig
  { ticPlaceholder :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON TextInputConfig where
  toJSON c = object ["placeholder" .= ticPlaceholder c]

instance FromJSON TextInputConfig where
  parseJSON = withObject "TextInputConfig" $ \v ->
    TextInputConfig <$> v .: "placeholder"

-- | Photo upload configuration.
data PhotoUploadConfig = PhotoUploadConfig
  { pucPrompt :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON PhotoUploadConfig where
  toJSON c = object ["prompt" .= pucPrompt c]

instance FromJSON PhotoUploadConfig where
  parseJSON = withObject "PhotoUploadConfig" $ \v ->
    PhotoUploadConfig <$> v .: "prompt"

-- | Choice option with rich metadata.
data ChoiceOption = ChoiceOption
  { coIndex :: Int              -- ^ 0-based index for response
  , coLabel :: Text             -- ^ Display label
  , coDescription :: Maybe Text -- ^ Optional descriptive text
  , coCosts :: [Text]           -- ^ Cost tags e.g. ["2 Stress", "1 Heat"]
  , coDisabled :: Maybe Text    -- ^ Nothing = enabled, Just reason = disabled
  }
  deriving (Show, Eq, Generic)

instance ToJSON ChoiceOption where
  toJSON c = object
    [ "index" .= coIndex c
    , "label" .= coLabel c
    , "description" .= coDescription c
    , "costs" .= coCosts c
    , "disabled" .= coDisabled c
    ]

instance FromJSON ChoiceOption where
  parseJSON = withObject "ChoiceOption" $ \v ->
    ChoiceOption
      <$> v .: "index"
      <*> v .: "label"
      <*> v .:? "description"
      <*> v .: "costs"
      <*> v .:? "disabled"

-- | Choice configuration with multi-select support.
data ChoiceConfig = ChoiceConfig
  { ccPrompt :: Text            -- ^ Prompt text shown above choices
  , ccOptions :: [ChoiceOption] -- ^ Available options
  , ccMultiSelect :: Bool       -- ^ True = checkboxes, False = radio buttons
  }
  deriving (Show, Eq, Generic)

instance ToJSON ChoiceConfig where
  toJSON c = object
    [ "prompt" .= ccPrompt c
    , "options" .= ccOptions c
    , "multiSelect" .= ccMultiSelect c
    ]

instance FromJSON ChoiceConfig where
  parseJSON = withObject "ChoiceConfig" $ \v ->
    ChoiceConfig
      <$> v .: "prompt"
      <*> v .: "options"
      <*> v .: "multiSelect"

-- | Deprecated alias for backwards compatibility.
{-# DEPRECATED ButtonConfig "Use ChoiceOption instead" #-}
type ButtonConfig = ChoiceOption


-- ════════════════════════════════════════════════════════════════════════════
-- UserAction (Client → Server)
-- ════════════════════════════════════════════════════════════════════════════

-- | User action sent from client to server.
data UserAction
  = TextAction Text           -- ^ Free-form text input
  | ChoiceAction Int          -- ^ Single selection (index)
  | MultiChoiceAction [Int]   -- ^ Multiple selections (indices)
  | PhotoAction Text Text     -- ^ Photo upload (base64 data, mimeType)
  deriving (Show, Eq, Generic)

instance ToJSON UserAction where
  toJSON (TextAction content_) = object
    [ "type" .= ("text" :: Text)
    , "content" .= content_
    ]
  toJSON (ChoiceAction idx) = object
    [ "type" .= ("choice" :: Text)
    , "index" .= idx
    ]
  toJSON (MultiChoiceAction idxs) = object
    [ "type" .= ("multiChoice" :: Text)
    , "indices" .= idxs
    ]
  toJSON (PhotoAction data_ mimeType_) = object
    [ "type" .= ("photo" :: Text)
    , "data" .= data_
    , "mimeType" .= mimeType_
    ]

instance FromJSON UserAction where
  parseJSON = withObject "UserAction" $ \v -> do
    ty <- v .: "type" :: Aeson.Parser Text
    case ty of
      "text" -> TextAction <$> v .: "content"
      "choice" -> ChoiceAction <$> v .: "index"
      "multiChoice" -> MultiChoiceAction <$> v .: "indices"
      "photo" -> PhotoAction <$> v .: "data" <*> v .: "mimeType"
      _ -> fail $ "Unknown action type: " ++ show ty
