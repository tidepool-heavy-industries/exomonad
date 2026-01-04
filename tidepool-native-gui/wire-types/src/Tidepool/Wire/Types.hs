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
  , ButtonConfig(..)

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
  , usButtons :: Maybe [ButtonConfig]
  , usGraphNode :: Text
  , usThinking :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON UIState where
  toJSON s = object
    [ "messages" .= usMessages s
    , "textInput" .= usTextInput s
    , "photoUpload" .= usPhotoUpload s
    , "buttons" .= usButtons s
    , "graphNode" .= usGraphNode s
    , "thinking" .= usThinking s
    ]

instance FromJSON UIState where
  parseJSON = withObject "UIState" $ \v ->
    UIState
      <$> v .: "messages"
      <*> v .:? "textInput"
      <*> v .:? "photoUpload"
      <*> v .:? "buttons"
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

-- | Button configuration.
data ButtonConfig = ButtonConfig
  { bcId :: Text
  , bcLabel :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ButtonConfig where
  toJSON c = object ["id" .= bcId c, "label" .= bcLabel c]

instance FromJSON ButtonConfig where
  parseJSON = withObject "ButtonConfig" $ \v ->
    ButtonConfig <$> v .: "id" <*> v .: "label"


-- ════════════════════════════════════════════════════════════════════════════
-- UserAction (Client → Server)
-- ════════════════════════════════════════════════════════════════════════════

-- | User action sent from client to server.
data UserAction
  = TextAction Text
  | ButtonAction Text        -- button id
  | PhotoAction Text Text    -- base64 data, mimeType
  deriving (Show, Eq, Generic)

instance ToJSON UserAction where
  toJSON (TextAction content_) = object
    [ "type" .= ("text" :: Text)
    , "content" .= content_
    ]
  toJSON (ButtonAction id_) = object
    [ "type" .= ("button" :: Text)
    , "id" .= id_
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
      "button" -> ButtonAction <$> v .: "id"
      "photo" -> PhotoAction <$> v .: "data" <*> v .: "mimeType"
      _ -> fail $ "Unknown action type: " ++ show ty
