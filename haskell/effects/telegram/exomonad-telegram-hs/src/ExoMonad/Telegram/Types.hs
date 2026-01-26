-- | Core types for the Telegram effect DSL
--
-- These types define the boundary between Haskell agent logic and
-- the TypeScript Telegram integration runtime.
--
-- JSON encoding is designed to match TypeScript discriminated unions:
-- @{"type": "text", "text": "..."}@ not @{"tag": "TextMsg", "contents": "..."}@
module ExoMonad.Telegram.Types
  ( -- * Opaque Handles
    MediaHandle(..)

    -- * Button Types
  , InlineButton(..)

    -- * Messages
  , OutgoingMessage(..)
  , IncomingMessage(..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), Value, (.:), (.:?), (.=)
  , object, withObject
  )
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Opaque handle for media files.
--
-- The runtime interprets these handles - agent code just passes them around.
-- Handles are obtained from incoming media and can be used to send media.
newtype MediaHandle = MediaHandle Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | Inline keyboard button with callback data.
--
-- When pressed, the button's callback data is returned to the agent.
-- Only callback buttons are supported (no URL/login buttons).
--
-- JSON: @{"text": "Click me", "data": {...}}@
data InlineButton = InlineButton
  { buttonText :: Text   -- ^ Display text on the button
  , buttonData :: Value  -- ^ JSON payload returned on click
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON InlineButton where
  toJSON b = object ["text" .= b.buttonText, "data" .= b.buttonData]

instance FromJSON InlineButton where
  parseJSON = withObject "InlineButton" $ \v ->
    InlineButton <$> v .: "text" <*> v .: "data"

-- | Outgoing messages that can be sent to the user.
--
-- JSON uses TypeScript-style discriminated unions with @type@ field.
data OutgoingMessage
  = TextMsg Text
    -- ^ Plain text message
  | PhotoMsg MediaHandle (Maybe Text)
    -- ^ Photo with optional caption
  | DocMsg MediaHandle Text
    -- ^ Document with filename
  | ButtonsMsg Text [[InlineButton]]
    -- ^ Text message with inline keyboard (rows of buttons)
  deriving stock (Show, Eq, Generic)

instance ToJSON OutgoingMessage where
  toJSON (TextMsg t) = object
    [ "type" .= ("text" :: Text)
    , "text" .= t
    ]
  toJSON (PhotoMsg h c) = object $
    [ "type" .= ("photo" :: Text)
    , "media" .= h
    ] ++ maybe [] (\x -> ["caption" .= x]) c
  toJSON (DocMsg h f) = object
    [ "type" .= ("document" :: Text)
    , "media" .= h
    , "filename" .= f
    ]
  toJSON (ButtonsMsg t bs) = object
    [ "type" .= ("buttons" :: Text)
    , "text" .= t
    , "buttons" .= bs
    ]

instance FromJSON OutgoingMessage where
  parseJSON = withObject "OutgoingMessage" $ \v -> do
    typ <- v .: "type"
    case (typ :: Text) of
      "text" -> TextMsg <$> v .: "text"
      "photo" -> PhotoMsg <$> v .: "media" <*> v .:? "caption"
      "document" -> DocMsg <$> v .: "media" <*> v .: "filename"
      "buttons" -> ButtonsMsg <$> v .: "text" <*> v .: "buttons"
      _ -> fail $ "Unknown OutgoingMessage type: " ++ show typ

-- | Incoming messages received from the user.
--
-- Each variant is distinct to allow pattern matching on the type of response.
--
-- JSON uses TypeScript-style discriminated unions with @type@ field.
data IncomingMessage
  = TextReply Text
    -- ^ Plain text from user
  | PhotoReply MediaHandle (Maybe Text)
    -- ^ Photo with optional caption
  | DocReply MediaHandle Text
    -- ^ Document with filename
  | ButtonClick Value
    -- ^ Inline button callback data
  deriving stock (Show, Eq, Generic)

instance ToJSON IncomingMessage where
  toJSON (TextReply t) = object
    [ "type" .= ("text" :: Text)
    , "text" .= t
    ]
  toJSON (PhotoReply h c) = object $
    [ "type" .= ("photo" :: Text)
    , "media" .= h
    ] ++ maybe [] (\x -> ["caption" .= x]) c
  toJSON (DocReply h f) = object
    [ "type" .= ("document" :: Text)
    , "media" .= h
    , "filename" .= f
    ]
  toJSON (ButtonClick d) = object
    [ "type" .= ("button_click" :: Text)
    , "data" .= d
    ]

instance FromJSON IncomingMessage where
  parseJSON = withObject "IncomingMessage" $ \v -> do
    typ <- v .: "type"
    case (typ :: Text) of
      "text" -> TextReply <$> v .: "text"
      "photo" -> PhotoReply <$> v .: "media" <*> v .:? "caption"
      "document" -> DocReply <$> v .: "media" <*> v .: "filename"
      "button_click" -> ButtonClick <$> v .: "data"
      _ -> fail $ "Unknown IncomingMessage type: " ++ show typ
