{-# LANGUAGE OverloadedStrings #-}
-- | Chat widgets for the Tidying GUI
--
-- Provides a sequential chat interface with system messages (left),
-- user messages (right), and photo thumbnails.
module Tidying.GUI.Widgets.Chat
  ( -- * Chat pane
    chatPane
  , updateChatPane
    -- * Individual messages
  , systemMessage
  , userMessage
  , photoMessage
  , typingIndicator
  ) where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad (void)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Tidepool.GUI.Core (GUIBridge(..))

-- | Create the chat pane container
--
-- This is the scrolling area that holds all messages.
-- Call 'updateChatPane' to refresh its contents.
chatPane :: UI Element
chatPane = UI.div #. "chat-pane"

-- | Update the chat pane with messages from the narrative log
--
-- Messages are parsed to determine if they're system or user messages.
-- Lines starting with "> " are treated as user input.
updateChatPane :: Element -> GUIBridge state -> UI ()
updateChatPane container bridge = do
  entries <- liftIO $ atomically $ readTVar bridge.gbNarrativeLog
  let messages = map parseMessage (toList entries)
  messageEls <- mapM renderMessage messages

  void $ element container # set children []
  void $ element container #+ map element messageEls

  -- Auto-scroll to bottom
  runFunction $ ffi "$(%1).scrollTop($(%1)[0].scrollHeight)" container

-- | Message types for rendering
data MessageType
  = SystemMsg Text
  | UserMsg Text
  | PhotoMsg Text Text  -- base64 data, mime type
  deriving (Eq, Show)

-- | Parse a narrative log entry into a message type
--
-- Convention:
-- - Lines starting with "> " are user input
-- - Lines starting with "[photo:" are photo references (format: [photo:data:image/TYPE;base64,DATA])
-- - Everything else is system messages
parseMessage :: Text -> MessageType
parseMessage txt
  | "> " `T.isPrefixOf` txt = UserMsg (T.drop 2 txt)
  | "[photo:" `T.isPrefixOf` txt = parsePhotoRef txt
  | otherwise = SystemMsg txt
  where
    -- Parse "[photo:data:image/jpeg;base64,...]" format
    -- Falls back to SystemMsg if format doesn't match
    parsePhotoRef t =
      let content = T.drop 7 $ T.dropEnd 1 t  -- Remove "[photo:" and "]"
          (mimeWithPrefix, dataWithPrefix) = T.breakOn ";base64," content
      in if T.null dataWithPrefix
         then SystemMsg txt  -- Malformed: no ";base64," found
         else
           let base64Data = T.drop 8 dataWithPrefix  -- Remove ";base64,"
               mime = if "data:" `T.isPrefixOf` mimeWithPrefix
                      then T.drop 5 mimeWithPrefix  -- Remove "data:" prefix
                      else mimeWithPrefix
           in if T.null base64Data
              then SystemMsg txt  -- Malformed: empty base64 data
              else PhotoMsg base64Data mime

-- | Render a message based on its type
renderMessage :: MessageType -> UI Element
renderMessage (SystemMsg txt) = systemMessage txt
renderMessage (UserMsg txt) = userMessage txt
renderMessage (PhotoMsg base64 mime) = photoMessage base64 mime

-- | Create a system message bubble (left-aligned)
systemMessage :: Text -> UI Element
systemMessage txt =
  UI.div #. "chat-message chat-message-system"
    # set text (T.unpack txt)

-- | Create a user message bubble (right-aligned)
userMessage :: Text -> UI Element
userMessage txt =
  UI.div #. "chat-message chat-message-user"
    # set text (T.unpack txt)

-- | Create a photo message with thumbnail
photoMessage :: Text -> Text -> UI Element
photoMessage base64Data mimeType = do
  container <- UI.div #. "chat-photo"

  let dataUrl = "data:" <> mimeType <> ";base64," <> base64Data

  img <- UI.img
    # set (attr "src") (T.unpack dataUrl)
    # set (attr "alt") "Uploaded photo"

  caption <- UI.div #. "chat-photo-caption"
    # set text "Photo uploaded"

  void $ element container #+ [element img, element caption]
  pure container

-- | Create a typing indicator (animated dots)
--
-- Use this to show that the system is "thinking" or processing.
typingIndicator :: UI Element
typingIndicator = do
  container <- UI.div #. "typing-indicator"

  dot1 <- UI.div #. "typing-dot"
  dot2 <- UI.div #. "typing-dot"
  dot3 <- UI.div #. "typing-dot"

  void $ element container #+ [element dot1, element dot2, element dot3]
  pure container
