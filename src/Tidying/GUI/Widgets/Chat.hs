{-# LANGUAGE OverloadedStrings #-}
-- | Chat widgets for the Tidying GUI
--
-- Provides a sequential chat interface with system messages (left),
-- user messages (right), and photo thumbnails.
--
-- Uses typed ChatMessage instead of string parsing for reliable rendering.
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
import Control.Monad (void, forM)
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Tidepool.GUI.Core (GUIBridge(..), ChatMessage(..))

-- | Create the chat pane container
--
-- This is the scrolling area that holds all messages.
-- Call 'updateChatPane' to refresh its contents.
chatPane :: UI Element
chatPane = UI.div #. "chat-pane"

-- | Update the chat pane with messages from the narrative log
--
-- Pattern-matches on ChatMessage constructors for type-safe rendering.
updateChatPane :: Element -> GUIBridge state -> UI ()
updateChatPane container bridge = do
  entries <- liftIO $ atomically $ readTVar bridge.gbNarrativeLog
  messageEls <- mapM renderMessage (toList entries)

  void $ element container # set children []
  void $ element container #+ map element messageEls

  -- Auto-scroll to bottom
  runFunction $ ffi "$(%1).scrollTop($(%1)[0].scrollHeight)" container

-- | Render a ChatMessage to a UI element
renderMessage :: ChatMessage -> UI Element
renderMessage (SystemMessage txt) = systemMessage txt
renderMessage (UserMessage txt) = userMessage txt
renderMessage (PhotoMessage base64 mime) = photoMessage base64 mime
renderMessage (ChoicesMessage prompt choices) = choicesMessage prompt choices
renderMessage (SelectedMessage txt) = selectedMessage txt
renderMessage (ErrorMessage txt) = errorMessage txt

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

-- | Create a choices message showing what options were presented
choicesMessage :: Text -> [Text] -> UI Element
choicesMessage prompt choices = do
  container <- UI.div #. "chat-message chat-message-system chat-choices"

  promptEl <- UI.div #. "chat-choices-prompt"
    # set text (T.unpack prompt)

  choicesEl <- UI.div #. "chat-choices-options"

  choiceBtns <- forM choices $ \choice ->
    UI.span #. "chat-choice-badge"
      # set text (T.unpack choice)

  void $ element choicesEl #+ map element choiceBtns
  void $ element container #+ [element promptEl, element choicesEl]
  pure container

-- | Create a selected message (right-aligned, shows user's choice)
selectedMessage :: Text -> UI Element
selectedMessage txt =
  UI.div #. "chat-message chat-message-user chat-selected"
    # set text (T.unpack $ "✓ " <> txt)

-- | Create an error message (left-aligned, warning style)
errorMessage :: Text -> UI Element
errorMessage txt =
  UI.div #. "chat-message chat-message-system chat-error"
    # set text (T.unpack $ "⚠️ " <> txt)

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
