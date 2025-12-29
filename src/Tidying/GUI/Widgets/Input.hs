{-# LANGUAGE OverloadedStrings #-}
-- | Text input with optional photo attachment
--
-- Provides a chat-style input widget where users can type text
-- and optionally attach a photo. Similar to messaging apps.
module Tidying.GUI.Widgets.Input
  ( textInputWithPhoto
  ) where

import Control.Monad (void, when)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Tidepool.GUI.Core (GUIBridge(..), RequestResponse(..), safeSubmitResponse)

-- | Create a text input widget with optional photo attachment
--
-- Features:
-- - Text input field with submit button
-- - Camera button to attach a photo
-- - Inline photo preview with remove option
-- - Submits TextWithPhotoResponse if photo attached, TextResponse otherwise
--
-- NOTE: Uses hardcoded element IDs so only one can exist per page.
textInputWithPhoto :: GUIBridge state -> Text -> UI Element
textInputWithPhoto bridge prompt = do
  container <- UI.div #. "text-input-with-photo"

  -- Prompt
  promptEl <- UI.div #. "input-prompt" # set text (T.unpack prompt)

  -- Photo preview area (hidden until photo attached)
  previewArea <- UI.div #. "photo-preview-area"
    # set (attr "id") "text-photo-preview-area"
    # set style [("display", "none")]

  previewImg <- UI.img #. "photo-preview-img"
    # set (attr "id") "text-photo-preview-img"

  removeBtn <- UI.button #. "photo-remove-btn"
    # set (attr "id") "text-photo-remove-btn"
    # set text "×"
    # set (attr "title") "Remove photo"

  void $ element previewArea #+ [element previewImg, element removeBtn]

  -- Input row: attach button + input field + submit button
  inputRow <- UI.div #. "text-input-row"

  -- Hidden file input for photos
  fileInput <- UI.input
    # set (attr "type") "file"
    # set (attr "accept") "image/*"
    # set (attr "id") "text-photo-file-input"
    # set style [("display", "none")]

  -- Attach photo button
  attachBtn <- UI.button #. "attach-photo-btn"
    # set (attr "id") "text-attach-btn"
    # set (attr "title") "Attach photo"
    # set html "📷"

  -- Text input
  inputEl <- UI.input #. "text-input"
    # set (attr "id") "text-input-field"
    # set (attr "placeholder") "Type your response..."
    # set (attr "autocomplete") "off"

  -- Submit button
  submitBtn <- UI.button #. "submit-btn"
    # set (attr "id") "text-submit-btn"
    # set text "Send"

  void $ element inputRow #+
    [ element fileInput
    , element attachBtn
    , element inputEl
    , element submitBtn
    ]

  -- Click attach button to trigger file input
  on UI.click attachBtn $ \_ ->
    runFunction $ ffi "var el = document.getElementById('text-photo-file-input'); if (el) el.click()"

  -- File input change handler - show preview
  -- Use setTimeout to defer until after DOM is ready
  runFunction $ ffi $ T.unpack $ T.unlines
    [ "setTimeout(function() {"
    , "  var fileInput = document.getElementById('text-photo-file-input');"
    , "  if (!fileInput) return;"
    , "  fileInput.addEventListener('change', function() {"
    , "    var file = this.files[0];"
    , "    if (!file) return;"
    , "    "
    , "    var reader = new FileReader();"
    , "    reader.onload = function(e) {"
    , "      var dataUrl = e.target.result;"
    , "      var previewArea = document.getElementById('text-photo-preview-area');"
    , "      var previewImg = document.getElementById('text-photo-preview-img');"
    , "      if (!previewArea || !previewImg) return;"
    , "      "
    , "      previewImg.src = dataUrl;"
    , "      previewImg.dataset.base64 = dataUrl.split(',')[1];"
    , "      previewImg.dataset.mime = file.type;"
    , "      previewArea.style.display = 'flex';"
    , "    };"
    , "    reader.readAsDataURL(file);"
    , "  });"
    , "}, 0);"
    ]

  -- Remove button - clear photo
  on UI.click removeBtn $ \_ -> do
    runFunction $ ffi $ T.unpack $ T.unlines
      [ "(function() {"
      , "  var previewArea = document.getElementById('text-photo-preview-area');"
      , "  var previewImg = document.getElementById('text-photo-preview-img');"
      , "  var fileInput = document.getElementById('text-photo-file-input');"
      , "  "
      , "  previewImg.src = '';"
      , "  delete previewImg.dataset.base64;"
      , "  delete previewImg.dataset.mime;"
      , "  previewArea.style.display = 'none';"
      , "  fileInput.value = '';"
      , "})();"
      ]

  -- Submit handler
  let submit = do
        val <- get value inputEl

        -- Check for attached photo
        mBase64 <- callFunction $ ffi
          "(document.getElementById('text-photo-preview-img').dataset.base64 || '')"
        mMime <- callFunction $ ffi
          "(document.getElementById('text-photo-preview-img').dataset.mime || '')"

        let base64 = mBase64 :: String
            mime = mMime :: String
            hasPhoto = not (null base64)
            hasText = not (null val)

        -- Submit if we have text or photo (or both)
        when (hasText || hasPhoto) $ do
          if hasPhoto
            then liftIO $ safeSubmitResponse bridge
              (TextWithPhotoResponse (T.pack val) (T.pack base64) (T.pack mime))
            else liftIO $ safeSubmitResponse bridge
              (TextResponse (T.pack val))

          -- Clear input and photo
          void $ element inputEl # set value ""
          runFunction $ ffi $ T.unpack $ T.unlines
            [ "(function() {"
            , "  var previewArea = document.getElementById('text-photo-preview-area');"
            , "  var previewImg = document.getElementById('text-photo-preview-img');"
            , "  var fileInput = document.getElementById('text-photo-file-input');"
            , "  previewImg.src = '';"
            , "  delete previewImg.dataset.base64;"
            , "  delete previewImg.dataset.mime;"
            , "  previewArea.style.display = 'none';"
            , "  fileInput.value = '';"
            , "})();"
            ]

  on UI.click submitBtn $ const submit
  on UI.keydown inputEl $ \code ->
    when (code == 13) submit  -- Enter key

  void $ element container #+
    [ element promptEl
    , element previewArea
    , element inputRow
    ]

  -- Auto-focus the text input (deferred to ensure DOM is ready)
  runFunction $ ffi "setTimeout(function() { var el = document.getElementById('text-input-field'); if (el) el.focus(); }, 0)"

  pure container
