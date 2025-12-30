{-# LANGUAGE OverloadedStrings #-}
-- | Text input with optional photo attachment
--
-- Provides a chat-style input widget where users can type text
-- and optionally attach a photo using file picker (which on mobile
-- integrates with the device camera).
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
-- - Photo button opens file picker (on mobile, offers camera option)
-- - Inline photo preview with remove option after selection
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

  -- Hidden file input for photo selection (works on HTTP, integrates with mobile camera)
  fileInput <- UI.input #. "photo-file-input"
    # set (attr "id") "text-file-input"
    # set (attr "type") "file"
    # set (attr "accept") "image/*"
    # set (attr "capture") "environment"  -- Prefer back camera on mobile
    # set style [("display", "none")]

  -- Attach photo button (opens file picker, which on mobile offers camera)
  attachBtn <- UI.button #. "attach-photo-btn"
    # set (attr "id") "text-attach-btn"
    # set (attr "title") "Add photo"
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
    [ element attachBtn
    , element inputEl
    , element submitBtn
    ]

  -- Attach button clicks the hidden file input
  on UI.click attachBtn $ \_ ->
    runFunction $ ffi "document.getElementById('text-file-input').click()"

  -- Register change handler for file input via FFI
  runFunction $ ffi $ T.unpack registerFileChangeHandlerScript

  -- Remove button - clear attached photo
  on UI.click removeBtn $ \_ ->
    runFunction $ ffi $ T.unpack removePhotoScript

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
          runFunction $ ffi $ T.unpack removePhotoScript

  on UI.click submitBtn $ const submit
  on UI.keydown inputEl $ \code ->
    when (code == 13) submit  -- Enter key

  void $ element container #+
    [ element promptEl
    , element previewArea
    , element inputRow
    , element fileInput
    ]

  -- Auto-focus the text input
  runFunction $ ffi "setTimeout(function() { var el = document.getElementById('text-input-field'); if (el) el.focus(); }, 0)"

  pure container

-- | JavaScript to remove attached photo
removePhotoScript :: Text
removePhotoScript = T.unlines
  [ "(function() {"
  , "  var previewArea = document.getElementById('text-photo-preview-area');"
  , "  var previewImg = document.getElementById('text-photo-preview-img');"
  , "  var fileInput = document.getElementById('text-file-input');"
  , "  "
  , "  if (previewImg) {"
  , "    previewImg.src = '';"
  , "    delete previewImg.dataset.base64;"
  , "    delete previewImg.dataset.mime;"
  , "  }"
  , "  if (previewArea) previewArea.style.display = 'none';"
  , "  if (fileInput) fileInput.value = '';"
  , "})();"
  ]

-- | JavaScript to register file input change handler
--
-- Sets up the change event listener once when the widget is created.
-- Reads the selected file as base64 and shows in preview area.
-- Uses setTimeout to defer until after elements are in DOM.
registerFileChangeHandlerScript :: Text
registerFileChangeHandlerScript = T.unlines
  [ "(function() {"
  , "  // Retry until element exists (up to 2 seconds)"
  , "  var attempts = 0;"
  , "  var maxAttempts = 20;"
  , "  function tryRegister() {"
  , "    var fileInput = document.getElementById('text-file-input');"
  , "    if (!fileInput) {"
  , "      if (++attempts < maxAttempts) setTimeout(tryRegister, 100);"
  , "      return;"
  , "    }"
  , "    "
  , "    fileInput.addEventListener('change', function() {"
  , "      var previewArea = document.getElementById('text-photo-preview-area');"
  , "      var previewImg = document.getElementById('text-photo-preview-img');"
  , "      if (!previewArea || !previewImg) return;"
  , "      "
  , "      var file = fileInput.files[0];"
  , "      if (!file || !file.type.startsWith('image/')) return;"
  , "      "
  , "      var reader = new FileReader();"
  , "      reader.onload = function(e) {"
  , "        var dataUrl = e.target.result;"
  , "        if (!dataUrl || !dataUrl.includes(',')) return;"
  , "        "
  , "        previewImg.src = dataUrl;"
  , "        previewImg.dataset.base64 = dataUrl.split(',')[1];"
  , "        previewImg.dataset.mime = file.type || 'image/jpeg';"
  , "        previewArea.style.display = 'flex';"
  , "      };"
  , "      reader.readAsDataURL(file);"
  , "    });"
  , "  }"
  , "  tryRegister();"
  , "})();"
  ]
