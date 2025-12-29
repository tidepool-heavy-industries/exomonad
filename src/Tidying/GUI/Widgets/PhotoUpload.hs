{-# LANGUAGE OverloadedStrings #-}
-- | Photo upload widget for the Tidying GUI
--
-- Provides a file input that reads images as base64 data URLs,
-- shows a preview, and submits via the GUIBridge.
module Tidying.GUI.Widgets.PhotoUpload
  ( photoUploadWidget
  ) where

import Control.Monad (void, when)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Tidepool.GUI.Core (GUIBridge(..), RequestResponse(..), safeSubmitResponse)

-- | Create a photo upload widget
--
-- Shows a file input with preview. When the user selects a file,
-- it's read as base64 and displayed. Clicking "Use this photo"
-- submits the PhotoResponse.
--
-- NOTE: Uses hardcoded element IDs (photo-file-input, photo-preview, etc.)
-- so only one photo upload widget can exist on a page at a time.
photoUploadWidget :: GUIBridge state -> Text -> UI Element
photoUploadWidget bridge prompt = do
  container <- UI.div #. "photo-upload-container"

  -- Prompt text
  promptEl <- UI.div #. "photo-upload-prompt"
    # set text (T.unpack prompt)

  -- Hidden file input
  fileInput <- UI.input
    # set (attr "type") "file"
    # set (attr "accept") "image/*"
    # set (attr "id") "photo-file-input"
    # set style [("display", "none")]

  -- Clickable dropzone area
  dropzone <- UI.div #. "photo-upload-dropzone"
    # set (attr "id") "photo-dropzone"

  -- Icon and text in dropzone
  iconEl <- UI.div #. "photo-upload-icon"
    # set html "<i class=\"fas fa-camera\"></i>"

  textEl <- UI.div #. "photo-upload-text"
    # set text "Click to select a photo"

  void $ element dropzone #+ [element iconEl, element textEl]

  -- Preview image (hidden initially)
  preview <- UI.img #. "photo-upload-preview"
    # set (attr "id") "photo-preview"
    # set style [("display", "none")]

  -- Action buttons (hidden initially)
  actionsEl <- UI.div #. "photo-upload-actions"
    # set (attr "id") "photo-actions"
    # set style [("display", "none")]

  submitBtn <- UI.button #. "photo-upload-btn"
    # set text "Use this photo"
    # set (attr "id") "photo-submit-btn"

  cancelBtn <- UI.button #. "photo-upload-btn photo-upload-btn-secondary"
    # set text "Choose different"
    # set (attr "id") "photo-cancel-btn"

  void $ element actionsEl #+ [element submitBtn, element cancelBtn]

  -- Click dropzone to trigger file input
  on UI.click dropzone $ \_ ->
    runFunction $ ffi "document.getElementById('photo-file-input').click()"

  -- Set up file input change handler via JavaScript
  -- (threepenny doesn't export 'change' event, so we use FFI)
  runFunction $ ffi $ T.unpack $ T.unlines
    [ "document.getElementById('photo-file-input').addEventListener('change', function() {"
    , "  var input = this;"
    , "  var file = input.files[0];"
    , "  if (!file) return;"
    , "  "
    , "  var reader = new FileReader();"
    , "  reader.onload = function(e) {"
    , "    var dataUrl = e.target.result;"
    , "    var preview = document.getElementById('photo-preview');"
    , "    var dropzone = document.getElementById('photo-dropzone');"
    , "    var actions = document.getElementById('photo-actions');"
    , "    "
    , "    preview.src = dataUrl;"
    , "    preview.style.display = 'block';"
    , "    dropzone.style.display = 'none';"
    , "    actions.style.display = 'flex';"
    , "    "
    , "    // Store data for later submission"
    , "    preview.dataset.base64 = dataUrl.split(',')[1];"
    , "    preview.dataset.mime = file.type;"
    , "  };"
    , "  reader.readAsDataURL(file);"
    , "});"
    ]

  -- Submit button - extract data from preview and send response
  on UI.click submitBtn $ \_ -> do
    -- Get data from preview element's dataset
    mBase64 <- callFunction $ ffi "document.getElementById('photo-preview').dataset.base64"
    mMime <- callFunction $ ffi "document.getElementById('photo-preview').dataset.mime"

    let base64 = mBase64 :: String
        mime = mMime :: String

    when (not (null base64)) $ do
      liftIO $ safeSubmitResponse bridge
        (PhotoResponse (T.pack base64) (T.pack mime))

  -- Cancel button - reset to initial state
  on UI.click cancelBtn $ \_ -> do
    runFunction $ ffi $ T.unpack $ T.unlines
      [ "(function() {"
      , "  var preview = document.getElementById('photo-preview');"
      , "  var dropzone = document.getElementById('photo-dropzone');"
      , "  var actions = document.getElementById('photo-actions');"
      , "  var input = document.getElementById('photo-file-input');"
      , "  "
      , "  preview.src = '';"
      , "  preview.style.display = 'none';"
      , "  dropzone.style.display = 'block';"
      , "  actions.style.display = 'none';"
      , "  input.value = '';"  -- Reset file input
      , "})();"
      ]

  void $ element container #+
    [ element promptEl
    , element fileInput
    , element dropzone
    , element preview
    , element actionsEl
    ]

  pure container
