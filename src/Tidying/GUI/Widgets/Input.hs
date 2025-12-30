{-# LANGUAGE OverloadedStrings #-}
-- | Text input with optional camera photo attachment
--
-- Provides a chat-style input widget where users can type text
-- and optionally attach a photo using the device camera/webcam.
module Tidying.GUI.Widgets.Input
  ( textInputWithPhoto
  ) where

import Control.Monad (void, when)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import Tidepool.GUI.Core (GUIBridge(..), RequestResponse(..), safeSubmitResponse)

-- | Create a text input widget with optional camera photo attachment
--
-- Features:
-- - Text input field with submit button
-- - Camera button opens camera capture modal
-- - Live camera preview, capture, confirm/retake flow
-- - Inline photo preview with remove option after capture
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

  -- Camera modal (hidden by default)
  cameraModal <- UI.div #. "camera-modal"
    # set (attr "id") "camera-modal"
    # set style [("display", "none")]

  cameraModalContent <- UI.div #. "camera-modal-content"

  -- Camera video element
  cameraVideo <- UI.mkElement "video"
    #. "camera-video"
    # set (attr "id") "camera-modal-video"
    # set (attr "autoplay") "true"
    # set (attr "playsinline") "true"

  -- Canvas for capturing (hidden)
  cameraCanvas <- UI.mkElement "canvas"
    # set (attr "id") "camera-modal-canvas"
    # set style [("display", "none")]

  -- Camera preview image (shown after capture, hidden initially)
  cameraPreview <- UI.img #. "camera-preview"
    # set (attr "id") "camera-modal-preview"
    # set style [("display", "none")]

  -- Camera status
  cameraStatus <- UI.div #. "camera-status"
    # set (attr "id") "camera-modal-status"

  -- Camera controls (capture button)
  cameraControls <- UI.div #. "camera-controls"
    # set (attr "id") "camera-modal-controls"

  captureBtn <- UI.button #. "camera-capture-btn"
    # set (attr "id") "camera-modal-capture-btn"
    # set text "📷 Take Photo"

  cancelBtn <- UI.button #. "camera-cancel-btn"
    # set (attr "id") "camera-modal-cancel-btn"
    # set text "Cancel"

  void $ element cameraControls #+ [element captureBtn, element cancelBtn]

  -- Post-capture actions (hidden initially)
  cameraActions <- UI.div #. "camera-actions"
    # set (attr "id") "camera-modal-actions"
    # set style [("display", "none")]

  usePhotoBtn <- UI.button #. "camera-use-btn"
    # set (attr "id") "camera-modal-use-btn"
    # set text "Use this photo"

  retakeBtn <- UI.button #. "camera-retake-btn"
    # set (attr "id") "camera-modal-retake-btn"
    # set text "Retake"

  void $ element cameraActions #+ [element usePhotoBtn, element retakeBtn]

  void $ element cameraModalContent #+
    [ element cameraVideo
    , element cameraCanvas
    , element cameraPreview
    , element cameraStatus
    , element cameraControls
    , element cameraActions
    ]

  void $ element cameraModal #+ [element cameraModalContent]

  -- Input row: attach button + input field + submit button
  inputRow <- UI.div #. "text-input-row"

  -- Attach photo button (opens camera modal)
  attachBtn <- UI.button #. "attach-photo-btn"
    # set (attr "id") "text-attach-btn"
    # set (attr "title") "Take photo"
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

  -- Open camera modal when attach button clicked
  on UI.click attachBtn $ \_ ->
    runFunction $ ffi $ T.unpack openCameraModalScript

  -- Capture button - take snapshot
  on UI.click captureBtn $ \_ ->
    runFunction $ ffi $ T.unpack capturePhotoScript

  -- Cancel button - close modal
  on UI.click cancelBtn $ \_ ->
    runFunction $ ffi $ T.unpack closeCameraModalScript

  -- Use photo button - save to preview area and close modal
  on UI.click usePhotoBtn $ \_ ->
    runFunction $ ffi $ T.unpack usePhotoScript

  -- Retake button - go back to live camera
  on UI.click retakeBtn $ \_ ->
    runFunction $ ffi $ T.unpack retakePhotoScript

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
    , element cameraModal
    ]

  -- Auto-focus the text input
  runFunction $ ffi "setTimeout(function() { var el = document.getElementById('text-input-field'); if (el) el.focus(); }, 0)"

  pure container

-- | JavaScript to open camera modal and start camera
--
-- Fixes applied:
-- - 10-second timeout on getUserMedia
-- - Tracks pending state to handle close-during-init race
-- - Null checks on all elements
openCameraModalScript :: Text
openCameraModalScript = T.unlines
  [ "(function() {"
  , "  var modal = document.getElementById('camera-modal');"
  , "  var video = document.getElementById('camera-modal-video');"
  , "  var preview = document.getElementById('camera-modal-preview');"
  , "  var controls = document.getElementById('camera-modal-controls');"
  , "  var actions = document.getElementById('camera-modal-actions');"
  , "  var status = document.getElementById('camera-modal-status');"
  , "  "
  , "  // Validate all elements exist"
  , "  if (!modal || !video || !preview || !controls || !actions || !status) {"
  , "    console.error('Camera modal elements not found');"
  , "    return;"
  , "  }"
  , "  "
  , "  // Reset to camera mode"
  , "  video.style.display = 'block';"
  , "  preview.style.display = 'none';"
  , "  controls.style.display = 'flex';"
  , "  actions.style.display = 'none';"
  , "  "
  , "  // Show modal"
  , "  modal.style.display = 'flex';"
  , "  "
  , "  // Check for camera support"
  , "  if (!navigator.mediaDevices || !navigator.mediaDevices.getUserMedia) {"
  , "    status.textContent = 'Camera not supported in this browser';"
  , "    status.style.color = '#c44';"
  , "    return;"
  , "  }"
  , "  "
  , "  status.textContent = 'Starting camera...';"
  , "  status.style.color = '#888';"
  , "  "
  , "  // Track pending state for close-during-init race"
  , "  window.cameraModalPending = true;"
  , "  "
  , "  // Set up 10-second timeout"
  , "  var timeoutId = setTimeout(function() {"
  , "    if (window.cameraModalPending) {"
  , "      window.cameraModalPending = false;"
  , "      status.textContent = 'Camera request timed out';"
  , "      status.style.color = '#c44';"
  , "    }"
  , "  }, 10000);"
  , "  "
  , "  // Request camera (prefer back camera on mobile)"
  , "  var constraints = {"
  , "    video: {"
  , "      facingMode: { ideal: 'environment' },"
  , "      width: { ideal: 1280 },"
  , "      height: { ideal: 720 }"
  , "    }"
  , "  };"
  , "  "
  , "  navigator.mediaDevices.getUserMedia(constraints)"
  , "    .then(function(stream) {"
  , "      clearTimeout(timeoutId);"
  , "      "
  , "      // Check if modal was closed during init"
  , "      if (!window.cameraModalPending) {"
  , "        stream.getTracks().forEach(function(t) { t.stop(); });"
  , "        return;"
  , "      }"
  , "      window.cameraModalPending = false;"
  , "      "
  , "      video.srcObject = stream;"
  , "      window.cameraModalStream = stream;"
  , "      status.textContent = 'Camera ready';"
  , "      status.style.color = '#4a4';"
  , "    })"
  , "    .catch(function(err) {"
  , "      clearTimeout(timeoutId);"
  , "      window.cameraModalPending = false;"
  , "      console.error('Camera error:', err);"
  , "      status.textContent = 'Camera access denied';"
  , "      status.style.color = '#c44';"
  , "    });"
  , "})();"
  ]

-- | JavaScript to capture photo from camera
--
-- Fixes applied:
-- - Video readiness check (videoWidth > 0)
-- - Canvas error handling with try/catch
-- - Null checks on all elements
capturePhotoScript :: Text
capturePhotoScript = T.unlines
  [ "(function() {"
  , "  var video = document.getElementById('camera-modal-video');"
  , "  var canvas = document.getElementById('camera-modal-canvas');"
  , "  var preview = document.getElementById('camera-modal-preview');"
  , "  var controls = document.getElementById('camera-modal-controls');"
  , "  var actions = document.getElementById('camera-modal-actions');"
  , "  var status = document.getElementById('camera-modal-status');"
  , "  var captureBtn = document.getElementById('camera-modal-capture-btn');"
  , "  "
  , "  // Validate all elements exist"
  , "  if (!video || !canvas || !preview || !controls || !actions || !status) {"
  , "    console.error('Camera capture elements not found');"
  , "    return;"
  , "  }"
  , "  "
  , "  // Check if video is ready"
  , "  if (video.videoWidth === 0 || video.videoHeight === 0) {"
  , "    status.textContent = 'Camera not ready yet, please wait...';"
  , "    status.style.color = '#c44';"
  , "    return;"
  , "  }"
  , "  "
  , "  // Disable button during capture to prevent race"
  , "  if (captureBtn) captureBtn.disabled = true;"
  , "  "
  , "  try {"
  , "    // Set canvas size to match video"
  , "    canvas.width = video.videoWidth;"
  , "    canvas.height = video.videoHeight;"
  , "    "
  , "    // Draw current frame"
  , "    var ctx = canvas.getContext('2d');"
  , "    ctx.drawImage(video, 0, 0);"
  , "    "
  , "    // Convert to data URL"
  , "    var dataUrl = canvas.toDataURL('image/jpeg', 0.85);"
  , "    "
  , "    // Validate data URL"
  , "    if (!dataUrl || !dataUrl.includes(',')) {"
  , "      throw new Error('Invalid canvas data');"
  , "    }"
  , "    "
  , "    var base64 = dataUrl.split(',')[1];"
  , "    if (!base64 || base64.length < 100) {"
  , "      throw new Error('Captured image is empty or too small');"
  , "    }"
  , "    "
  , "    preview.src = dataUrl;"
  , "    preview.dataset.base64 = base64;"
  , "    "
  , "    // Switch to preview mode"
  , "    video.style.display = 'none';"
  , "    preview.style.display = 'block';"
  , "    controls.style.display = 'none';"
  , "    actions.style.display = 'flex';"
  , "    status.textContent = 'Photo captured';"
  , "    status.style.color = '#4a4';"
  , "  } catch (e) {"
  , "    console.error('Capture error:', e);"
  , "    status.textContent = 'Failed to capture: ' + e.message;"
  , "    status.style.color = '#c44';"
  , "  } finally {"
  , "    if (captureBtn) captureBtn.disabled = false;"
  , "  }"
  , "})();"
  ]

-- | JavaScript to close camera modal and stop camera
--
-- Fixes applied:
-- - Cancels pending getUserMedia via cameraModalPending flag
-- - Null checks on all elements
closeCameraModalScript :: Text
closeCameraModalScript = T.unlines
  [ "(function() {"
  , "  var modal = document.getElementById('camera-modal');"
  , "  var video = document.getElementById('camera-modal-video');"
  , "  "
  , "  // Cancel any pending getUserMedia request"
  , "  window.cameraModalPending = false;"
  , "  "
  , "  // Stop camera stream"
  , "  if (window.cameraModalStream) {"
  , "    window.cameraModalStream.getTracks().forEach(function(track) {"
  , "      track.stop();"
  , "    });"
  , "    window.cameraModalStream = null;"
  , "  }"
  , "  "
  , "  // Clear video source"
  , "  if (video) video.srcObject = null;"
  , "  "
  , "  // Hide modal"
  , "  if (modal) modal.style.display = 'none';"
  , "})();"
  ]

-- | JavaScript to use captured photo (save to preview and close modal)
--
-- Fixes applied:
-- - Validates base64 exists before copying
-- - Proper cleanup of camera stream
usePhotoScript :: Text
usePhotoScript = T.unlines
  [ "(function() {"
  , "  var modalPreview = document.getElementById('camera-modal-preview');"
  , "  var previewArea = document.getElementById('text-photo-preview-area');"
  , "  var previewImg = document.getElementById('text-photo-preview-img');"
  , "  "
  , "  if (!modalPreview || !previewArea || !previewImg) {"
  , "    console.error('Photo preview elements not found');"
  , "    return;"
  , "  }"
  , "  "
  , "  // Validate we have a photo to use"
  , "  var base64 = modalPreview.dataset.base64;"
  , "  if (!base64 || base64.length < 100) {"
  , "    console.error('No valid photo to use');"
  , "    return;"
  , "  }"
  , "  "
  , "  // Copy photo to main preview area"
  , "  previewImg.src = modalPreview.src;"
  , "  previewImg.dataset.base64 = base64;"
  , "  previewImg.dataset.mime = 'image/jpeg';"
  , "  previewArea.style.display = 'flex';"
  , "  "
  , "  // Cancel pending and close modal"
  , "  window.cameraModalPending = false;"
  , "  "
  , "  var modal = document.getElementById('camera-modal');"
  , "  var video = document.getElementById('camera-modal-video');"
  , "  "
  , "  if (window.cameraModalStream) {"
  , "    window.cameraModalStream.getTracks().forEach(function(track) {"
  , "      track.stop();"
  , "    });"
  , "    window.cameraModalStream = null;"
  , "  }"
  , "  "
  , "  if (video) video.srcObject = null;"
  , "  if (modal) modal.style.display = 'none';"
  , "})();"
  ]

-- | JavaScript to retake photo (back to live camera)
--
-- Fixes applied:
-- - Null checks on all elements
retakePhotoScript :: Text
retakePhotoScript = T.unlines
  [ "(function() {"
  , "  var video = document.getElementById('camera-modal-video');"
  , "  var preview = document.getElementById('camera-modal-preview');"
  , "  var controls = document.getElementById('camera-modal-controls');"
  , "  var actions = document.getElementById('camera-modal-actions');"
  , "  var status = document.getElementById('camera-modal-status');"
  , "  "
  , "  // Validate all elements exist"
  , "  if (!video || !preview || !controls || !actions || !status) {"
  , "    console.error('Retake elements not found');"
  , "    return;"
  , "  }"
  , "  "
  , "  // Clear captured photo"
  , "  preview.style.display = 'none';"
  , "  preview.src = '';"
  , "  delete preview.dataset.base64;"
  , "  "
  , "  // Switch back to camera"
  , "  video.style.display = 'block';"
  , "  controls.style.display = 'flex';"
  , "  actions.style.display = 'none';"
  , "  status.textContent = 'Camera ready';"
  , "  status.style.color = '#4a4';"
  , "})();"
  ]

-- | JavaScript to remove attached photo
removePhotoScript :: Text
removePhotoScript = T.unlines
  [ "(function() {"
  , "  var previewArea = document.getElementById('text-photo-preview-area');"
  , "  var previewImg = document.getElementById('text-photo-preview-img');"
  , "  "
  , "  if (previewImg) {"
  , "    previewImg.src = '';"
  , "    delete previewImg.dataset.base64;"
  , "    delete previewImg.dataset.mime;"
  , "  }"
  , "  if (previewArea) previewArea.style.display = 'none';"
  , "  "
  , "  // Also clean up any active camera streams"
  , "  window.cameraModalPending = false;"
  , "  if (window.cameraModalStream) {"
  , "    window.cameraModalStream.getTracks().forEach(function(track) {"
  , "      track.stop();"
  , "    });"
  , "    window.cameraModalStream = null;"
  , "  }"
  , "})();"
  ]
