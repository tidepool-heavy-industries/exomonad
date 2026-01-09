-- | GUI-based InputHandler implementation
--
-- This module provides an 'InputHandler' that bridges to the GUI
-- via the 'GUIBridge' types. When input is requested, it posts
-- to the bridge and blocks until the GUI responds.
module Tidepool.GUI.Handler
  ( makeGUIHandler
  , guiPhoto
  , guiTextWithPhoto
  , guiCustom
  ) where

import Control.Concurrent.MVar (takeMVar)
import Control.Concurrent.STM (atomically, writeTVar)
import Data.Aeson (Value)
import Data.Text (Text)

import Tidepool.Effect (InputHandler(..))
import Tidepool.GUI.Core

-- | Create an InputHandler that bridges to the GUI
--
-- When 'requestChoice' or 'requestText' is called in the game loop,
-- this handler posts the request to the bridge's TVar and blocks
-- on the MVar until the GUI responds.
makeGUIHandler :: GUIBridge state -> InputHandler
makeGUIHandler bridge = InputHandler
  { ihChoice = guiChoice bridge
  , ihText = guiText bridge
  , ihTextWithPhoto = guiTextWithPhoto bridge
  , ihCustom = guiCustom bridge
  }

-- | Handle a choice request via the GUI
guiChoice :: GUIBridge state -> Text -> [(Text, a)] -> IO a
guiChoice bridge prompt options = do
  -- Create indexed version for GUI (we need to map indices back to values)
  let indexed = zip [0..] (map fst options)
      indexedOptions = [(label, idx) | (idx, label) <- indexed]

  -- Hide spinner while waiting for player input
  atomically $ writeTVar bridge.gbLLMActive False

  -- Post the request to the GUI
  atomically $ writeTVar bridge.gbPendingRequest
    (Just $ PendingChoice prompt indexedOptions)

  -- Block until GUI responds
  response <- takeMVar bridge.gbRequestResponse

  -- Clear the pending request and re-enable spinner (LLM continues processing)
  atomically $ do
    writeTVar bridge.gbPendingRequest Nothing
    writeTVar bridge.gbLLMActive True

  -- Return the value at the selected index (with bounds check)
  case response of
    ChoiceResponse idx
      | idx >= 0 && idx < length options -> pure (snd (options !! idx))
      | otherwise -> error $ "Choice index out of bounds: " ++ show idx
                          ++ " (valid: 0-" ++ show (length options - 1) ++ ")"
    TextResponse _ -> error "Expected ChoiceResponse, got TextResponse"
    PhotoResponse _ _ -> error "Expected ChoiceResponse, got PhotoResponse"
    TextWithPhotoResponse {} -> error "Expected ChoiceResponse, got TextWithPhotoResponse"
    CustomResponse _ -> error "Expected ChoiceResponse, got CustomResponse"

-- | Handle a text input request via the GUI
guiText :: GUIBridge state -> Text -> IO Text
guiText bridge prompt = do
  -- Hide spinner while waiting for player input
  atomically $ writeTVar bridge.gbLLMActive False

  -- Post the request to the GUI
  atomically $ writeTVar bridge.gbPendingRequest
    (Just $ PendingText prompt)

  -- Block until GUI responds
  response <- takeMVar bridge.gbRequestResponse

  -- Clear the pending request and re-enable spinner (LLM continues processing)
  atomically $ do
    writeTVar bridge.gbPendingRequest Nothing
    writeTVar bridge.gbLLMActive True

  -- Return the text
  case response of
    TextResponse txt -> pure txt
    TextWithPhotoResponse txt _ _ -> pure txt  -- Ignore photo, return just text
    ChoiceResponse _ -> error "Expected TextResponse, got ChoiceResponse"
    PhotoResponse _ _ -> error "Expected TextResponse, got PhotoResponse"
    CustomResponse _ -> error "Expected TextResponse, got CustomResponse"

-- | Handle a photo upload request via the GUI
--
-- Shows a file input for selecting an image. Returns the photo data
-- as a (base64 data, mime type) tuple.
--
-- Example usage:
--
-- @
-- -- In your tidying agent loop
-- (photoData, mimeType) <- guiPhoto bridge "Please take a photo of the space you want to tidy:"
-- let photo = Photo photoData mimeType
-- @
guiPhoto :: GUIBridge state -> Text -> IO (Text, Text)
guiPhoto bridge prompt = do
  -- Hide spinner while waiting for player input
  atomically $ writeTVar bridge.gbLLMActive False

  -- Post the photo request to the GUI
  atomically $ writeTVar bridge.gbPendingRequest
    (Just $ PendingPhoto prompt)

  -- Block until GUI responds
  response <- takeMVar bridge.gbRequestResponse

  -- Clear the pending request and re-enable spinner (LLM continues processing)
  atomically $ do
    writeTVar bridge.gbPendingRequest Nothing
    writeTVar bridge.gbLLMActive True

  -- Return the photo data and mime type
  case response of
    PhotoResponse photoData mimeType -> pure (photoData, mimeType)
    TextWithPhotoResponse _ photoData mimeType -> pure (photoData, mimeType)
    ChoiceResponse _ -> error "Expected PhotoResponse, got ChoiceResponse"
    TextResponse _ -> error "Expected PhotoResponse, got TextResponse"
    CustomResponse _ -> error "Expected PhotoResponse, got CustomResponse"

-- | Handle a text input request with optional photo attachment
--
-- Uses a text input widget that also allows attaching a photo.
-- Returns the text and a list of photos (0 or 1).
--
-- Example usage:
--
-- @
-- -- In your agent loop
-- (text, photos) <- guiTextWithPhoto bridge "Tell me about your space:"
-- let userInput = UserInput { inputText = Just text, inputPhotos = photos }
-- @
guiTextWithPhoto :: GUIBridge state -> Text -> IO (Text, [(Text, Text)])
guiTextWithPhoto bridge prompt = do
  -- Hide spinner while waiting for player input
  atomically $ writeTVar bridge.gbLLMActive False

  -- Post the request to the GUI
  -- We reuse PendingText - the GUI decides which widget to show
  atomically $ writeTVar bridge.gbPendingRequest
    (Just $ PendingText prompt)

  -- Block until GUI responds
  response <- takeMVar bridge.gbRequestResponse

  -- Clear the pending request and re-enable spinner
  atomically $ do
    writeTVar bridge.gbPendingRequest Nothing
    writeTVar bridge.gbLLMActive True

  -- Return text and optional photo
  case response of
    TextResponse txt -> pure (txt, [])
    TextWithPhotoResponse txt photoData mimeType -> pure (txt, [(photoData, mimeType)])
    ChoiceResponse _ -> error "Expected TextResponse, got ChoiceResponse"
    PhotoResponse _ _ -> error "Expected TextResponse, got PhotoResponse"
    CustomResponse _ -> error "Expected TextResponse, got CustomResponse"

-- | Handle a custom request via the GUI
--
-- Routes custom requests based on tag. Currently supports:
-- - "character-creation": Uses gbCharacterCreationResult MVar
--
-- Unknown tags return an error value.
guiCustom :: GUIBridge state -> Text -> Value -> IO Value
guiCustom bridge tag _payload = case tag of
  "character-creation" -> do
    -- Post character creation request to GUI
    atomically $ writeTVar bridge.gbPendingRequest (Just PendingCharacterCreation)

    -- Block until character creation completes
    result <- takeMVar bridge.gbCharacterCreationResult

    -- Clear pending request
    atomically $ writeTVar bridge.gbPendingRequest Nothing

    pure result

  _ -> error $ "Unknown custom request tag: " ++ show tag
