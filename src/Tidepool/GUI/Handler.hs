-- | GUI-based InputHandler implementation
--
-- This module provides an 'InputHandler' that bridges to the GUI
-- via the 'GUIBridge' types. When input is requested, it posts
-- to the bridge and blocks until the GUI responds.
module Tidepool.GUI.Handler
  ( makeGUIHandler
  , guiDice
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
  , ihDice = guiDice bridge
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
    ChoiceResponse _ -> error "Expected TextResponse, got ChoiceResponse"

-- | Handle a dice selection request via the GUI
--
-- Shows dice cards with LLM-generated hints for each outcome.
-- Returns the selected die's index.
--
-- Example usage:
--
-- @
-- -- In your game loop
-- let dicePool = [(4, 0, "Guard spots you"), (2, 1, "Clean escape"), (6, 2, "Perfect timing")]
-- selectedIdx <- guiDice bridge "Choose a die from your pool:" dicePool
-- @
guiDice :: GUIBridge state -> Text -> [(Int, Int, Text)] -> IO Int
guiDice bridge prompt diceWithHints = do
  -- Hide spinner while waiting for player input
  atomically $ writeTVar bridge.gbLLMActive False

  -- Post the dice request to the GUI (includes hints for display)
  atomically $ writeTVar bridge.gbPendingRequest
    (Just $ PendingDice prompt diceWithHints)

  -- Block until GUI responds
  response <- takeMVar bridge.gbRequestResponse

  -- Clear the pending request and re-enable spinner (LLM continues processing)
  atomically $ do
    writeTVar bridge.gbPendingRequest Nothing
    writeTVar bridge.gbLLMActive True

  -- Return the selected index
  case response of
    ChoiceResponse idx -> pure idx
    TextResponse _ -> error "Expected ChoiceResponse, got TextResponse"

-- | Handle a custom request via the GUI
--
-- Routes custom requests based on tag. Currently supports:
-- - "character-creation": Uses gbCharacterCreationResult MVar
--
-- Unknown tags return an error value.
--
-- ════════════════════════════════════════════════════════════════════════════
-- FIXME: MAKE GENERIC
-- ════════════════════════════════════════════════════════════════════════════
-- This is a stopgap. The "character-creation" tag is DM-specific and shouldn't
-- be hardcoded here. Proper solution:
--
--   1. GUIBridge gets a generic `gbCustomResult :: MVar Value`
--   2. PendingRequest gets `PendingCustom Text Value` (tag + payload)
--   3. GUI routes based on tag, any agent can define custom UI flows
--   4. Remove DM-specific gbCharacterCreationResult
--
-- For now, this works because DM is the only agent using custom requests.
-- ════════════════════════════════════════════════════════════════════════════
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
