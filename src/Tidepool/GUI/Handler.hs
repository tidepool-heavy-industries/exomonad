-- | GUI-based InputHandler implementation
--
-- This module provides an 'InputHandler' that bridges to the GUI
-- via the 'GUIBridge' types. When input is requested, it posts
-- to the bridge and blocks until the GUI responds.
--
-- = Dice Support
--
-- The 'guiDice' function provides dice selection via the GUI. Since
-- dice selection is rendered differently (with tier colors based on
-- Position), it uses a separate 'PendingDice' request type.
--
-- To fully integrate dice into the effect system, add to Effect.hs:
--
-- @
-- data InputHandler = InputHandler
--   { ihChoice :: forall a. Text -> [(Text, a)] -> IO a
--   , ihText   :: Text -> IO Text
--   , ihDice   :: Text -> [(Int, Int)] -> IO Int  -- (die value, index)
--   }
-- @
--
-- Until then, use 'guiDice' directly from game logic.
module Tidepool.GUI.Handler
  ( makeGUIHandler
    -- * Direct dice support
  , guiDice
  ) where

import Control.Concurrent.MVar (takeMVar)
import Control.Concurrent.STM (atomically, writeTVar)
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
  }

-- | Handle a choice request via the GUI
guiChoice :: GUIBridge state -> Text -> [(Text, a)] -> IO a
guiChoice bridge prompt options = do
  -- Create indexed version for GUI (we need to map indices back to values)
  let indexed = zip [0..] (map fst options)
      indexedOptions = [(label, idx) | (idx, label) <- indexed]

  -- Post the request to the GUI
  atomically $ writeTVar bridge.gbPendingRequest
    (Just $ PendingChoice prompt indexedOptions)

  -- Block until GUI responds
  response <- takeMVar bridge.gbRequestResponse

  -- Clear the pending request
  atomically $ writeTVar bridge.gbPendingRequest Nothing

  -- Return the value at the selected index
  case response of
    ChoiceResponse idx -> pure (snd (options !! idx))
    TextResponse _ -> error "Expected ChoiceResponse, got TextResponse"

-- | Handle a text input request via the GUI
guiText :: GUIBridge state -> Text -> IO Text
guiText bridge prompt = do
  -- Post the request to the GUI
  atomically $ writeTVar bridge.gbPendingRequest
    (Just $ PendingText prompt)

  -- Block until GUI responds
  response <- takeMVar bridge.gbRequestResponse

  -- Clear the pending request
  atomically $ writeTVar bridge.gbPendingRequest Nothing

  -- Return the text
  case response of
    TextResponse txt -> pure txt
    ChoiceResponse _ -> error "Expected TextResponse, got ChoiceResponse"

-- | Handle a dice selection request via the GUI
--
-- Shows dice cards with outcome tier colors based on Position (stored in game state).
-- Returns the selected die's index.
--
-- Example usage:
--
-- @
-- -- In your game loop
-- let dicePool = [(4, 0), (2, 1), (6, 2)]  -- (die value, index) pairs
-- selectedIdx <- guiDice bridge "Choose a die from your pool:" dicePool
-- @
guiDice :: GUIBridge state -> Text -> [(Int, Int)] -> IO Int
guiDice bridge prompt diceWithIndices = do
  -- Post the dice request to the GUI
  atomically $ writeTVar bridge.gbPendingRequest
    (Just $ PendingDice prompt diceWithIndices)

  -- Block until GUI responds
  response <- takeMVar bridge.gbRequestResponse

  -- Clear the pending request
  atomically $ writeTVar bridge.gbPendingRequest Nothing

  -- Return the selected index
  case response of
    ChoiceResponse idx -> pure idx
    TextResponse _ -> error "Expected ChoiceResponse, got TextResponse"
