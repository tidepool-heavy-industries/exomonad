-- | GUI-based InputHandler implementation
--
-- This module provides an 'InputHandler' that bridges to the GUI
-- via the 'GUIBridge' types. When input is requested, it posts
-- to the bridge and blocks until the GUI responds.
module Tidepool.GUI.Handler
  ( makeGUIHandler
  ) where

import Control.Concurrent.MVar (putMVar, takeMVar)
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
  atomically $ writeTVar (gbPendingRequest bridge)
    (Just $ PendingChoice prompt indexedOptions)

  -- Block until GUI responds
  response <- takeMVar (gbRequestResponse bridge)

  -- Clear the pending request
  atomically $ writeTVar (gbPendingRequest bridge) Nothing

  -- Return the value at the selected index
  case response of
    ChoiceResponse idx -> pure (snd (options !! idx))
    TextResponse _ -> error "Expected ChoiceResponse, got TextResponse"

-- | Handle a text input request via the GUI
guiText :: GUIBridge state -> Text -> IO Text
guiText bridge prompt = do
  -- Post the request to the GUI
  atomically $ writeTVar (gbPendingRequest bridge)
    (Just $ PendingText prompt)

  -- Block until GUI responds
  response <- takeMVar (gbRequestResponse bridge)

  -- Clear the pending request
  atomically $ writeTVar (gbPendingRequest bridge) Nothing

  -- Return the text
  case response of
    TextResponse txt -> pure txt
    ChoiceResponse _ -> error "Expected TextResponse, got ChoiceResponse"

-- | Respond to a pending choice request
--
-- This is called by the GUI when the user clicks a choice card.
respondChoice :: GUIBridge state -> Int -> IO ()
respondChoice bridge idx =
  putMVar (gbRequestResponse bridge) (ChoiceResponse idx)

-- | Respond to a pending text request
--
-- This is called by the GUI when the user submits text.
respondText :: GUIBridge state -> Text -> IO ()
respondText bridge txt =
  putMVar (gbRequestResponse bridge) (TextResponse txt)
