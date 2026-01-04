-- | Echo agent - simple test agent for verifying the effect stack.
--
-- This agent demonstrates the full effect composition working:
-- - Uses UI effects (showText, requestTextInput)
-- - Runs through the complete effect stack
-- - Proves WebSocket <-> Effect bridging works
module Tidepool.Server.Echo
  ( echoAgent
  ) where

import Control.Monad (forever)
import Control.Monad.Freer (Eff, Member)
import Data.Text (Text)

import Tidepool.Effects.UI (UI, showText, requestTextInput)


-- | Simple echo agent using UI effects.
--
-- This tests the full effect composition pipeline by:
-- 1. Sending a greeting via showText
-- 2. Requesting text input from the user
-- 3. Echoing the input back
-- 4. Repeating forever
--
-- = Usage
--
-- @
-- result <- runEffects env ctx callback echoAgent
-- @
echoAgent :: Member UI effs => Eff effs ()
echoAgent = do
  showText "Welcome to the Echo Agent! Type something and I'll echo it back."
  echoLoop

-- | The echo loop - request input and echo it back.
echoLoop :: Member UI effs => Eff effs ()
echoLoop = forever $ do
  input <- requestTextInput "Type a message..."
  showText $ "Echo: " <> input
