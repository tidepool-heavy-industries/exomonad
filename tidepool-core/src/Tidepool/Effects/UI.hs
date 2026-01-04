-- | UI effect for user interface interactions.
--
-- Effect type only - executors live in tidepool-ui-executor.
-- This effect bridges graph execution to UI state via wire types.
module Tidepool.Effects.UI
  ( -- * Effect
    UI(..)
  , showText
  , requestTextInput
  , requestPhotoInput
  , requestChoice
  , setThinking
  ) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Control.Monad.Freer (Eff, Member, send)


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | UI effect for displaying content and requesting user input.
--
-- The executor translates these to wire types (UIState/UserAction).
data UI r where
  -- | Display text to the user (appended to chat log).
  ShowText :: Text -> UI ()

  -- | Request text input from user with a prompt.
  RequestTextInput :: Text -> UI Text

  -- | Request photo input from user with a prompt.
  RequestPhotoInput :: Text -> UI ByteString

  -- | Present choices to user, return selected index.
  RequestChoice :: Text -> [(Text, a)] -> UI a

  -- | Set the "thinking" indicator state.
  SetThinking :: Bool -> UI ()


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Display text to the user.
showText :: Member UI effs => Text -> Eff effs ()
showText = send . ShowText

-- | Request text input with a prompt.
requestTextInput :: Member UI effs => Text -> Eff effs Text
requestTextInput = send . RequestTextInput

-- | Request photo input with a prompt.
requestPhotoInput :: Member UI effs => Text -> Eff effs ByteString
requestPhotoInput = send . RequestPhotoInput

-- | Present choices and get user selection.
requestChoice :: Member UI effs => Text -> [(Text, a)] -> Eff effs a
requestChoice prompt choices = send (RequestChoice prompt choices)

-- | Set thinking indicator.
setThinking :: Member UI effs => Bool -> Eff effs ()
setThinking = send . SetThinking
