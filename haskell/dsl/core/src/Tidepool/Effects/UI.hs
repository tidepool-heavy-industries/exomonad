-- | UI effect for user interface interactions.
--
-- Effect type only - interpreters live in tidepool-ui-interpreter.
-- This effect bridges graph execution to UI state via wire types.
--
-- == Core Primitives
--
-- * 'requestChoice' - Single selection from options (returns typed value)
-- * 'requestMultiChoice' - Multiple selection (checkboxes)
-- * 'requestChoiceDesc' - Single selection with descriptions
-- * 'requestChoiceMeta' - Single selection with full metadata (costs, disabled states)
--
-- == Type Safety
--
-- All choice primitives use 'NonEmpty' to prevent empty choice lists at compile time.
-- The handler manages the index-to-value mapping internally - callers get typed values back.
--
-- @
-- data BargainChoice = AcceptDeal | Retreat | PassOut
--
-- choice <- requestChoice "You're out of dice:" $ NE.fromList
--   [ ("Accept the devil's bargain", AcceptDeal)
--   , ("Retreat to safety", Retreat)
--   , ("Pass out from exhaustion", PassOut)
--   ]
-- -- choice :: BargainChoice
-- @
module Tidepool.Effects.UI
  ( -- * Effect
    UI(..)
  , showText
  , requestTextInput
  , requestPhotoInput
  , requestChoice
  , requestMultiChoice
  , requestChoiceDesc
  , requestChoiceMeta
  , setThinking
    -- * Rich Choice Metadata
  , ChoiceAvailability(..)
  , ChoiceMeta(..)
    -- * Convenience Helpers
  , confirm
  , requestDie
  , selectMultiple
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Control.Monad.Freer (Eff, Member, send)
import GHC.Generics (Generic)


-- ════════════════════════════════════════════════════════════════════════════
-- CHOICE METADATA
-- ════════════════════════════════════════════════════════════════════════════

-- | Availability state for a choice option.
--
-- Sum type makes the state explicit - no more "Nothing means enabled".
data ChoiceAvailability
  = Available                    -- ^ Option can be selected
  | DisabledBecause Text         -- ^ Option disabled with reason
  deriving (Show, Eq, Generic)

-- | Metadata for rich choice options.
--
-- Used with 'requestChoiceMeta' to provide descriptions, costs, and disabled states.
data ChoiceMeta = ChoiceMeta
  { cmDescription :: Maybe Text       -- ^ Descriptive text below label
  , cmCosts :: [Text]                 -- ^ Cost tags e.g. ["2 Stress", "1 Heat"]
  , cmAvailability :: ChoiceAvailability -- ^ Whether option is selectable
  }
  deriving (Show, Eq, Generic)

-- | Default metadata (no description, no costs, enabled).
defaultChoiceMeta :: ChoiceMeta
defaultChoiceMeta = ChoiceMeta Nothing [] Available


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | UI effect for displaying content and requesting user input.
--
-- The interpreter translates these to wire types (UIState/UserAction).
data UI r where
  -- | Display text to the user (appended to chat log).
  ShowText :: Text -> UI ()

  -- | Request text input from user with a prompt.
  RequestTextInput :: Text -> UI Text

  -- | Request photo input from user with a prompt.
  RequestPhotoInput :: Text -> UI ByteString

  -- | Present choices to user, return selected value.
  --
  -- Uses 'NonEmpty' to ensure at least one option exists.
  RequestChoice :: Text -> NonEmpty (Text, a) -> UI a

  -- | Present multiple choices (checkboxes), return all selected values.
  --
  -- Uses 'NonEmpty' to ensure at least one option exists.
  -- Returns empty list if user selects nothing.
  RequestMultiChoice :: Text -> NonEmpty (Text, a) -> UI [a]

  -- | Present choices with descriptions, return selected value.
  --
  -- Each option has (label, description, value).
  RequestChoiceDesc :: Text -> NonEmpty (Text, Text, a) -> UI a

  -- | Present choices with full metadata, return selected value.
  --
  -- Each option has (label, metadata, value).
  -- Selecting a disabled option returns an error.
  RequestChoiceMeta :: Text -> NonEmpty (Text, ChoiceMeta, a) -> UI a

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
--
-- @
-- die <- requestChoice "Spend a die:" $ NE.fromList
--   [ ("⚃ 4", 4)
--   , ("⚂ 3", 3)
--   ]
-- -- die :: Int
-- @
requestChoice :: Member UI effs => Text -> NonEmpty (Text, a) -> Eff effs a
requestChoice prompt choices = send (RequestChoice prompt choices)

-- | Present multiple choices (checkboxes) and get all selected.
--
-- @
-- activities <- requestMultiChoice "How do you spend downtime?" $ NE.fromList
--   [ ("Recover", RecoverActivity)
--   , ("Train", TrainActivity)
--   , ("Work on project", ProjectActivity)
--   ]
-- -- activities :: [DowntimeActivity]
-- @
requestMultiChoice :: Member UI effs
                   => Text                -- ^ Prompt
                   -> NonEmpty (Text, a)  -- ^ (label, value)
                   -> Eff effs [a]
requestMultiChoice prompt choices = send (RequestMultiChoice prompt choices)

-- | Present choices with descriptions and get user selection.
--
-- @
-- choice <- requestChoiceDesc "Choose position:" $ NE.fromList
--   [ ("Controlled", "You have clear advantage", Controlled)
--   , ("Risky", "Things could go either way", Risky)
--   , ("Desperate", "You're outmatched", Desperate)
--   ]
-- @
requestChoiceDesc :: Member UI effs
                  => Text                        -- ^ Prompt
                  -> NonEmpty (Text, Text, a)    -- ^ (label, description, value)
                  -> Eff effs a
requestChoiceDesc prompt choices = send (RequestChoiceDesc prompt choices)

-- | Present choices with full metadata and get user selection.
--
-- @
-- choice <- requestChoiceMeta "Choose action:" $ NE.fromList
--   [ ("Push yourself", ChoiceMeta (Just "Take +1d") ["2 Stress"] Available, PushAction)
--   , ("Devil's bargain", ChoiceMeta (Just "Complication later") [] Available, BargainAction)
--   , ("Give up", ChoiceMeta Nothing [] (DisabledBecause "Not available"), GiveUpAction)
--   ]
-- @
requestChoiceMeta :: Member UI effs
                  => Text                          -- ^ Prompt
                  -> NonEmpty (Text, ChoiceMeta, a) -- ^ (label, meta, value)
                  -> Eff effs a
requestChoiceMeta prompt choices = send (RequestChoiceMeta prompt choices)

-- | Set thinking indicator.
setThinking :: Member UI effs => Bool -> Eff effs ()
setThinking = send . SetThinking


-- ════════════════════════════════════════════════════════════════════════════
-- CONVENIENCE HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Confirmation dialog (Yes/No).
--
-- @
-- confirmed <- confirm "Delete this item?"
-- when confirmed $ deleteItem item
-- @
confirm :: Member UI effs => Text -> Eff effs Bool
confirm prompt = requestChoice prompt $ NE.fromList
  [("Yes", True), ("No", False)]

-- | Die selection helper (formats dice with Unicode faces).
--
-- @
-- die <- requestDie "Spend a die from your pool:" $ NE.fromList [4, 4, 3, 2]
-- -- die :: Int (the selected die value)
-- @
requestDie :: Member UI effs => Text -> NonEmpty Int -> Eff effs Int
requestDie prompt dice = requestChoice prompt $
  NE.map (\d -> (dieFace d, d)) dice
  where
    dieFace :: Int -> Text
    dieFace n
      | n >= 1 && n <= 6 = T.singleton (faces !! (n - 1)) <> " " <> T.pack (show n)
      | otherwise = T.pack (show n)
    faces :: [Char]
    faces = "⚀⚁⚂⚃⚄⚅"

-- | Simple multi-choice with "Select all that apply" style.
--
-- Alias for 'requestMultiChoice'.
selectMultiple :: Member UI effs => Text -> NonEmpty (Text, a) -> Eff effs [a]
selectMultiple = requestMultiChoice
