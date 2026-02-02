{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | UI effect for user interface interactions.
--
-- Effect type only - interpreters live in exomonad-ui-interpreter.
-- This effect bridges agent execution to UI state via wire types.
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
module ExoMonad.Effects.UI
  ( -- * Effect
    UI (..),
    showText,
    requestTextInput,
    requestPhotoInput,
    requestChoice,
    requestMultiChoice,
    requestChoiceDesc,
    requestChoiceMeta,
    setThinking,

    -- * Rich Choice Metadata
    ChoiceAvailability (..),
    ChoiceMeta (..),

    -- * Convenience Helpers
    confirm,
    requestDie,
    selectMultiple,
  )
where

import Polysemy (Sem, Member, makeSem)
import Data.Kind (Type)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T

-- ════════════════════════════════════════════════════════════════════════════
-- CHOICE METADATA
-- ════════════════════════════════════════════════════════════════════════════

-- | Availability state for a choice option.
--
-- Sum type makes the state explicit - no more "Nothing means enabled".
data ChoiceAvailability
  = -- | Option can be selected
    Available
  | -- | Option disabled with reason
    DisabledBecause Text
  deriving (Show, Eq, Generic)

-- | Metadata for rich choice options.
--
-- Used with 'requestChoiceMeta' to provide descriptions, costs, and disabled states.
data ChoiceMeta = ChoiceMeta
  { -- | Descriptive text below label
    cmDescription :: Maybe Text,
    -- | Cost tags e.g. ["2 Stress", "1 Heat"]
    cmCosts :: [Text],
    -- | Whether option is selectable
    cmAvailability :: ChoiceAvailability
  }
  deriving (Show, Eq, Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | UI effect for displaying content and requesting user input.
--
-- The interpreter translates these to wire types (UIState/UserAction).
data UI m a where
  -- | Display text to the user (appended to chat log).
  ShowText :: Text -> UI m ()
  -- | Request text input from user with a prompt.
  RequestTextInput :: Text -> UI m Text
  -- | Request photo input from user with a prompt.
  RequestPhotoInput :: Text -> UI m ByteString
  -- | Present choices to user, return selected value.
  --
  -- Uses 'NonEmpty' to ensure at least one option exists.
  RequestChoice :: Text -> NonEmpty (Text, a) -> UI m a
  -- | Present multiple choices (checkboxes), return all selected values.
  --
  -- Uses 'NonEmpty' to ensure at least one option exists.
  -- Returns empty list if user selects nothing.
  RequestMultiChoice :: Text -> NonEmpty (Text, a) -> UI m [a]
  -- | Present choices with descriptions, return selected value.
  --
  -- Each option has (label, description, value).
  RequestChoiceDesc :: Text -> NonEmpty (Text, Text, a) -> UI m a
  -- | Present choices with full metadata, return selected value.
  --
  -- Each option has (label, metadata, value).
  -- Selecting a disabled option returns an error.
  RequestChoiceMeta :: Text -> NonEmpty (Text, ChoiceMeta, a) -> UI m a
  -- | Set the "thinking" indicator state.
  SetThinking :: Bool -> UI m ()

makeSem ''UI

-- ════════════════════════════════════════════════════════════════════════════
-- CONVENIENCE HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Confirmation dialog (Yes/No).
--
-- @
-- confirmed <- confirm "Delete this item?"
-- when confirmed $ deleteItem item
-- @
confirm :: (Member UI r) => Text -> Sem r Bool
confirm prompt =
  requestChoice prompt $
    NE.fromList
      [("Yes", True), ("No", False)]

-- | Die selection helper (formats dice with Unicode faces).
--
-- @
-- die <- requestDie "Spend a die from your pool:" $ NE.fromList [4, 4, 3, 2]
-- -- die :: Int (the selected die value)
-- @
requestDie :: (Member UI r) => Text -> NonEmpty Int -> Sem r Int
requestDie prompt dice =
  requestChoice prompt $
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
selectMultiple :: (Member UI r) => Text -> NonEmpty (Text, a) -> Sem r [a]
selectMultiple = requestMultiChoice

