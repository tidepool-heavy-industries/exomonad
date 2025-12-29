{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Tidying.Decide
  ( -- * Core routing
    decideFromExtract

    -- * Helpers
  , suggestSplit

    -- * Photo analysis helpers
  , chaosLevel
  , hasBlockedFunction
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T

import Tidying.State
import Tidying.Action
import Tidying.Context (PhotoAnalysis(..))
import Tidying.Output (Extract(..), Intent(..), Choice(..))
import Tidying.Types (ItemName(..), Location(..), CategoryName(..), ChaosLevel(..))

-- ══════════════════════════════════════════════════════════════
-- EXTRACTION-BASED ROUTING
-- ══════════════════════════════════════════════════════════════

-- | Route based on extracted information from user input
-- LLM extracts facts, this function decides what to do
--
-- This is PHASE-AWARE: different phases have different behaviors.
-- Photo analysis is consulted for chaos level, blocked function, and first target.
decideFromExtract :: SessionState -> Maybe PhotoAnalysis -> Extract -> (Action, Phase)
decideFromExtract st mPhoto ext = case ext.exIntent of
  -- Universal intents (work in any phase)
  IntentStop ->
    (Summary, phase st)

  IntentHelp ->
    -- Stuck - provide decision aid if they mentioned an item
    case ext.exItem of
      Just item -> (DecisionAid item, DecisionSupport)
      Nothing   -> (FirstInstruction, Sorting)

  -- Phase-specific routing
  _ -> case phase st of

    -- SURVEYING: Focus on getting function/anchors
    -- Photo analysis can fast-track to sorting if space is chaotic
    Surveying
      -- Buried chaos: skip questions, get them moving immediately
      | chaosLevel mPhoto == Just Buried ->
          (FirstInstruction, Sorting)
      -- Blocked function + have function: start sorting to unblock
      | hasBlockedFunction mPhoto, hasFunction st ->
          (FirstInstruction, Sorting)
      -- If they told us the function, check if we need anchors
      | Just _ <- ext.exFunction ->
          if hasAnchors st || isJust ext.exAnchors
            then (FirstInstruction, Sorting)
            else (AskAnchors, Surveying)
      -- If they're describing space (start intent), ask function
      | IntentStart <- ext.exIntent ->
          if hasFunction st
            then (AskAnchors, Surveying)
            else (AskFunction, Surveying)
      -- If they want to continue but we don't have function yet
      | not (hasFunction st) ->
          (AskFunction, Surveying)
      | otherwise ->
          -- Have function, start sorting
          (FirstInstruction, Sorting)

    -- SORTING: Handle item decisions
    -- Photo analysis can acknowledge progress
    Sorting
      -- Clear space: acknowledge progress!
      | chaosLevel mPhoto == Just Clear, IntentStart <- ext.exIntent ->
          (AckProgress "Space is looking clear!", Sorting)
      | otherwise -> routeInSorting st ext

    -- SPLITTING: Usually back to sorting after action
    Splitting -> case ext.exIntent of
      IntentContinue -> (InstructNext, Sorting)
      IntentDecided  -> routeInSorting st ext
      _              -> (InstructNext, Sorting)

    -- DECISION_SUPPORT: Help with stuck items
    DecisionSupport -> case ext.exIntent of
      IntentDecided -> routeInSorting st ext
      _             -> case ext.exItem of
        Just item -> (DecisionAid item, DecisionSupport)
        Nothing   -> (InstructNext, Sorting)

    -- REFINING: Like sorting but for specific categories
    Refining -> case ext.exIntent of
      IntentDecided -> routeInSorting st ext
      IntentItem    -> (AskItemDecision (fromMaybe (ItemName "that") ext.exItem), Refining)
      _             -> (InstructNext, Refining)

-- | Route decisions within Sorting phase
routeInSorting :: SessionState -> Extract -> (Action, Phase)
routeInSorting st ext = case ext.exIntent of
  IntentDecided ->
    case ext.exChoice of
      Just ChoiceTrash  -> (InstructTrash, Sorting)
      Just ChoicePlace  -> (InstructPlace (fromMaybe (Location "its spot") ext.exPlace), Sorting)
      Just ChoiceKeep   -> (InstructPlace (Location "keep pile"), Sorting)
      Just ChoiceUnsure ->
        -- Check if unsure pile is getting too big
        if unsureCount st.piles >= unsureThreshold
          then (InstructSplit (suggestSplit st), Splitting)
          else (InstructUnsure, Sorting)
      Nothing           -> (InstructNext, Sorting)

  IntentItem ->
    -- They described an item but haven't said what to do with it
    (AskItemDecision (fromMaybe (ItemName "that") ext.exItem), Sorting)

  IntentContinue ->
    (InstructNext, Sorting)

  IntentStart ->
    -- Beginning or describing space - give first instruction
    (FirstInstruction, Sorting)

  _ -> (InstructNext, Sorting)


-- | Threshold for when to split unsure pile
unsureThreshold :: Int
unsureThreshold = 5

-- | Suggest categories for splitting unsure pile
suggestSplit :: SessionState -> NonEmpty CategoryName
suggestSplit st =
  let items = st.piles.unsure
      lowerItems = map (\(ItemName n) -> T.toLower n) items
  in
    -- Check for common patterns
    if any (hasWord "cable") lowerItems || any (hasWord "cord") lowerItems
      then CategoryName "cables" :| [CategoryName "other"]
    else if any (hasWord "paper") lowerItems || any (hasWord "document") lowerItems
      then CategoryName "papers" :| [CategoryName "other"]
    else if any (hasWord "book") lowerItems
      then CategoryName "books" :| [CategoryName "other"]
    else if any (hasWord "clothes") lowerItems || any (hasWord "shirt") lowerItems
      then CategoryName "clothes" :| [CategoryName "other"]
    else
      -- Default: keep-maybe vs donate-maybe
      CategoryName "keep-maybe" :| [CategoryName "donate-maybe"]
  where
    hasWord w t = w `T.isInfixOf` t

-- ══════════════════════════════════════════════════════════════
-- PHOTO ANALYSIS HELPERS
-- ══════════════════════════════════════════════════════════════

-- | Extract chaos level from photo analysis
-- Now just extracts the already-parsed field from PhotoAnalysis.
-- ChaosLevel is defined in Types.hs with proper FromJSON instance.
chaosLevel :: Maybe PhotoAnalysis -> Maybe ChaosLevel
chaosLevel = fmap (.paChaosLevel)

-- | Does photo analysis indicate a blocked function?
hasBlockedFunction :: Maybe PhotoAnalysis -> Bool
hasBlockedFunction (Just pa) = isJust pa.paBlockedFunction
hasBlockedFunction Nothing = False
