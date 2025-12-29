{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Tidying.Decide
  ( -- * Core routing
    decide
  , decideFromExtract

    -- * Helpers
  , suggestSplit
  , pickNextTarget

    -- * Photo analysis helpers
  , ChaosLevel(..)
  , chaosLevel
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T

import Tidying.State
import Tidying.Situation
import Tidying.Action
import Tidying.Context (PhotoAnalysis(..))
import Tidying.Output (Extract(..), Intent(..), Choice(..))
import Tidying.Types (ItemName(..), Location(..), AnxietyTrigger(..), CategoryName(..))

-- | Pure routing: (State, Situation) -> (Action, NextPhase)
-- This is the core state machine logic. NO LLM calls here.
decide :: SessionState -> Situation -> (Action, Phase)
decide st sit = case (st.phase, sit) of

  ------------------------------------
  -- SURVEYING
  ------------------------------------

  (Surveying, NeedFunction) ->
    (AskFunction, Surveying)

  (Surveying, NeedAnchors) ->
    (AskAnchors, Surveying)

  (Surveying, OverwhelmedNeedMomentum) ->
    -- Skip questions, get them moving
    (FirstInstruction, Sorting)

  -- User answered function question
  (Surveying, ActionDone)
    | hasFunction st && not (hasAnchors st) ->
        -- Could ask anchors, but check if they seem overwhelmed
        (AskAnchors, Surveying)
    | hasFunction st ->
        (FirstInstruction, Sorting)
    | otherwise ->
        (AskFunction, Surveying)

  ------------------------------------
  -- SORTING
  ------------------------------------

  (Sorting, ItemDescribed _item Trash) ->
    (InstructTrash, Sorting)

  (Sorting, ItemDescribed _item (Belongs location)) ->
    (InstructPlace location, Sorting)

  (Sorting, ItemDescribed item Unsure) ->
    let st' = st { piles = addUnsure item st.piles }
    in if unsureCount st'.piles > unsureThreshold
       then (InstructSplit (suggestSplit st'), Splitting)
       else (InstructUnsure, Sorting)

  (Sorting, ItemDescribed item NeedsDecisionSupport) ->
    (DecisionAid item, DecisionSupport)

  (Sorting, ActionDone) ->
    (InstructNext, Sorting)

  (Sorting, UnsureGrowing) ->
    (InstructSplit (suggestSplit st), Splitting)

  (Sorting, MainPileDone) ->
    pickNextTarget st

  ------------------------------------
  -- SPLITTING
  ------------------------------------

  (Splitting, ActionDone) ->
    -- After split, back to sorting
    (InstructNext, Sorting)

  ------------------------------------
  -- DECISION_SUPPORT
  ------------------------------------

  (DecisionSupport, ItemDescribed item cls) ->
    -- User made a decision, route based on classification
    case cls of
      Trash -> (InstructTrash, Sorting)
      Belongs loc -> (InstructPlace loc, Sorting)
      Unsure -> (InstructUnsure, Sorting)
      NeedsDecisionSupport -> (DecisionAid item, DecisionSupport)

  (DecisionSupport, ActionDone) ->
    (InstructNext, Sorting)

  ------------------------------------
  -- REFINING
  ------------------------------------

  (Refining, ItemDescribed item cls) ->
    case cls of
      Trash -> (InstructTrash, Refining)
      Belongs loc -> (InstructPlace loc, Refining)
      -- In refining, no more unsure - must decide
      Unsure -> (DecisionAid item, DecisionSupport)
      NeedsDecisionSupport -> (DecisionAid item, DecisionSupport)

  (Refining, ActionDone) ->
    (InstructNext, Refining)

  (Refining, MainPileDone) ->
    pickNextTarget st

  ------------------------------------
  -- ENERGY / ANXIETY (any phase)
  ------------------------------------

  (_, Anxious trigger) ->
    -- Pivot away from anxiety trigger
    let alternative = findAlternative st ((\(AnxietyTrigger t) -> t) trigger)
    in (PivotAway trigger (Location alternative), st.phase)

  (_, Flagging) ->
    (EnergyCheck, st.phase)

  (_, WantsToStop) ->
    (Summary, st.phase)

  (_, WantsToContinue) ->
    -- Continue with whatever is next
    (InstructNext, st.phase)

  (_, Stuck maybeItem) ->
    let item = fromMaybe (ItemName "this item") maybeItem
    in (DecisionAid item, DecisionSupport)

  ------------------------------------
  -- COMPLETION
  ------------------------------------

  (_, AllDone) ->
    (Summary, st.phase)

  ------------------------------------
  -- DEFAULT (intentional catch-all)
  ------------------------------------
  --
  -- This default handles any (Phase, Situation) pairs not explicitly matched.
  -- It's safe because:
  -- 1. Universal handlers above catch Anxious, Flagging, WantsToStop, etc.
  -- 2. Phase-specific handlers catch the main flows
  -- 3. InstructNext is a safe fallback that keeps momentum
  --
  -- If you add a new Phase or Situation, test that it routes correctly.
  -- Consider adding explicit cases if the default behavior is wrong.

  _ ->
    (InstructNext, st.phase)


-- ══════════════════════════════════════════════════════════════
-- EXTRACTION-BASED ROUTING (new approach)
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
    (Summary, st.phase)

  IntentHelp ->
    -- Stuck - provide decision aid if they mentioned an item
    case ext.exItem of
      Just item -> (DecisionAid item, DecisionSupport)
      Nothing   -> (FirstInstruction, Sorting)

  -- Phase-specific routing
  _ -> case st.phase of

    -- SURVEYING: Focus on getting function/anchors
    -- Photo analysis can fast-track to sorting if space is chaotic
    Surveying
      -- Buried chaos: skip questions, get them moving immediately
      | chaosLevel mPhoto == Buried ->
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
      | chaosLevel mPhoto == Clear, IntentStart <- ext.exIntent ->
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

-- | Add item to unsure pile
addUnsure :: ItemName -> Piles -> Piles
addUnsure item p = p { unsure = item : p.unsure }

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

-- | Pick next target after pile is done
pickNextTarget :: SessionState -> (Action, Phase)
pickNextTarget st
  -- More unsure items?
  | not (null st.piles.unsure) =
      (AckProgress "Pile done. Unsure items left.", Sorting)

  -- Emergent categories to refine?
  | Just (CategoryName nextCat, _) <- Map.lookupMin (getEmergentCats st) =
      (AckProgress ("Let's do " <> nextCat), Refining)

  -- All done
  | otherwise =
      (Summary, st.phase)

-- | Find an alternative when pivoting away from anxiety trigger
findAlternative :: SessionState -> Text -> Text
findAlternative st trigger =
  -- Simple heuristic: if trigger is "boxes", suggest something else
  case T.toLower trigger of
    t | "box" `T.isInfixOf` t ->
        if not (null st.piles.unsure)
          then "the unsure pile"
          else "the desk"
    t | "paper" `T.isInfixOf` t ->
        "something on the floor"
    _ ->
        "something smaller"

-- ══════════════════════════════════════════════════════════════
-- PHOTO ANALYSIS HELPERS
-- ══════════════════════════════════════════════════════════════

-- | Photo chaos level (extracted from photo analysis)
data ChaosLevel
  = Buried     -- ^ "buried" - overwhelmingly messy, fast-track to action
  | Cluttered  -- ^ "cluttered" - clearly messy
  | Moderate   -- ^ "moderate" - some clutter
  | Clear      -- ^ "clear" - space is mostly tidy
  | Unknown    -- ^ No photo analysis available
  deriving (Eq, Show)

-- | Extract chaos level from photo analysis
chaosLevel :: Maybe PhotoAnalysis -> ChaosLevel
chaosLevel Nothing = Unknown
chaosLevel (Just pa) = case T.toLower pa.paChaosLevel of
  "buried"    -> Buried
  "cluttered" -> Cluttered
  "moderate"  -> Moderate
  "clear"     -> Clear
  _           -> Unknown

-- | Does photo analysis indicate a blocked function?
hasBlockedFunction :: Maybe PhotoAnalysis -> Bool
hasBlockedFunction (Just pa) = isJust pa.paBlockedFunction
hasBlockedFunction Nothing = False

-- | Does photo analysis suggest a first target?
hasFirstTarget :: Maybe PhotoAnalysis -> Bool
hasFirstTarget (Just pa) = isJust pa.paFirstTarget
hasFirstTarget Nothing = False
