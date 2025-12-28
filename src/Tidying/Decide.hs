{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Tidying.Decide
  ( -- * Core routing
    decide

    -- * Helpers
  , suggestSplit
  , pickNextTarget
  ) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Tidying.State
import Tidying.Situation
import Tidying.Action

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
    let alternative = findAlternative st trigger
    in (PivotAway trigger alternative, st.phase)

  (_, Flagging) ->
    (EnergyCheck, st.phase)

  (_, WantsToStop) ->
    (Summary, st.phase)

  (_, WantsToContinue) ->
    -- Continue with whatever is next
    (InstructNext, st.phase)

  (_, Stuck maybeItem) ->
    let item = maybe "this item" id maybeItem
    in (DecisionAid item, DecisionSupport)

  ------------------------------------
  -- COMPLETION
  ------------------------------------

  (_, AllDone) ->
    (Summary, st.phase)

  ------------------------------------
  -- DEFAULT
  ------------------------------------

  _ ->
    (InstructNext, st.phase)


-- | Threshold for when to split unsure pile
unsureThreshold :: Int
unsureThreshold = 5

-- | Add item to unsure pile
addUnsure :: Text -> Piles -> Piles
addUnsure item p = p { unsure = item : p.unsure }

-- | Suggest categories for splitting unsure pile
suggestSplit :: SessionState -> [Text]
suggestSplit st =
  let items = st.piles.unsure
      lowerItems = map T.toLower items
  in
    -- Check for common patterns
    if any (hasWord "cable") lowerItems || any (hasWord "cord") lowerItems
      then ["cables", "other"]
    else if any (hasWord "paper") lowerItems || any (hasWord "document") lowerItems
      then ["papers", "other"]
    else if any (hasWord "book") lowerItems
      then ["books", "other"]
    else if any (hasWord "clothes") lowerItems || any (hasWord "shirt") lowerItems
      then ["clothes", "other"]
    else
      -- Default: keep-maybe vs donate-maybe
      ["keep-maybe", "donate-maybe"]
  where
    hasWord w t = w `T.isInfixOf` t

-- | Pick next target after pile is done
pickNextTarget :: SessionState -> (Action, Phase)
pickNextTarget st
  -- More unsure items?
  | not (null st.piles.unsure) =
      (AckProgress "Pile done. Unsure items left.", Sorting)

  -- Emergent categories to refine?
  | not (Map.null st.emergentCats) =
      let cats = Map.keys st.emergentCats
          nextCat = head cats
      in (AckProgress ("Let's do " <> nextCat), Refining)

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
