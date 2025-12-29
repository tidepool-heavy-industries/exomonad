{-# LANGUAGE OverloadedStrings #-}

-- | Example simulation of the tidying agent
--
-- This shows the flow without actual LLM calls.

module Tidying.Example where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

import Tidying
import Tidying.State (ActiveState(..))
import Tidying.Types (ItemName(..), AnxietyTrigger(..), SpaceFunction(..))

-- | Simulate a session (no actual LLM)
exampleSession :: IO ()
exampleSession = do
  putStrLn "=== Tidying Agent Example Session ===\n"

  let st0 = newSession

  -- Turn 1: User sends photo, says "idk where to start"
  putStrLn "USER: [photo of cluttered office] \"idk where to start\""
  let (action1, phase1) = decide st0 OverwhelmedNeedMomentum
  putStrLn $ "ORIENT: OverwhelmedNeedMomentum"
  putStrLn $ "DECIDE: " <> show action1 <> " -> " <> show phase1
  putStrLn $ "ACT: (would generate first instruction from photo)"
  putStrLn ""

  -- Turn 2: User answers function question (if we asked)
  -- st0 is already in Surveying with no function
  let (action2, phase2) = decide st0 NeedFunction
  putStrLn "--- Alternative path: asking function ---"
  putStrLn $ "DECIDE: " <> show action2 <> " -> " <> show phase2
  putStrLn "BOT: \"What do you need to DO in this space?\""
  putStrLn ""

  -- Turn 3: User provides function (simulate with Sorting state)
  let st2 = st0 { phaseData = SortingData (ActiveState (SpaceFunction "work from home, designer") [] Nothing) Nothing }
  let (action3, phase3) = decide st2 OverwhelmedNeedMomentum
  putStrLn "USER: \"work from home, I'm a designer\""
  putStrLn $ "DECIDE: " <> show action3 <> " -> " <> show phase3
  putStrLn "BOT: (first instruction based on photo)"
  putStrLn ""

  -- Turn 4: User completes action
  let st3 = st2  -- already in Sorting
  let (action4, phase4) = decide st3 ActionDone
  putStrLn "USER: \"done\" [photo of cleared chair]"
  putStrLn $ "ORIENT: ActionDone"
  putStrLn $ "DECIDE: " <> show action4 <> " -> " <> show phase4
  putStrLn "BOT: \"Next thing.\""
  putStrLn ""

  -- Turn 5: User describes an item
  let (action5, phase5) = decide st3 (ItemDescribed (ItemName "empty amazon envelope") Trash)
  putStrLn "USER: \"empty amazon envelope\""
  putStrLn $ "ORIENT: ItemDescribed \"empty amazon envelope\" Trash"
  putStrLn $ "DECIDE: " <> show action5 <> " -> " <> show phase5
  putStrLn "BOT: \"Trash. Toss it by the door. Next.\""
  putStrLn ""

  -- Turn 6: User hesitates on item
  let (action6, phase6) = decide st3 (ItemDescribed (ItemName "sketchbook") NeedsDecisionSupport)
  putStrLn "USER: \"sketchbook with some ideas, idk...\""
  putStrLn $ "ORIENT: ItemDescribed \"sketchbook\" NeedsDecisionSupport"
  putStrLn $ "DECIDE: " <> show action6 <> " -> " <> show phase6
  putStrLn "BOT: \"This space is for design work. Does the sketchbook help with that?\""
  putStrLn ""

  -- Turn 7: User expresses anxiety
  let (action7, phase7) = decide st3 (Anxious (AnxietyTrigger "boxes"))
  putStrLn "USER: \"the boxes freak me out\""
  putStrLn $ "ORIENT: Anxious \"boxes\""
  putStrLn $ "DECIDE: " <> show action7 <> " -> " <> show phase7
  putStrLn "BOT: \"Boxes stay closed for now. Let's do something smaller.\""
  putStrLn ""

  -- Turn 8: Unsure pile growing
  let st4 = st3 { piles = Piles [] [] [ItemName "cable1", ItemName "cable2", ItemName "paper1", ItemName "thing1", ItemName "thing2", ItemName "thing3"] }
  let (action8, phase8) = decide st4 UnsureGrowing
  putStrLn "--- Unsure pile has 6 items ---"
  putStrLn $ "ORIENT: UnsureGrowing"
  putStrLn $ "DECIDE: " <> show action8 <> " -> " <> show phase8
  putStrLn "BOT: \"Pause. Split your unsure pile: cables in one spot, other in another.\""
  putStrLn ""

  -- Turn 9: User wants to stop
  let (action9, phase9) = decide st3 WantsToStop
  putStrLn "USER: \"I need to stop\""
  putStrLn $ "ORIENT: WantsToStop"
  putStrLn $ "DECIDE: " <> show action9 <> " -> " <> show phase9
  putStrLn "BOT: (summary of session)"
  putStrLn ""

  putStrLn "=== End Example ==="

-- | Show the state machine transitions
showTransitions :: IO ()
showTransitions = do
  putStrLn "=== State Machine Transitions ===\n"

  putStrLn "SURVEYING:"
  putStrLn "  NeedFunction -> AskFunction, stay Surveying"
  putStrLn "  NeedAnchors -> AskAnchors, stay Surveying"
  putStrLn "  OverwhelmedNeedMomentum -> FirstInstruction, go Sorting"
  putStrLn ""

  putStrLn "SORTING:"
  putStrLn "  ItemDescribed _ Trash -> InstructTrash, stay Sorting"
  putStrLn "  ItemDescribed _ (Belongs loc) -> InstructPlace loc, stay Sorting"
  putStrLn "  ItemDescribed _ Unsure -> InstructUnsure, stay Sorting"
  putStrLn "  ItemDescribed _ NeedsDecisionSupport -> DecisionAid, go DecisionSupport"
  putStrLn "  ActionDone -> InstructNext, stay Sorting"
  putStrLn "  UnsureGrowing -> InstructSplit, go Splitting"
  putStrLn "  MainPileDone -> (pick next target)"
  putStrLn ""

  putStrLn "SPLITTING:"
  putStrLn "  ActionDone -> InstructNext, go Sorting"
  putStrLn ""

  putStrLn "DECISION_SUPPORT:"
  putStrLn "  ItemDescribed _ cls -> (classify action), go Sorting"
  putStrLn ""

  putStrLn "ANY PHASE:"
  putStrLn "  Anxious trigger -> PivotAway, stay current"
  putStrLn "  Flagging -> EnergyCheck, stay current"
  putStrLn "  WantsToStop -> Summary, stay current"
  putStrLn "  Stuck item -> DecisionAid, go DecisionSupport"
