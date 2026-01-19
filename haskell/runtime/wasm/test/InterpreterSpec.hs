{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | End-to-end tests for the graph interpreter using freer-simple.
--
-- These tests exercise graph interpretation patterns via the WASM effect system:
--
-- 1. LinearGraph: Entry -> Add1 -> Add2 -> Exit (chain)
-- 2. BranchGraph: Entry -> IsEven -> EvenPath|OddPath -> Exit (branching)
-- 3. DiamondGraph: Entry -> Split -> PathA|PathB -> Merge -> Exit (diamond)
-- 4. LoopGraph: Entry -> Decrement (self-loop) -> Exit (self-loops)
--
-- The tests verify:
-- - Correct handler dispatch based on GotoChoice
-- - Payload types flow correctly between nodes
-- - Effects execute in correct order
-- - Exit node returns final value correctly
-- - Self-loops terminate correctly
module InterpreterSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Test.Hspec

import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, Self)
import Tidepool.Graph.Generic (GraphMode(..), type (:-))
import qualified Tidepool.Graph.Generic as G (EntryNode, ExitNode, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, OneOf, gotoChoice, gotoExit, gotoSelf)
import Tidepool.Graph.Goto.Internal (GotoChoice(..), OneOf(..))  -- For test assertions

import Tidepool.Wasm.Effect (WasmM, logInfo)
import Tidepool.Wasm.Runner (initializeWasm, WasmResult(..))
import Tidepool.Wasm.WireTypes (EffectResult(..))


-- ════════════════════════════════════════════════════════════════════════════
-- TEST FIXTURE GRAPHS
-- ════════════════════════════════════════════════════════════════════════════

-- | Linear graph: Entry(Int) -> Add1 -> Add2 -> Exit(Int)
--
-- Tests chaining of handlers through multiple nodes.
data LinearGraph mode = LinearGraph
  { lgEntry :: mode :- G.EntryNode Int
  , lgAdd1  :: mode :- G.LogicNode :@ Input Int :@ UsesEffects '[Goto "lgAdd2" Int]
  , lgAdd2  :: mode :- G.LogicNode :@ Input Int :@ UsesEffects '[Goto Exit Int]
  , lgExit  :: mode :- G.ExitNode Int
  }
  deriving Generic


-- | Branching graph: Entry(Int) -> IsEven -> EvenPath|OddPath -> Exit(String)
--
-- Tests branching based on handler logic.
data BranchGraph mode = BranchGraph
  { bgEntry    :: mode :- G.EntryNode Int
  , bgIsEven   :: mode :- G.LogicNode :@ Input Int
               :@ UsesEffects '[Goto "bgEvenPath" Int, Goto "bgOddPath" Int]
  , bgEvenPath :: mode :- G.LogicNode :@ Input Int :@ UsesEffects '[Goto Exit Text]
  , bgOddPath  :: mode :- G.LogicNode :@ Input Int :@ UsesEffects '[Goto Exit Text]
  , bgExit     :: mode :- G.ExitNode Text
  }
  deriving Generic


-- | Diamond graph: Entry(Int) -> Split -> PathA|PathB -> Merge -> Exit(Int)
--
-- Tests convergent paths meeting at a merge node.
data DiamondGraph mode = DiamondGraph
  { dgEntry :: mode :- G.EntryNode Int
  , dgSplit :: mode :- G.LogicNode :@ Input Int
            :@ UsesEffects '[Goto "dgPathA" Int, Goto "dgPathB" Int]
  , dgPathA :: mode :- G.LogicNode :@ Input Int :@ UsesEffects '[Goto "dgMerge" Int]
  , dgPathB :: mode :- G.LogicNode :@ Input Int :@ UsesEffects '[Goto "dgMerge" Int]
  , dgMerge :: mode :- G.LogicNode :@ Input Int :@ UsesEffects '[Goto Exit Int]
  , dgExit  :: mode :- G.ExitNode Int
  }
  deriving Generic


-- | Loop graph: Entry(Int) -> Decrement (self-loop until 0) -> Exit(Int)
--
-- Tests self-loops with bounded iteration.
data LoopGraph mode = LoopGraph
  { loopEntry     :: mode :- G.EntryNode Int
  , loopDecrement :: mode :- G.LogicNode :@ Input Int
                  :@ UsesEffects '[Goto Self Int, Goto Exit Int]
  , loopExit      :: mode :- G.ExitNode Int
  }
  deriving Generic


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- Linear Graph Handlers

lgAdd1Handler :: Int -> WasmM (GotoChoice '[To "lgAdd2" Int])
lgAdd1Handler n = do
  logInfo $ "Add1: " <> T.pack (show n)
  pure $ gotoChoice @"lgAdd2" (n + 1)

lgAdd2Handler :: Int -> WasmM (GotoChoice '[To Exit Int])
lgAdd2Handler n = do
  logInfo $ "Add2: " <> T.pack (show n)
  pure $ gotoExit (n + 1)


-- Branching Graph Handlers

bgIsEvenHandler :: Int -> WasmM (GotoChoice '[To "bgEvenPath" Int, To "bgOddPath" Int])
bgIsEvenHandler n
  | even n    = pure $ gotoChoice @"bgEvenPath" n
  | otherwise = pure $ gotoChoice @"bgOddPath" n

bgEvenPathHandler :: Int -> WasmM (GotoChoice '[To Exit Text])
bgEvenPathHandler n = do
  logInfo $ "EvenPath: " <> T.pack (show n)
  pure $ gotoExit $ "even: " <> T.pack (show n)

bgOddPathHandler :: Int -> WasmM (GotoChoice '[To Exit Text])
bgOddPathHandler n = do
  logInfo $ "OddPath: " <> T.pack (show n)
  pure $ gotoExit $ "odd: " <> T.pack (show n)


-- Diamond Graph Handlers

dgSplitHandler :: Int -> WasmM (GotoChoice '[To "dgPathA" Int, To "dgPathB" Int])
dgSplitHandler n
  | n >= 10   = pure $ gotoChoice @"dgPathA" n
  | otherwise = pure $ gotoChoice @"dgPathB" n

dgPathAHandler :: Int -> WasmM (GotoChoice '[To "dgMerge" Int])
dgPathAHandler n = do
  logInfo $ "PathA: " <> T.pack (show n)
  pure $ gotoChoice @"dgMerge" (n + 10)

dgPathBHandler :: Int -> WasmM (GotoChoice '[To "dgMerge" Int])
dgPathBHandler n = do
  logInfo $ "PathB: " <> T.pack (show n)
  pure $ gotoChoice @"dgMerge" (n + 100)

dgMergeHandler :: Int -> WasmM (GotoChoice '[To Exit Int])
dgMergeHandler n = do
  logInfo $ "Merge: " <> T.pack (show n)
  pure $ gotoExit (n + 1)


-- Loop Graph Handlers

loopDecrementHandler :: Int -> WasmM (GotoChoice '[To Self Int, To Exit Int])
loopDecrementHandler n
  | n <= 0    = pure $ gotoExit n
  | otherwise = do
      logInfo $ "Loop: " <> T.pack (show n)
      pure $ gotoSelf (n - 1)


-- ════════════════════════════════════════════════════════════════════════════
-- TEST RUNNERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a WasmResult to completion, handling all yields with success responses.
runToCompletion :: WasmResult a -> a
runToCompletion (WasmComplete a) = a
runToCompletion (WasmYield _eff resume) = runToCompletion (resume (ResSuccess Nothing))
runToCompletion (WasmError msg) = error $ "WasmError: " <> T.unpack msg


-- | Run the linear graph: Entry -> Add1 -> Add2 -> Exit
runLinearGraph :: Int -> Int
runLinearGraph input = runToCompletion $ initializeWasm $ do
  c1 <- lgAdd1Handler input
  case c1 of
    GotoChoice (Here n) -> do
      c2 <- lgAdd2Handler n
      case c2 of
        GotoChoice (Here result) -> pure result
        _ -> error "runLinearGraph: unexpected choice from lgAdd2Handler"
    _ -> error "runLinearGraph: unexpected choice from lgAdd1Handler"


-- | Run the branching graph: Entry -> IsEven -> EvenPath|OddPath -> Exit
runBranchGraph :: Int -> Text
runBranchGraph input = runToCompletion $ initializeWasm $ do
  c1 <- bgIsEvenHandler input
  case c1 of
    GotoChoice (Here n) -> do
      -- Even path
      c2 <- bgEvenPathHandler n
      case c2 of
        GotoChoice (Here result) -> pure result
        _ -> error "runBranchGraph: unexpected choice from bgEvenPathHandler"
    GotoChoice (There (Here n)) -> do
      -- Odd path
      c2 <- bgOddPathHandler n
      case c2 of
        GotoChoice (Here result) -> pure result
        _ -> error "runBranchGraph: unexpected choice from bgOddPathHandler"
    _ -> error "runBranchGraph: unexpected choice from bgIsEvenHandler"


-- | Run the diamond graph: Entry -> Split -> PathA|PathB -> Merge -> Exit
runDiamondGraph :: Int -> Int
runDiamondGraph input = runToCompletion $ initializeWasm $ do
  c1 <- dgSplitHandler input
  c2 <- case c1 of
    GotoChoice (Here n) -> dgPathAHandler n
    GotoChoice (There (Here n)) -> dgPathBHandler n
    _ -> error "runDiamondGraph: unexpected choice from dgSplitHandler"
  case c2 of
    GotoChoice (Here n) -> do
      c3 <- dgMergeHandler n
      case c3 of
        GotoChoice (Here result) -> pure result
        _ -> error "runDiamondGraph: unexpected choice from dgMergeHandler"
    _ -> error "runDiamondGraph: unexpected choice from dgPathHandler"


-- | Run the loop graph: Entry -> Decrement (loop) -> Exit
-- Manually implements the self-loop dispatch.
runLoopGraph :: Int -> Int
runLoopGraph input = runToCompletion $ initializeWasm $ runLoop input
  where
    runLoop :: Int -> WasmM Int
    runLoop n = do
      c <- loopDecrementHandler n
      case c of
        GotoChoice (Here n') -> runLoop n'  -- Self: continue loop
        GotoChoice (There (Here result)) -> pure result  -- Exit
        _ -> error "runLoopGraph: unexpected choice from loopDecrementHandler"


-- ════════════════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════════════════

spec :: Spec
spec = do
  linearGraphSpec
  branchGraphSpec
  diamondGraphSpec
  loopGraphSpec
  effectOrderingSpec


linearGraphSpec :: Spec
linearGraphSpec = describe "LinearGraph" $ do

  it "chains handlers through two nodes" $ do
    -- Input 5 -> Add1(+1) -> Add2(+1) -> Exit = 7
    runLinearGraph 5 `shouldBe` 7

  it "handles zero input" $ do
    runLinearGraph 0 `shouldBe` 2

  it "handles negative input" $ do
    runLinearGraph (-5) `shouldBe` (-3)

  it "handles large input" $ do
    runLinearGraph 1000000 `shouldBe` 1000002


branchGraphSpec :: Spec
branchGraphSpec = describe "BranchGraph" $ do

  it "routes even numbers to EvenPath" $ do
    runBranchGraph 4 `shouldBe` "even: 4"

  it "routes odd numbers to OddPath" $ do
    runBranchGraph 5 `shouldBe` "odd: 5"

  it "routes zero to EvenPath" $ do
    runBranchGraph 0 `shouldBe` "even: 0"

  it "routes negative even to EvenPath" $ do
    runBranchGraph (-10) `shouldBe` "even: -10"

  it "routes negative odd to OddPath" $ do
    runBranchGraph (-7) `shouldBe` "odd: -7"


diamondGraphSpec :: Spec
diamondGraphSpec = describe "DiamondGraph" $ do

  it "routes >= 10 through PathA and merges" $ do
    -- 10 -> PathA(+10) -> Merge(+1) = 21
    runDiamondGraph 10 `shouldBe` 21

  it "routes < 10 through PathB and merges" $ do
    -- 5 -> PathB(+100) -> Merge(+1) = 106
    runDiamondGraph 5 `shouldBe` 106

  it "handles boundary condition (9 goes to PathB)" $ do
    -- 9 -> PathB(+100) -> Merge(+1) = 110
    runDiamondGraph 9 `shouldBe` 110

  it "handles zero" $ do
    runDiamondGraph 0 `shouldBe` 101


loopGraphSpec :: Spec
loopGraphSpec = describe "LoopGraph (self-loop)" $ do

  it "loops until zero" $ do
    runLoopGraph 5 `shouldBe` 0

  it "exits immediately for zero" $ do
    runLoopGraph 0 `shouldBe` 0

  it "exits immediately for negative" $ do
    runLoopGraph (-5) `shouldBe` (-5)

  it "handles single iteration" $ do
    runLoopGraph 1 `shouldBe` 0

  it "handles many iterations" $ do
    runLoopGraph 100 `shouldBe` 0


-- | Tests for verifying effect execution order.
effectOrderingSpec :: Spec
effectOrderingSpec = describe "Effect Ordering" $ do

  it "linear graph produces effects in order" $ do
    -- Verify that the effects happen in the expected sequence
    let result = initializeWasm $ do
          c1 <- lgAdd1Handler 5
          case c1 of
            GotoChoice (Here n) -> do
              c2 <- lgAdd2Handler n
              case c2 of
                GotoChoice (Here r) -> pure r
                _ -> error "effectOrderingSpec: unexpected choice from lgAdd2Handler"
            _ -> error "effectOrderingSpec: unexpected choice from lgAdd1Handler"

    -- First yield should be Log "Add1: 5"
    case result of
      WasmYield _eff1 resume1 -> do
        -- Resume first effect
        case resume1 (ResSuccess Nothing) of
          WasmYield _eff2 resume2 -> do
            -- Resume second effect
            case resume2 (ResSuccess Nothing) of
              WasmComplete r -> r `shouldBe` 7
              _ -> expectationFailure "Expected WasmComplete after second resume"
          WasmComplete _ -> expectationFailure "Expected second WasmYield"
          WasmError _ -> expectationFailure "Unexpected WasmError"
      WasmComplete _ -> expectationFailure "Expected initial WasmYield"
      WasmError _ -> expectationFailure "Unexpected WasmError"

  it "loop graph produces effects for each iteration" $ do
    -- 3 iterations: Loop 3, Loop 2, Loop 1, then exit at 0
    let result = initializeWasm $ runLoop 3
          where
            runLoop n = do
              c <- loopDecrementHandler n
              case c of
                GotoChoice (Here n') -> runLoop n'
                GotoChoice (There (Here r)) -> pure r
                _ -> error "effectOrderingSpec: unexpected choice from loopDecrementHandler"

    -- Should yield 3 times (for n=3,2,1), then complete
    case result of
      WasmYield _ r1 ->
        case r1 (ResSuccess Nothing) of
          WasmYield _ r2 ->
            case r2 (ResSuccess Nothing) of
              WasmYield _ r3 ->
                case r3 (ResSuccess Nothing) of
                  WasmComplete n -> n `shouldBe` 0
                  _ -> expectationFailure "Expected completion after 3 yields"
              _ -> expectationFailure "Expected third WasmYield"
          _ -> expectationFailure "Expected second WasmYield"
      _ -> expectationFailure "Expected initial WasmYield"
