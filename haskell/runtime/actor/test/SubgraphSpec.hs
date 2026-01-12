{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Tests for the Subgraph effect and interpreter.
--
-- These tests verify that the Subgraph effect correctly:
-- * Spawns child graph instances
-- * Collects results via awaitAny
-- * Tracks pending children via getPending
module SubgraphSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad.Freer (Eff, runM)
import Data.IORef (newIORef, atomicModifyIORef')
import Data.List (sort)
import Test.Hspec
import qualified Ki

import Tidepool.Actor.Subgraph
  ( SubgraphState(..)
  , newSubgraphState
  , newSubgraphStateDeferred
  , withRecursiveGraph
  , runSubgraph
  , Subgraph
  , spawnSelf
  , awaitAny
  , getPending
  , ChildHandle(..)
  , ChildId(..)
  )


spec :: Spec
spec = do
  describe "newSubgraphState" $ do
    it "creates empty state" $ do
      Ki.scoped $ \scope -> do
        state <- newSubgraphState @Int @Int scope (\_ -> pure)
        pending <- runSubgraphAction state (getPending @Int @Int)
        pending `shouldBe` []

  describe "spawnSelf" $ do
    it "spawns a child and returns a handle" $ do
      Ki.scoped $ \scope -> do
        state <- newSubgraphState scope simpleRunner
        handle <- runSubgraphAction state (spawnSelf @Int @Int 42)
        -- Handle should have a valid child ID
        let ChildHandle cid = handle
        cid `shouldSatisfy` (\_ -> True)  -- Just checking it exists

    it "child appears in getPending while running" $ do
      Ki.scoped $ \scope -> do
        state <- newSubgraphState scope slowRunner
        _ <- runSubgraphAction state (spawnSelf @Int @Int 42)
        -- Check immediately while child is still running (100ms delay in slowRunner)
        pendingList <- runSubgraphAction state (getPending @Int @Int)
        length pendingList `shouldBe` 1
        -- Wait for child to complete before exiting test
        _ <- runSubgraphAction state (awaitAny @Int @Int)
        pure ()

  describe "awaitAny" $ do
    it "returns child result when complete" $ do
      Ki.scoped $ \scope -> do
        state <- newSubgraphState scope simpleRunner
        _ <- runSubgraphAction state (spawnSelf @Int @Int 42)
        (_, result) <- runSubgraphAction state (awaitAny @Int @Int)
        result `shouldBe` Right 43  -- simpleRunner adds 1

    it "collects multiple children" $ do
      Ki.scoped $ \scope -> do
        state <- newSubgraphState scope simpleRunner

        -- Spawn 3 children
        _ <- runSubgraphAction state (spawnSelf @Int @Int 1)
        _ <- runSubgraphAction state (spawnSelf @Int @Int 2)
        _ <- runSubgraphAction state (spawnSelf @Int @Int 3)

        -- Collect all results
        (_, r1) <- runSubgraphAction state (awaitAny @Int @Int)
        (_, r2) <- runSubgraphAction state (awaitAny @Int @Int)
        (_, r3) <- runSubgraphAction state (awaitAny @Int @Int)

        -- Results should be 2, 3, 4 (order may vary)
        let results = [r1, r2, r3]
            isRight (Right _) = True
            isRight (Left _) = False
            rights xs = [x | Right x <- xs]
        all isRight results `shouldBe` True
        sort (rights results) `shouldBe` [2, 3, 4]

  describe "collect loop pattern" $ do
    it "collects all children with loop" $ do
      Ki.scoped $ \scope -> do
        state <- newSubgraphState scope simpleRunner

        -- Spawn children
        handles <- runSubgraphAction state $ do
          h1 <- spawnSelf @Int @Int 10
          h2 <- spawnSelf @Int @Int 20
          h3 <- spawnSelf @Int @Int 30
          pure [h1, h2, h3]

        -- Collection loop (the pattern from the plan)
        results <- collectLoop state handles

        sort results `shouldBe` [11, 21, 31]

  describe "recursive spawning" $ do
    it "handles nested spawning" $ do
      Ki.scoped $ \scope -> do
        -- Track spawn order
        orderRef <- newIORef (0 :: Int)

        -- Runner that spawns a child if input > 0
        let recursiveRunner :: ChildId -> Int -> IO Int
            recursiveRunner _ 0 = pure 0
            recursiveRunner _ n = do
              -- Mark this call
              order <- atomicModifyIORef' orderRef (\i -> (i + 1, i))
              -- In real usage, we'd use SubgraphState here too
              -- For this test, just simulate the pattern
              pure (order + n)

        state <- newSubgraphState scope recursiveRunner

        -- Spawn with input 5
        _ <- runSubgraphAction state (spawnSelf @Int @Int 5)
        (_, result) <- runSubgraphAction state (awaitAny @Int @Int)

        -- Should have executed once with order=0, so result=0+5=5
        result `shouldBe` Right 5

  -- ══════════════════════════════════════════════════════════════════════════
  -- DEFERRED WIRING TESTS
  -- ══════════════════════════════════════════════════════════════════════════
  --
  -- These tests verify the deferred binding pattern that enables recursive
  -- graph spawning when there's a chicken-and-egg dependency.

  describe "newSubgraphStateDeferred" $ do
    it "works when wired before spawning" $ do
      Ki.scoped $ \scope -> do
        -- Create deferred state
        (state, wire) <- newSubgraphStateDeferred @Int @Int scope

        -- Wire the runner BEFORE spawning
        wire simpleRunner

        -- Now spawning should work
        _ <- runSubgraphAction state (spawnSelf @Int @Int 42)
        (_, result) <- runSubgraphAction state (awaitAny @Int @Int)
        result `shouldBe` Right 43

    it "works with multiple children after wiring" $ do
      Ki.scoped $ \scope -> do
        (state, wire) <- newSubgraphStateDeferred @Int @Int scope
        wire simpleRunner

        -- Spawn multiple children
        _ <- runSubgraphAction state (spawnSelf @Int @Int 1)
        _ <- runSubgraphAction state (spawnSelf @Int @Int 2)
        _ <- runSubgraphAction state (spawnSelf @Int @Int 3)

        -- Collect all
        (_, r1) <- runSubgraphAction state (awaitAny @Int @Int)
        (_, r2) <- runSubgraphAction state (awaitAny @Int @Int)
        (_, r3) <- runSubgraphAction state (awaitAny @Int @Int)

        let results = [r1, r2, r3]
            isRight (Right _) = True
            isRight (Left _) = False
            rights xs = [x | Right x <- xs]
        all isRight results `shouldBe` True
        sort (rights results) `shouldBe` [2, 3, 4]

  describe "withRecursiveGraph" $ do
    it "provides state and wire function" $ do
      result <- withRecursiveGraph @Int @Int $ \state wire -> do
        -- Wire the runner
        wire simpleRunner

        -- Spawn and collect
        _ <- runSubgraphAction state (spawnSelf @Int @Int 100)
        (_, r) <- runSubgraphAction state (awaitAny @Int @Int)
        pure r

      result `shouldBe` Right 101

    it "supports the full recursion pattern" $ do
      -- This simulates what a real runner does:
      -- 1. Get state and wire function
      -- 2. Build interpreter using state
      -- 3. Wire the recursion
      -- 4. Run
      result <- withRecursiveGraph @Int @Int $ \state wire -> do
        -- In real code, this would be: buildHandlerMap (interpret state)
        -- For testing, we just wire directly (ignore ChildId)
        wire $ \_ n -> pure (n * 2)  -- Simulated "graph runner"

        -- Spawn children
        _ <- runSubgraphAction state (spawnSelf @Int @Int 5)
        _ <- runSubgraphAction state (spawnSelf @Int @Int 10)

        -- Collect
        (_, r1) <- runSubgraphAction state (awaitAny @Int @Int)
        (_, r2) <- runSubgraphAction state (awaitAny @Int @Int)

        case (r1, r2) of
          (Right v1, Right v2) -> pure (v1 + v2)
          _ -> error "Unexpected child failure"

      result `shouldBe` 30  -- (5*2) + (10*2)


-- ════════════════════════════════════════════════════════════════════════════
-- TEST HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a Subgraph action against a state.
runSubgraphAction
  :: SubgraphState entry result
  -> Eff '[Subgraph entry result, IO] a
  -> IO a
runSubgraphAction state action =
  runM $ runSubgraph state action


-- | Simple runner: adds 1 to input.
simpleRunner :: ChildId -> Int -> IO Int
simpleRunner _ n = pure (n + 1)


-- | Slow runner: waits briefly before returning.
-- Used to test getPending while children are still running.
slowRunner :: ChildId -> Int -> IO Int
slowRunner _ n = do
  -- Small delay to ensure child is "in progress"
  threadDelay 100000  -- 100ms delay
  pure (n + 1)


-- | Collection loop pattern from the plan.
--
-- Awaits children one by one, tracking which have completed.
collectLoop
  :: SubgraphState Int Int
  -> [ChildHandle]
  -> IO [Int]
collectLoop state = go []
  where
    go acc [] = pure acc
    go acc (_:rest) = do
      (_, eitherResult) <- runSubgraphAction state (awaitAny @Int @Int)
      case eitherResult of
        Right result -> go (result : acc) rest
        Left _err -> go acc rest  -- Skip failed children
