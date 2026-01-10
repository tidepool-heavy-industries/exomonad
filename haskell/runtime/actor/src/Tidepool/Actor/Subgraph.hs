{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- | Subgraph effect interpreter for spawning recursive graph instances.
--
-- This interpreter enables graphs to spawn child instances of themselves,
-- creating a tree of concurrent executions. Key aspects:
--
-- * Children run in the same process (async, not separate processes)
-- * Completion notifications via TQueue (for awaitAny)
-- * Closure-based recursion: ssRunGraph holds how to run a new instance
-- * No messaging between nodes - rebase cascade via git (deferred)
--
-- = Usage Pattern
--
-- @
-- -- Top-level runner creates SubgraphState with self-reference
-- runV2Graph :: Spec -> IO V2Result
-- runV2Graph spec = do
--   state <- newSubgraphState runV2Graph  -- pass self!
--   runGraphWithSubgraph state spec
--
-- -- Handler uses the effect
-- hDecideHandler result = do
--   let childSpecs = deriveChildSpecs result
--   handles <- traverse spawnSelf childSpecs
--   collectChildren handles
--
-- collectChildren handles = go handles []
--   where
--     go [] acc = finalize acc
--     go pending acc = do
--       (cid, result) <- awaitAny
--       go (removeByCid cid pending) (result : acc)
-- @
module Tidepool.Actor.Subgraph
  ( -- * State
    SubgraphState(..)
  , newSubgraphState
  , newSubgraphStateDeferred

    -- * High-Level Runner
    -- | These functions handle the chicken-and-egg dependency automatically.
    -- Prefer these over manual state creation when possible.
  , withRecursiveGraph

    -- * Interpreter
  , runSubgraph

    -- * Handler Builders
  , effHandlerWithSubgraph

    -- * Re-exports
  , Subgraph(..)
  , ChildHandle(..)
  , ChildId(..)
  , spawnSelf
  , awaitAny
  , getPending
  ) where

import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM
  ( TVar, TQueue
  , newTVarIO, newTQueueIO
  , readTVarIO, atomically
  , modifyTVar', writeTQueue, readTQueue
  )
import Control.Monad.Freer (Eff, LastMember, interpret, sendM, runM)
import Data.Aeson (FromJSON)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.UUID.V4 (nextRandom)

import Tidepool.Effect.Subgraph
  ( Subgraph(..)
  , ChildHandle(..)
  , ChildId(..)
  , spawnSelf
  , awaitAny
  , getPending
  )

import Tidepool.Actor.Graph
  ( HandlerBuilder
  , ExtractChoice
  , wrapEffHandler
  , GotoChoice
  )


-- ════════════════════════════════════════════════════════════════════════════
-- SUBGRAPH STATE
-- ════════════════════════════════════════════════════════════════════════════

-- | State for managing spawned child graph instances.
--
-- Each parent graph instance maintains its own SubgraphState. The key
-- insight is that @ssGetRunner@ provides a closure that knows how to run
-- a new graph instance - enabling recursive tree construction.
--
-- The indirection through @IO@ allows deferred binding: create the state
-- first, then wire up the runner after handlers are built.
--
-- Type parameters:
--   * @entry@ - input type for the graph (e.g., Spec)
--   * @result@ - output type from the graph (e.g., V2Result)
data SubgraphState entry result = SubgraphState
  { ssChildren  :: !(TVar (Map ChildId (Async result)))
    -- ^ Currently running children. Removed when they complete.
  , ssCompleted :: !(TQueue (ChildId, result))
    -- ^ Completion queue. AwaitAny blocks on this.
  , ssGetRunner :: !(IO (entry -> IO result))
    -- ^ How to get the graph runner. The IO layer enables deferred binding
    -- for cases where the runner can't be known at state creation time.
  }


-- | Create a new SubgraphState with an immediate runner.
--
-- Use this for simple cases (tests) where the runner is known upfront.
-- The runner is wrapped in @pure@ for immediate access.
--
-- @
-- state <- newSubgraphState simpleRunner
-- @
newSubgraphState
  :: (entry -> IO result)  -- ^ The graph runner (known immediately)
  -> IO (SubgraphState entry result)
newSubgraphState runner = SubgraphState
  <$> newTVarIO Map.empty
  <*> newTQueueIO
  <*> pure (pure runner)  -- Wrap in pure for immediate access


-- | Create a new SubgraphState with deferred runner wiring.
--
-- __Low-level API__ - prefer 'withRecursiveGraph' for most use cases.
--
-- Use this when you have a chicken-and-egg dependency:
--
-- * SubgraphState needs the graph runner
-- * Graph runner needs handlers
-- * Handlers need the interpreter
-- * Interpreter needs SubgraphState
--
-- Returns @(state, wireRecursion)@. Call @wireRecursion@ AFTER building
-- handlers but BEFORE running the graph.
--
-- @
-- (subgraphState, wireRecursion) <- newSubgraphStateDeferred
-- let interpret = runSubgraph subgraphState ...
-- handlers <- buildHandlerMap interpret
-- wireRecursion $ \\entry -> runGraphAsActors handlers (toJSON entry)
-- runGraphAsActors handlers (toJSON rootEntry)
-- @
--
-- == Implementation Note
--
-- Internally uses an 'IORef' to break the circular dependency. The IORef
-- starts with a placeholder that errors if called before wiring. The
-- @wireRecursion@ callback writes the real runner to the IORef.
--
-- This is safe because:
--
-- 1. Wiring happens once, before any children spawn
-- 2. No concurrent access during the wiring phase
-- 3. After wiring, the IORef is only read (no writes)
newSubgraphStateDeferred
  :: forall entry result.
     IO (SubgraphState entry result, (entry -> IO result) -> IO ())
newSubgraphStateDeferred = do
  -- IORef holds the runner. Starts with error placeholder.
  -- INVARIANT: Must be written exactly once before any child spawns.
  ref <- newIORef (\_ -> error "Subgraph recursion not wired - call wireRecursion before running graph")
  state <- SubgraphState
    <$> newTVarIO Map.empty
    <*> newTQueueIO
    <*> pure (readIORef ref)  -- Deferred: reads IORef when called
  -- The wire function writes the real runner to the IORef
  pure (state, writeIORef ref)


-- | Run a computation with a recursive subgraph, handling all wiring automatically.
--
-- __This is the recommended API for DSL users.__
--
-- The callback receives:
--
-- 1. @SubgraphState@ - pass this to your effect interpreter
-- 2. A @wire@ function - call this with your graph runner BEFORE running
--
-- @
-- result <- withRecursiveGraph \@Spec \@V2Result $ \\subgraphState wire -> do
--   let interpret = runV2Effects subgraphState ...
--   handlers <- buildHandlerMap interpret
--
--   -- Wire the recursion (MUST happen before running!)
--   wire $ \\childSpec -> runGraphAsActors handlers (toJSON childSpec)
--
--   -- Now run - children will recursively spawn correctly
--   runGraphAsActors handlers (toJSON rootSpec)
-- @
--
-- == Why This Pattern?
--
-- Graph execution has a chicken-and-egg problem:
--
-- 1. The interpreter needs SubgraphState
-- 2. SubgraphState needs the graph runner
-- 3. The graph runner needs handlers
-- 4. Handlers need the interpreter (step 1!)
--
-- This function breaks the cycle using deferred binding. The internal IORef
-- is completely hidden - you just call @wire@ with your runner.
withRecursiveGraph
  :: forall entry result a.
     (SubgraphState entry result -> ((entry -> IO result) -> IO ()) -> IO a)
     -- ^ Callback receives (state, wireRecursion). Wire before running!
  -> IO a
withRecursiveGraph callback = do
  (state, wire) <- newSubgraphStateDeferred
  callback state wire


-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Interpret the Subgraph effect using SubgraphState.
--
-- This is the core interpreter that translates effect operations to IO:
--
-- * SpawnSelf: spawn async child, track in ssChildren, notify on complete
-- * AwaitAny: block on ssCompleted queue
-- * GetPending: read ssChildren TVar
--
-- Note: Children remove themselves from ssChildren and add to ssCompleted
-- when they finish. This enables AwaitAny to work correctly.
--
-- == Deferred Runner Lookup
--
-- The interpreter calls @ssGetRunner@ to obtain the runner function.
-- This indirection enables deferred binding:
--
-- * With @newSubgraphState@: @ssGetRunner = pure runner@ (immediate)
-- * With @newSubgraphStateDeferred@: @ssGetRunner = readIORef ref@ (deferred)
--
-- The deferred case allows the runner to be wired up after the interpreter
-- is built, breaking the chicken-and-egg dependency cycle.
runSubgraph
  :: forall entry result effs a.
     LastMember IO effs
  => SubgraphState entry result
  -> Eff (Subgraph entry result ': effs) a
  -> Eff effs a
runSubgraph state = interpret $ \case

  SpawnSelf childEntry -> sendM $ do
    -- Generate unique child ID
    cid <- ChildId <$> nextRandom

    -- Spawn child graph asynchronously
    childAsync <- async $ do
      -- Get the runner (may be deferred via IORef)
      -- This is where the deferred binding resolves!
      runner <- state.ssGetRunner

      -- Run the full graph for this child
      result <- runner childEntry

      -- On completion:
      -- 1. Remove from children map
      -- 2. Add to completion queue (unblocks awaitAny)
      atomically $ do
        modifyTVar' state.ssChildren (Map.delete cid)
        writeTQueue state.ssCompleted (cid, result)

      pure result

    -- Track the running child
    atomically $ modifyTVar' state.ssChildren (Map.insert cid childAsync)

    -- Return handle to caller
    pure (ChildHandle cid)

  AwaitAny -> sendM $
    -- Block until any child completes
    atomically $ readTQueue state.ssCompleted

  GetPending -> sendM $ do
    children <- readTVarIO state.ssChildren
    pure [ChildHandle cid | cid <- Map.keys children]


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER BUILDERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Build a handler with Subgraph effect.
--
-- This follows the same pattern as effHandlerWithMem - the SubgraphState
-- is created once when the handler is built, then shared across all
-- invocations of that handler.
--
-- @
-- -- Handler that can spawn children
-- decideHandler :: ScaffoldingResult
--               -> Eff '[Subgraph Spec V2Result, IO] (GotoChoice '[To Exit V2Result])
-- decideHandler result = do
--   handles <- traverse spawnSelf childSpecs
--   results <- collectLoop handles
--   pure $ gotoExit results
--
-- -- Build the handler with subgraph support
-- buildDecideHandler :: (Spec -> IO V2Result) -> HandlerBuilder
-- buildDecideHandler runGraph = effHandlerWithSubgraph runGraph decideHandler
-- @
effHandlerWithSubgraph
  :: forall entry result payload targets.
     ( FromJSON payload
     , ExtractChoice targets
     )
  => (entry -> IO result)  -- ^ Graph runner for children
  -> (payload -> Eff '[Subgraph entry result, IO] (GotoChoice targets))
  -> HandlerBuilder
effHandlerWithSubgraph runGraph handler = do
  state <- newSubgraphState runGraph
  pure $ wrapEffHandler (runSubgraphToIO state) handler


-- | Run Subgraph effect stack to IO.
runSubgraphToIO
  :: SubgraphState entry result
  -> Eff '[Subgraph entry result, IO] a
  -> IO a
runSubgraphToIO state = runM . runSubgraph state
