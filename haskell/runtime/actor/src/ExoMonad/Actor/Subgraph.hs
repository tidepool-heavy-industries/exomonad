{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- | Subgraph effect interpreter for spawning recursive graph instances.
--
-- This interpreter enables graphs to spawn child instances of themselves,
-- creating a tree of concurrent executions. Key aspects:
--
-- * Children run via ki (structured concurrency) - automatic cleanup on scope exit
-- * Completion notifications via TQueue (for awaitAny)
-- * Closure-based recursion: ssRunGraph holds how to run a new instance
-- * No messaging between nodes - rebase cascade via git (deferred)
-- * When parent exits scope (success or failure), all children are cancelled
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
--
-- = Scope-Based Cancellation (Phase 4)
--
-- Children run within a ki scope. When the parent computation exits the scope
-- (either normally or via exception), all children are automatically cancelled.
-- This enables abort-on-failure semantics without explicit cancellation API.
module ExoMonad.Actor.Subgraph
  ( -- * State
    SubgraphState (..),
    newSubgraphState,
    newSubgraphStateDeferred,

    -- * High-Level Runner

    -- | These functions handle the chicken-and-egg dependency automatically.
    -- Prefer these over manual state creation when possible.
    withRecursiveGraph,

    -- * Interpreter
    runSubgraph,

    -- * Re-exports
    Subgraph (..),
    ChildHandle (..),
    ChildId (..),
    ChildError (..),
    spawnSelf,
    awaitAny,
    getPending,
  )
where

import Control.Concurrent.STM
  ( TQueue,
    TVar,
    atomically,
    modifyTVar',
    newTQueueIO,
    newTVarIO,
    readTQueue,
    readTVarIO,
    writeTQueue,
  )
import Control.Concurrent.STM.TSem
  ( TSem,
    newTSem,
    signalTSem,
    waitTSem,
  )
import Control.Exception (SomeException, displayException, finally, try)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Typeable (typeOf)
import Data.UUID.V4 (nextRandom)
import ExoMonad.Effect.Subgraph
  ( ChildError (..),
    ChildHandle (..),
    ChildId (..),
    Subgraph (..),
    awaitAny,
    getPending,
    spawnSelf,
  )
import Ki qualified

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
-- = Scope-Based Cancellation
--
-- Children run within the @ssScope@. When the parent exits the scope
-- (via normal completion or exception), ki automatically cancels all
-- running children. This enables abort-on-failure without explicit API.
--
-- Type parameters:
--   * @entry@ - input type for the graph (e.g., Spec)
--   * @result@ - output type from the graph (e.g., V2Result)
data SubgraphState entry result = SubgraphState
  { -- | Ki scope for structured concurrency. Children forked here are
    -- automatically cancelled when the scope exits.
    ssScope :: !Ki.Scope,
    -- | Currently running children (Ki threads). Removed when they complete.
    -- The thread result is Either to capture exceptions.
    ssChildren :: !(TVar (Map ChildId (Ki.Thread (Either ChildError result)))),
    -- | Completion queue. AwaitAny blocks on this.
    -- Results are Either to capture child failures.
    ssCompleted :: !(TQueue (ChildId, Either ChildError result)),
    -- | How to get the graph runner. The IO layer enables deferred binding
    -- for cases where the runner can't be known at state creation time.
    -- The runner receives the ChildId for linking child nodes to parent.
    ssGetRunner :: !(IO (ChildId -> entry -> IO result)),
    -- | Optional concurrency limiter. When present, limits number of
    -- concurrent children per parent. Nothing = unlimited.
    ssConcurrencySem :: !(Maybe TSem)
  }

-- | Create a new SubgraphState with an immediate runner.
--
-- Use this for simple cases (tests) where the runner is known upfront.
-- The runner is wrapped in @pure@ for immediate access.
--
-- @
-- Ki.scoped $ \scope -> do
--   state <- newSubgraphState scope simpleRunner
--   ...
-- @
newSubgraphState ::
  -- | Ki scope for structured concurrency
  Ki.Scope ->
  -- | Optional concurrency limit (Nothing = unlimited)
  Maybe Int ->
  -- | The graph runner (receives ChildId for linking)
  (ChildId -> entry -> IO result) ->
  IO (SubgraphState entry result)
newSubgraphState scope maybeConcurrencyLimit runner = do
  sem <- case maybeConcurrencyLimit of
    Just limit -> Just <$> atomically (newTSem (fromIntegral limit))
    Nothing -> pure Nothing
  SubgraphState scope
    <$> newTVarIO Map.empty
    <*> newTQueueIO
    <*> pure (pure runner) -- Wrap in pure for immediate access
    <*> pure sem

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
-- Ki.scoped $ \scope -> do
--   (subgraphState, wireRecursion) <- newSubgraphStateDeferred scope
--   let interpret = runSubgraph subgraphState ...
--   handlers <- buildHandlerMap interpret
--   wireRecursion $ \\entry -> runGraphAsActors handlers (toJSON entry)
--   runGraphAsActors handlers (toJSON rootEntry)
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
newSubgraphStateDeferred ::
  forall entry result.
  -- | Ki scope for structured concurrency
  Ki.Scope ->
  -- | Optional concurrency limit (Nothing = unlimited)
  Maybe Int ->
  IO (SubgraphState entry result, (ChildId -> entry -> IO result) -> IO ())
newSubgraphStateDeferred scope maybeConcurrencyLimit = do
  -- IORef holds the runner. Starts with error placeholder.
  -- INVARIANT: Must be written exactly once before any child spawns.
  ref <- newIORef (\_ _ -> error "Subgraph recursion not wired - call wireRecursion before running graph")
  sem <- case maybeConcurrencyLimit of
    Just limit -> Just <$> atomically (newTSem (fromIntegral limit))
    Nothing -> pure Nothing
  state <-
    SubgraphState scope
      <$> newTVarIO Map.empty
      <*> newTQueueIO
      <*> pure (readIORef ref) -- Deferred: reads IORef when called
      <*> pure sem
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
--   -- Runner receives ChildId for linking child nodes to parent
--   wire $ \\childId childSpec -> runGraphAsActors handlers (toJSON childSpec)
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
--
-- == Scope-Based Cancellation
--
-- The function creates a ki scope that wraps the entire computation.
-- All children spawned via @spawnSelf@ run within this scope.
-- When the callback returns (success or exception), the scope exits
-- and ki automatically cancels any still-running children.
withRecursiveGraph ::
  forall entry result a.
  -- | Optional concurrency limit (Nothing = unlimited)
  Maybe Int ->
  -- | Callback receives (state, wireRecursion). Wire before running!
  -- The runner receives ChildId for linking child execution trees.
  (SubgraphState entry result -> ((ChildId -> entry -> IO result) -> IO ()) -> IO a) ->
  IO a
withRecursiveGraph maybeConcurrencyLimit callback =
  -- Create ki scope for structured concurrency
  Ki.scoped $ \scope -> do
    (state, wire) <- newSubgraphStateDeferred scope maybeConcurrencyLimit
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
runSubgraph ::
  forall entry result effs a.
  (LastMember IO effs) =>
  SubgraphState entry result ->
  Eff (Subgraph entry result ': effs) a ->
  Eff effs a
runSubgraph state = interpret $ \case
  SpawnSelf childEntry -> sendM $ do
    -- Generate unique child ID
    cid <- ChildId <$> nextRandom

    -- Acquire semaphore before spawning (blocks if at limit)
    case state.ssConcurrencySem of
      Just sem -> atomically $ waitTSem sem
      Nothing -> pure ()

    -- Spawn child graph within the ki scope
    -- When the scope exits (parent completes or throws), all children
    -- are automatically cancelled by ki's structured concurrency.
    childThread <- Ki.fork state.ssScope $ do
      -- Ensure semaphore released on all exit paths (success, failure, cancellation)
      finally
        ( do
            -- Get the runner (may be deferred via IORef)
            -- This is where the deferred binding resolves!
            runner <- state.ssGetRunner

            -- Run the full graph for this child, catching exceptions
            -- This prevents one child's failure from killing siblings via Ki cancellation
            -- Pass the ChildId so the runner can link child nodes to parent
            outcome <- try @SomeException $ runner cid childEntry

            let result = case outcome of
                  Right r -> Right r
                  Left ex -> Left $ exceptionToChildError ex

            -- On completion (success OR failure):
            -- 1. Remove from children map
            -- 2. Add to completion queue (unblocks awaitAny)
            atomically $ do
              modifyTVar' state.ssChildren (Map.delete cid)
              writeTQueue state.ssCompleted (cid, result)

            pure result
        )
        -- Release semaphore in finally block (guaranteed to run)
        ( case state.ssConcurrencySem of
            Just sem -> atomically $ signalTSem sem
            Nothing -> pure ()
        )

    -- Track the running child
    atomically $ modifyTVar' state.ssChildren (Map.insert cid childThread)

    -- Return handle to caller
    pure (ChildHandle cid)
  AwaitAny ->
    sendM $
      -- Block until any child completes (success or failure)
      atomically $
        readTQueue state.ssCompleted
  GetPending -> sendM $ do
    children <- readTVarIO state.ssChildren
    pure [ChildHandle cid | cid <- Map.keys children]

-- | Convert a SomeException to ChildError.
--
-- Captures exception type name and message for parent to inspect.
exceptionToChildError :: SomeException -> ChildError
exceptionToChildError ex =
  ChildError
    { ceMessage = T.pack (displayException ex),
      ceException = T.pack (show (typeOf ex)),
      ceDetails = Nothing -- Could extract more details for specific exception types
    }
