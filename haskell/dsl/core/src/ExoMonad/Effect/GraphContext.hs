{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Graph and node context effects for reading ambient context values.
--
-- This module provides two bundled effects for accessing context in graph handlers:
--
-- * 'GraphContext entry' - Read the entry value that spawned this graph instance
-- * 'NodeInput input' - Read the current node's input value
--
-- Both follow the same pattern: read-only context injection, similar to Reader.
-- They're bundled because:
--
--   1. Same pattern (read-only context)
--   2. Complementary uses (graph-level vs node-level)
--   3. Simplifies effect stack reasoning
--
-- = Usage
--
-- @
-- -- In a handler, access the graph entry:
-- scaffoldAfter :: (ScaffoldExit, SessionId) -> Eff es (GotoChoice targets)
-- scaffoldAfter (exit, sid) = do
--   entry <- getEntry \@ScaffoldInput
--   when (entry.siCurrentDepth < entry.siMaxDepth) $ do
--     ...  -- spawn children
--
-- -- In an after-handler, access the node's input directly:
-- implAfter :: (ImplExit, SessionId) -> Eff es (GotoChoice targets)
-- implAfter (exit, sid) = do
--   originalInput <- getNodeInput \@ImplInput
--   case exit of
--     ImplTestsPassed _ -> pure $ gotoChoice \@"v3TDDReviewImpl" ...
--     ImplRequestRetry _ | originalInput.iiAttemptCount < 5 ->
--       pure $ gotoSelf originalInput { iiAttemptCount = originalInput.iiAttemptCount + 1 }
-- @
--
-- = Why NodeInput?
--
-- Without NodeInput, after-handlers would need to:
--   1. Store input in Memory during before-handler
--   2. Retrieve from Memory in after-handler
--
-- NodeInput eliminates this round-trip by injecting the input at the call site.
-- The interpreter wraps each handler invocation with @runNodeInput nodeInput@.
module ExoMonad.Effect.GraphContext
  ( -- * Graph Context Effect
    GraphContext (..),
    getEntry,

    -- * Node Input Effect
    NodeInput (..),
    getNodeInput,

    -- * Interpreters
    runGraphContext,
    runNodeInput,
  )
where

import Control.Monad.Freer (Eff, Member, interpret, send)

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH CONTEXT EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect for reading the entry value that spawned this graph instance.
--
-- Type parameters:
--   * @entry@ - the entry type for this graph (e.g., ScaffoldInput)
--
-- This enables any handler in the graph to access the original entry,
-- which is useful for:
--
--   * Depth tracking (entry carries current depth)
--   * Parent context access (entry may have ParentContext)
--   * Session inheritance (entry may have parent SessionId)
--
-- The interpreter is provided by the graph runner when spawning the graph.
-- For recursive graphs (via Subgraph effect), the interpreter ensures each
-- child graph sees its own entry value.
data GraphContext entry r where
  -- | Get the entry value that spawned this graph.
  GetEntry :: GraphContext entry entry

-- | Read the entry value that spawned this graph.
--
-- Usage requires type application to specify the entry type:
--
-- @
-- entry <- getEntry \@ScaffoldInput
-- let depth = entry.siCurrentDepth
-- @
--
-- This value is constant for the lifetime of the graph instance.
-- In recursive execution, each child sees its own entry.
getEntry ::
  forall entry effs.
  (Member (GraphContext entry) effs) =>
  Eff effs entry
getEntry = send (GetEntry :: GraphContext entry entry)

-- ════════════════════════════════════════════════════════════════════════════
-- NODE INPUT EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect for reading the current node's input value.
--
-- Type parameters:
--   * @input@ - the input type for this node (e.g., ImplInput)
--
-- This enables after-handlers to access the node's input without
-- storing it in Memory. The input is injected by the dispatcher
-- when calling the handler.
--
-- Key differences from GraphContext:
--   * GraphContext: same value for entire graph lifetime
--   * NodeInput: changes per node invocation (each node has different input type)
--
-- The type parameter varies per call site - when dispatching to "impl",
-- the interpreter uses @runNodeInput \@ImplInput implInput@.
data NodeInput input r where
  -- | Get the input value for this node invocation.
  GetNodeInput :: NodeInput input input

-- | Read the current node's input value.
--
-- Usage requires type application to specify the input type:
--
-- @
-- -- In implAfter
-- originalInput <- getNodeInput \@ImplInput
-- case originalInput.iiAttemptCount < 5 of
--   True -> pure $ gotoSelf originalInput { iiAttemptCount = originalInput.iiAttemptCount + 1 }
--   False -> pure $ gotoExit FailureResult
-- @
--
-- This replaces the pattern of storing input in Memory:
--
-- @
-- -- OLD: Memory round-trip
-- implBefore input = do
--   updateMem \@ImplMem $ \\m -> m { imOriginalInput = Just input }
--   ...
-- implAfter (exit, sid) = do
--   mem <- getMem \@ImplMem
--   let originalInput = fromMaybe (error \"impossible\") mem.imOriginalInput
--   ...
--
-- -- NEW: Direct via NodeInput
-- implAfter (exit, sid) = do
--   originalInput <- getNodeInput \@ImplInput
--   ...
-- @
getNodeInput ::
  forall input effs.
  (Member (NodeInput input) effs) =>
  Eff effs input
getNodeInput = send (GetNodeInput :: NodeInput input input)

-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Run GraphContext effect with a fixed entry value.
--
-- Typically provided by the graph runner when spawning a graph.
-- For recursive execution, each child graph gets its own entry.
--
-- @
-- -- In graph runner
-- runGraphContext scaffoldInput $ do
--   ... -- all handlers can now call getEntry
-- @
runGraphContext ::
  entry ->
  Eff (GraphContext entry ': effs) a ->
  Eff effs a
runGraphContext entry = interpret $ \case
  GetEntry -> pure entry

-- | Run NodeInput effect with a fixed input value.
--
-- Typically provided by the dispatcher when calling a handler.
-- Each node invocation wraps the handler in @runNodeInput@.
--
-- @
-- -- In dispatcher
-- runNodeInput implInput $ do
--   (exit, sid) <- runLLMNode ...
--   implAfter (exit, sid)  -- can now call getNodeInput
-- @
runNodeInput ::
  input ->
  Eff (NodeInput input ': effs) a ->
  Eff effs a
runNodeInput input = interpret $ \case
  GetNodeInput -> pure input
