{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}

-- | The Memory effect for persistent state in graph nodes.
--
-- Memory provides typed, persistent state that survives across graph runs.
-- A single parameterized effect is used for both node-private state and
-- graph-level shared state - the difference is just the type parameter.
--
-- = Design Philosophy
--
-- "One effect, parameterized by type. Node vs global is a matter of scope."
--
-- Rather than separate @NodeMemory@ and @GlobalMemory@ effects, we use
-- @Memory s@ where @s@ is the state type. This is cleaner because:
--
-- * Same API (get/update) for both scopes
-- * Type system naturally distinguishes different memory types
-- * Multiple memory types can coexist in an effect stack
-- * Interpretation handles the persistence semantics
--
-- = Usage
--
-- For a graph with node-private and global memory:
--
-- @
-- type MyGraph = Graph
--   '[ Entry :~> Message
--    , "explore" := LLM :@ Needs '[Message] :@ Schema Findings :@ Memory ExploreMem
--    , Exit :<~ Response
--    ]
--   :& Global SessionState
-- @
--
-- A node handler can access both:
--
-- @
-- exploreHandler :: (Memory ExploreMem :> es, Memory SessionState :> es) => Eff es ()
-- exploreHandler = do
--   -- Access node's private state
--   myMem <- getMem \@ExploreMem
--   updateMem \@ExploreMem $ \\m -> m { urlsVisited = newUrl : m.urlsVisited }
--
--   -- Access global shared state
--   global <- getMem \@SessionState
--   updateMem \@SessionState $ \\g -> g { totalSearches = g.totalSearches + 1 }
--
--   -- With lenses (preferred for nested updates)
--   modifyMem \@ExploreMem #urlsVisited (newUrl :)
-- @
--
-- = Why update, not set?
--
-- The API uses @update :: (s -> s) -> m ()@ rather than @set :: s -> m ()@.
-- This prevents bugs where concurrent or interleaved updates clobber each
-- other. With @update@, each modification sees the current state.
--
-- = Interpretation
--
-- The effect is interpreted differently depending on scope:
--
-- * __Global memory__: Single state slot, persisted at graph boundaries
-- * __Node memory__: Per-node state slots, loaded/stored around node execution
--
-- See 'runMemory' for the basic interpreter and 'runMemoryPersistent' for
-- persistence-aware interpretation.
module Tidepool.Graph.Memory
  ( -- * The Memory Effect
    Memory(..)
  , getMem
  , updateMem

    -- * Lens Helpers
  , modifyMem

    -- * Effect Interpretation
  , runMemory
  , runMemoryPure
  , evalMemory
  ) where

import Data.Kind (Type)
import Effectful
import Effectful.Dispatch.Dynamic
import qualified Effectful.State.Static.Local as EState
import Optics (A_Setter, Is, Optic', over)

-- ════════════════════════════════════════════════════════════════════════════
-- THE MEMORY EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect for accessing persistent memory.
--
-- The type parameter @s@ determines which memory slot is accessed.
-- Different types access different slots:
--
-- * @Memory ExploreMem@ - a node's private persistent state
-- * @Memory SessionState@ - the graph's shared persistent state
--
-- Multiple @Memory@ effects with different types can coexist in the same
-- effect stack, each accessing its own independent state.
type Memory :: Type -> Effect
data Memory s :: Effect where
  -- | Get the current memory value.
  GetMem :: Memory s m s

  -- | Update memory by applying a function.
  --
  -- Uses a function rather than a setter to prevent update conflicts.
  -- Each update sees the current state, not a stale snapshot.
  UpdateMem :: (s -> s) -> Memory s m ()

type instance DispatchOf (Memory s) = 'Dynamic

-- | Get the current memory value.
--
-- @
-- myState <- getMem \@MyStateType
-- @
getMem :: forall s es. Memory s :> es => Eff es s
getMem = send GetMem

-- | Update memory by applying a function to the current state.
--
-- Uses a function rather than a setter to prevent update conflicts:
--
-- @
-- -- Good: sees current state
-- updateMem \@Counter (+ 1)
--
-- -- Risky pattern (if we had set):
-- -- x <- getMem
-- -- setMem (x + 1)  -- Could clobber concurrent updates!
-- @
updateMem :: forall s es. Memory s :> es => (s -> s) -> Eff es ()
updateMem f = send (UpdateMem f)

-- ════════════════════════════════════════════════════════════════════════════
-- LENS HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Modify a field within memory using an optic (lens/setter).
--
-- This is the preferred way to update nested fields:
--
-- @
-- -- Using OverloadedLabels + generic-optics
-- modifyMem \@ExploreMem #urlsVisited (newUrl :)
--
-- -- Or with explicit optics
-- modifyMem \@Config (field \@"timeout") (* 2)
-- @
modifyMem :: forall s a es k is. (Memory s :> es, Is k A_Setter) => Optic' k is s a -> (a -> a) -> Eff es ()
modifyMem l f = updateMem @s (over l f)

-- | Monadic version for updates that need effects.
--
-- When you need to compute the new value effectfully, use getMem/updateMem:
--
-- @
-- s <- getMem \@Cache
-- newEntries <- filterM (fmap not . isExpired) s.entries
-- updateMem \@Cache $ \\c -> c { entries = newEntries }
-- @
--
-- Note: This pattern isn't atomic, but that's fine for sequential execution.
-- For concurrent access, you'd need STM-based persistence anyway.

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT INTERPRETATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Run memory effect with an initial value, returning final state.
--
-- This is the basic interpreter that wraps effectful's State.
--
-- @
-- (result, finalState) <- runMemory initialState $ do
--   updateMem (+ 1)
--   getMem
-- @
runMemory :: forall s es a. s -> Eff (Memory s : es) a -> Eff es (a, s)
runMemory initial action = do
  reinterpret (EState.runState initial) handler action
  where
    handler :: forall es'. EState.State s :> es'
            => EffectHandler (Memory s) es'
    handler _ = \case
      GetMem -> EState.get
      UpdateMem f -> EState.modify f

-- | Run memory effect purely, returning only the result.
--
-- Useful when you don't need the final state.
evalMemory :: forall s es a. s -> Eff (Memory s : es) a -> Eff es a
evalMemory initial = fmap fst . runMemory initial

-- | Run memory effect with pure initial value, discarding final state.
--
-- Alias for 'evalMemory' for symmetry with State naming conventions.
runMemoryPure :: forall s es a. s -> Eff (Memory s : es) a -> Eff es a
runMemoryPure = evalMemory

-- ════════════════════════════════════════════════════════════════════════════
-- FUTURE: PERSISTENT INTERPRETERS
-- ════════════════════════════════════════════════════════════════════════════

-- Note: The following interpreters will be added when persistence is wired up:
--
-- runMemoryPersistent
--   :: (IOE :> es, FromJSON s, ToJSON s)
--   => FilePath -> s -> Eff (Memory s : es) a -> Eff es a
--
-- This would:
-- 1. Load state from file (or use default if missing)
-- 2. Run the action
-- 3. Persist final state to file
--
-- For the graph runner, node memory and global memory will have different
-- persistence strategies:
--
-- * Global: Load at graph start, save at graph end
-- * Node: Load before each node execution, save after
--
-- The persistence backend (SQLite, files, etc.) is a separate concern.
