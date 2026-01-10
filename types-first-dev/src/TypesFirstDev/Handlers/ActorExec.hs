{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Actor runtime execution for the hybrid TDD graph.
--
-- This module provides the bridge between the effectful handler record and the
-- actor runtime system. The key idea:
--
-- 1. Handlers are defined with the full HybridEffects stack
-- 2. For actor execution, we need to convert them to NodeHandler form
-- 3. The effect interpreters are captured per-actor via handler builders
-- 4. runGraphAsActors orchestrates the flow
--
-- This enables:
-- * Type-safe routing between 20+ nodes
-- * Parallel blind execution via ForkNode/BarrierNode
-- * Clean separation: handlers don't need to know about routing logic
-- * Reusable effect stack (Worktree, ClaudeCodeExec, Build, etc.)
--
-- Architecture:
--
-- @
-- Effect Stack (top)
--   ├─ Worktree
--   ├─ ClaudeCodeExec
--   ├─ Build
--   ├─ Reader StackSpec
--   ├─ Memory SessionContext
--   ├─ Error WorkflowError
--   └─ IO (bottom)
--       │
--       v
--   sendM $ runGraphAsActors handlers input
--       │
--       v
--   Actor System (in separate threads)
--       ├─ [entry] -> [hTypes] -> [hSkeleton] -> ...
--       ├─ [hFork] spawns workers
--       ├─ [hTests] and [hImpl] run parallel
--       └─ [hJoin] collects results
-- @
module TypesFirstDev.Handlers.ActorExec
  ( -- * Actor-based Graph Execution
    runGraphAsHybridActors

    -- * Re-exports
  , runGraphAsActors
  , HandlerBuilder
  , NodeHandler
  ) where

import Control.Monad (void)
import Control.Monad.Freer (Eff, sendM)
import Data.Aeson (Value, ToJSON(..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Tidepool.Actor.Graph
  ( runGraphAsActors
  , HandlerBuilder
  , NodeHandler
  )

import TypesFirstDev.Handlers.Hybrid.Effects (HybridEffects)


-- ════════════════════════════════════════════════════════════════════════════
-- ACTOR-BASED GRAPH EXECUTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the hybrid graph using the actor runtime system.
--
-- This lifts 'runGraphAsActors' into the HybridEffects stack, allowing
-- handlers to access the full effect context (Worktree, ClaudeCodeExec, Build, etc.)
-- while executing through the actor system.
--
-- The actor system provides:
-- * Type-safe routing between nodes via typed GotoChoice
-- * Automatic parallel execution coordination (ForkNode/BarrierNode)
-- * Clean handler isolation (each node independent)
-- * Deterministic testing support (can swap interpreters)
--
-- Flow:
--
-- 1. Build effect interpreter stack (Worktree, ClaudeCodeExec, Build, ...)
-- 2. Create handler builders that capture the effect interpreters
-- 3. Inside runGraphAsActors:
--    a. Spawn one actor per handler builder
--    b. Route payloads between actors via JSON
--    c. When a handler completes (gotoExit), return the result
--
-- @
-- handlers <- buildHybridActorHandlers hybridGenesisHandlers
-- result <- runGraphAsHybridActors handlers (toJSON spec)
-- @
--
-- TODO: Once ClaudeCodeLLMHandler types are migrated to Session effect,
--       implement buildHybridActorHandlers to extract and wrap handlers from
--       the record.
runGraphAsHybridActors
  :: forall result.
     ( ToJSON result
     )
  => Map.Map Text HandlerBuilder  -- ^ Actor handler map (one per node)
  -> Value                         -- ^ Initial input (to "entry" handler)
  -> Eff HybridEffects result
runGraphAsHybridActors handlers initialInput = do
  result <- sendM $ runGraphAsActors handlers initialInput
  pure result


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLER EXTRACTION (TODO)
-- ════════════════════════════════════════════════════════════════════════════

-- | Build the actor handler map from the hybrid handlers record.
--
-- This extracts each handler from the record and wraps it in a HandlerBuilder.
--
-- TODO: Implementation requires:
-- 1. ClaudeCodeLLMHandler -> SessionExecutor migration
-- 2. Generic traversal of TypesFirstGraphHybrid record
-- 3. Per-handler effect interpreter binding
--
-- For now, handlers are manually constructed in Main.hs and passed via a builder
-- function that captures the full effect stack.
buildHybridActorHandlers
  :: forall result.
     ()  -- TODO: Add parameters
  -> Eff HybridEffects (Map.Map Text HandlerBuilder)
buildHybridActorHandlers () = do
  -- TODO: Extract handlers from record and wrap each one
  -- Example structure:
  --   handlers = Map.fromList
  --     [ ("entry", pureHandler entryHandler)
  --     , ("hTypes", effHandler runHybridEffects hTypesHandler)
  --     , ("hFork", forkHandler runHybridEffects hForkHandler)
  --     , ("hJoin", barrierHandler runHybridEffects hJoinHandler)
  --     , ... (20+ total handlers)
  --     ]
  error "buildHybridActorHandlers: TODO - implement handler extraction"
