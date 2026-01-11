{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | V3 Effect Interpreter Chain
--
-- Composes interpreters for the V3 TDD protocol:
-- - Session: Claude Code session management via mantle
-- - Memory: TDDMem (shared) and ImplMem (node-private) state
-- - Subgraph: Child graph spawning (handled by system infrastructure)
-- - IO: Final conversion to IO monad
--
-- Interpreter order (innermost to outermost):
--   runM . runSessionIO . runMemoryIO . runMemoryIO . runSubgraph
--
-- Note: Build and Worktree operations are handled by system infrastructure
-- (mantle, git, cabal). Not exposed as effects in Phase 8.
module TypesFirstDev.V3.Interpreters
  ( V3Effects
  , V3Result(..)
  , runV3Effects
  , defaultSessionConfig
  , WorktreeConfig(..)
  ) where

import Control.Monad.Freer (Eff, LastMember, interpret, sendM, runM)
import Control.Concurrent.STM (TVar, readTVarIO, atomically, readTVar, writeTVar)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import Tidepool.Session.Executor (defaultSessionConfig, runSessionIO)
import Tidepool.Graph.Memory (Memory(GetMem, UpdateMem))
import Tidepool.Actor.Subgraph (SubgraphState, Subgraph, runSubgraph)
import Tidepool.Effect.Session (Session)

-- Placeholder imports - these will be updated once types are finalized
import TypesFirstDev.Types.Memory (TDDMem, ImplMem)
import TypesFirstDev.Types.Core (Spec)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Worktree configuration (for future use - currently handled by system)
data WorktreeConfig = WorktreeConfig
  { wtcBaseDir :: FilePath
    -- ^ Base directory for worktrees (e.g., ".worktrees")
  , wtcParentBranch :: Text
    -- ^ Parent git branch (e.g., "main")
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- | V3 execution result (simplified for Phase 8)
data V3Result = V3Success | V3Failure Text
  deriving (Show, Eq)

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT STACK
-- ════════════════════════════════════════════════════════════════════════════

-- | Concrete effect stack for V3 TDD protocol.
-- All handlers must satisfy their constraints with this stack.
--
-- Note: Build and Worktree are handled by system infrastructure, not as effects.
-- Child spawning is handled by Subgraph effect + mantle machinery.
type V3Effects =
  '[ Session
   , Memory TDDMem
   , Memory ImplMem
   , Subgraph Spec V3Result
   , IO
   ]

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT INTERPRETER CHAIN
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the full V3 effect stack to IO.
--
-- Stacks interpreters in order:
-- 1. IO monad at the outermost layer
-- 2. Subgraph for child spawning (uses deferred binding via SubgraphState)
-- 3. Memory ImplMem for impl-private state
-- 4. Memory TDDMem for shared TDD state
-- 5. Session for Claude Code interaction (innermost)
--
-- The SubgraphState is created by withRecursiveGraph in the runner.
-- The wire function sets up the deferred graph runner before execution.
runV3Effects
  :: SubgraphState Spec V3Result
  -> TVar TDDMem
  -> TVar ImplMem
  -> WorktreeConfig  -- Provides base directory for session config
  -> Eff V3Effects a
  -> IO a
runV3Effects subgraphState tddMem implMem wtConfig action =
  runM
    . runSubgraph subgraphState
    . runMemoryIO @ImplMem implMem
    . runMemoryIO @TDDMem tddMem
    . runSessionIO (defaultSessionConfig wtConfig.wtcBaseDir)
    $ action

-- ════════════════════════════════════════════════════════════════════════════
-- INDIVIDUAL INTERPRETERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Interpret Memory effect using TVar for thread-safe state.
--
-- Each memory type (TDDMem, ImplMem) gets its own TVar instance.
-- GetMem reads the current value via readTVarIO.
-- UpdateMem atomically applies a function to the stored state.
runMemoryIO :: forall s effs a.
               (LastMember IO effs)
            => TVar s
            -> Eff (Memory s ': effs) a
            -> Eff effs a
runMemoryIO tvar = interpret $ \case
  GetMem -> sendM $ readTVarIO tvar
  UpdateMem f -> sendM $ atomically $ do
    current <- readTVar tvar
    let updated = f current
    writeTVar tvar updated
    pure ()
