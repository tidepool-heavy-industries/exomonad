{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | V3 Effect Interpreter Chain
--
-- Composes interpreters for the V3 TDD protocol:
-- - Session: Claude Code session management via mantle
-- - GraphContext: Graph-level entry value access
-- - ExecutionContext: Runtime dependencies (Spec, Scaffold, NodeInfo)
-- - Memory: TDDMem (shared) and ImplMem (node-private) state
-- - Subgraph: Child graph spawning (handled by system infrastructure)
-- - IO: Final conversion to IO monad
--
-- Interpreter order (innermost to outermost):
--   runM . runSessionIO . runReader . runGraphContext . runMemoryIO . runMemoryIO . runSubgraph
--
-- Note: Build and Worktree operations are handled by system infrastructure
-- (mantle, git, cabal). Not exposed as effects in Phase 8.
module TypesFirstDev.V3.Interpreters
  ( V3Effects
  , V3Result(..)
  , ExecutionContext(..)
  , runV3Effects
  , defaultSessionConfig
  , WorktreeConfig(..)
  ) where

import Control.Monad.Freer (Eff, LastMember, interpret, sendM, runM)
import Control.Monad.Freer.Reader (Reader, runReader)
import Control.Concurrent.STM (TVar, readTVarIO, atomically, readTVar, writeTVar)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import Tidepool.Session.Executor (defaultSessionConfig, runSessionIO)
import Tidepool.Graph.Memory (Memory(GetMem, UpdateMem))
import Tidepool.Actor.Subgraph (SubgraphState, Subgraph, runSubgraph)
import Tidepool.Effect.Session (Session)
import Tidepool.Effect.GraphContext (GraphContext, runGraphContext)

-- Placeholder imports - these will be updated once types are finalized
import TypesFirstDev.Types.Memory (TDDMem, ImplMem)
import TypesFirstDev.Types.Core (Spec)
import TypesFirstDev.Types.Payloads (MergeComplete, InitWorkPayload)
import TypesFirstDev.Types.Nodes (ScaffoldInput)
import TypesFirstDev.Types.Shared (NodeInfo)

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

-- | Execution context for handlers to access runtime dependencies.
--
-- This provides handlers with read-only access to:
-- - The original Spec that initiated this graph execution
-- - The Scaffold output (once available after Scaffold node completes)
-- - Node identification info for routing and git operations
--
-- Pattern: Handlers call @ask \@ExecutionContext@ to read context.
-- Context is immutable within a graph execution; different children
-- get different contexts via Subgraph spawning.
--
-- Prefix: ec
data ExecutionContext = ExecutionContext
  { ecSpec :: Spec
    -- ^ The original spec that initiated this graph execution.
    -- Immutable throughout execution.
  , ecScaffold :: Maybe InitWorkPayload
    -- ^ Scaffold output. 'Nothing' during Scaffold node, 'Just' after.
    -- Updated via runV3Effects when scaffold completes.
  , ecNodeInfo :: Maybe NodeInfo
    -- ^ Current node identification. Used for git branch naming
    -- and routing decisions.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT STACK
-- ════════════════════════════════════════════════════════════════════════════

-- | Concrete effect stack for V3 TDD protocol.
-- All handlers must satisfy their constraints with this stack.
--
-- Note: Build and Worktree are handled by system infrastructure, not as effects.
-- Child spawning is handled by Subgraph effect + mantle machinery.
--
-- Effect ordering (innermost to outermost):
--   Session → Reader ExecutionContext → GraphContext → Memory → Memory → Subgraph → IO
type V3Effects =
  '[ Session
   , Reader ExecutionContext     -- Runtime dependencies (Spec, Scaffold, NodeInfo)
   , GraphContext ScaffoldInput  -- Graph-level: entry value for entire graph
   , Memory TDDMem
   , Memory ImplMem
   , Subgraph ScaffoldInput MergeComplete  -- Children spawned with full ScaffoldInput
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
-- 5. GraphContext for graph-level entry access
-- 6. Reader ExecutionContext for runtime dependencies
-- 7. Session for Claude Code interaction (innermost)
--
-- The SubgraphState is created by withRecursiveGraph in the runner.
-- The wire function sets up the deferred graph runner before execution.
runV3Effects
  :: SubgraphState ScaffoldInput MergeComplete
  -> TVar TDDMem
  -> TVar ImplMem
  -> ScaffoldInput  -- Entry value for GraphContext effect
  -> ExecutionContext  -- Runtime dependencies for handlers
  -> WorktreeConfig  -- Provides base directory for session config
  -> Eff V3Effects a
  -> IO a
runV3Effects subgraphState tddMem implMem scaffoldInput execCtx wtConfig action =
  runM
    . runSubgraph subgraphState
    . runMemoryIO @ImplMem implMem
    . runMemoryIO @TDDMem tddMem
    . runGraphContext scaffoldInput  -- GraphContext: any handler can call getEntry
    . runReader execCtx              -- ExecutionContext: handlers read with ask
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
