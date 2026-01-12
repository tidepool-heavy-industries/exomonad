{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Effect interpreter chain for Work graph.
--
-- Effect stack:
-- - Session: Claude Code interaction via mantle
-- - GraphContext: Entry value for handlers
-- - Memory: WorkMem state across self-loops
-- - Subgraph: Child spawning for recursive decomposition
-- - IO: Final conversion
module TypesFirstDev.WorkInterpreters
  ( WorkEffects
  , runWorkEffects
  , WorkConfig(..)
  ) where

import Control.Monad.Freer (Eff, runM)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import Tidepool.Session.Executor (defaultSessionConfig, runSessionIO)
import Tidepool.Actor.Subgraph (SubgraphState, Subgraph, runSubgraph)
import Tidepool.Effect.Session (Session)
import Tidepool.Effect.GraphContext (GraphContext, runGraphContext)
import Tidepool.Graph.Memory (Memory, evalMemory)

import TypesFirstDev.Types.Work (WorkInput, WorkResult)
import TypesFirstDev.WorkHandlers (WorkMem, emptyWorkMem)

-- ════════════════════════════════════════════════════════════════════════════
-- TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for Work graph execution.
data WorkConfig = WorkConfig
  { wcBaseDir :: FilePath
    -- ^ Base directory for worktrees/sessions
  , wcParentBranch :: Text
    -- ^ Parent git branch (e.g., "main")
  , wcMaxDepth :: Int
    -- ^ Maximum recursion depth for children
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT STACK
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect stack for Work graph.
--
-- Includes Memory effect for WorkMem to track state across self-loops.
-- Children are spawned with WorkInput and return WorkResult.
type WorkEffects =
  '[ Session
   , GraphContext WorkInput  -- Entry value for getEntry in handlers
   , Memory WorkMem          -- State across self-loops
   , Subgraph WorkInput WorkResult
   , IO
   ]

-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER CHAIN
-- ════════════════════════════════════════════════════════════════════════════

-- | Run the Work effect stack to IO.
--
-- Stack order (innermost to outermost for runM):
-- 1. Session for Claude Code (innermost)
-- 2. GraphContext for entry access
-- 3. Memory for WorkMem state
-- 4. Subgraph for child spawning
-- 5. IO (outermost)
runWorkEffects
  :: SubgraphState WorkInput WorkResult
  -> WorkInput  -- Entry value for GraphContext
  -> WorkConfig
  -> Eff WorkEffects a
  -> IO a
runWorkEffects subgraphState entryInput config action =
  runM
    . runSubgraph subgraphState
    . evalMemory emptyWorkMem
    . runGraphContext entryInput
    . runSessionIO (defaultSessionConfig config.wcBaseDir)
    $ action
