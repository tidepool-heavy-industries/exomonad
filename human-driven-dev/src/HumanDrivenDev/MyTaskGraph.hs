{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | MyTask Graph Definition
--
-- Single-node graph with self-loops for retry/continuation.
--
-- @
-- Entry(TopLevelInput)
--     │
--     ▼
-- ┌────────────────────────────────────────────┐
-- │              MyTask                        │
-- │                                            │
-- │  Exits:                                    │
-- │    TaskComplete ─────────▶ Exit            │
-- │    TaskRetry ────────────┐                │
-- │    TaskSpawn ────────────┼──▶ self-loop   │
-- │    TaskFailed ───────────┘                │
-- └────────────────────────────────────────────┘
--     │
--     ▼
-- Exit(FinalResult)
-- @
module HumanDrivenDev.MyTaskGraph
  ( MyTaskGraph(..)
  ) where

import GHC.Generics (Generic)

import Tidepool.Graph.Types
  ( type (:@)
  , Schema
  , Template
  , UsesEffects
  , Exit
  , ClaudeCode
  , ModelChoice(..)
  )
import qualified Tidepool.Graph.Types as Types (Input)
import Tidepool.Graph.Generic (GraphMode(..))
import qualified Tidepool.Graph.Generic as G
import Tidepool.Graph.Goto (Goto)

import HumanDrivenDev.Types.Core (TopLevelInput, FinalResult)
import HumanDrivenDev.Types.MyTask (MyTaskInput, MyTaskExit)
import HumanDrivenDev.MyTaskTemplates (MyTaskTpl)

-- | Single-node task graph
--
-- The MyTask node handles all work via exit type routing:
-- - TaskComplete → exit with result
-- - TaskRetry → self-loop with adjustments
-- - TaskSpawn → spawn subtasks, then self-loop
-- - TaskFailed → exit with error
data MyTaskGraph mode = MyTaskGraph
  { --------------------------------------------------------------------------
    -- ENTRY
    --------------------------------------------------------------------------
    mtgEntry :: mode :- G.Entry TopLevelInput

    --------------------------------------------------------------------------
    -- MY TASK: Main work node
    --------------------------------------------------------------------------
    --
    -- Prompted to:
    -- - Analyze the input and context
    -- - Attempt to complete the task
    -- - Decide whether to complete, retry, spawn subtasks, or fail
    --
    -- Exit type determines next transition:
    -- - TaskComplete: done, return result
    -- - TaskRetry: loop back with adjustments
    -- - TaskSpawn: spawn children, wait for completion
    -- - TaskFailed: abort with diagnostics

  , mtgMyTask :: mode :- G.LLMNode
      :@ Types.Input MyTaskInput
      :@ Template MyTaskTpl
      :@ Schema MyTaskExit
      :@ UsesEffects '[ Goto "mtgMyTask" MyTaskInput  -- Self-loop for retry/continuation
                      , Goto Exit FinalResult          -- Exit on complete/failure
                      ]

    --------------------------------------------------------------------------
    -- EXIT
    --------------------------------------------------------------------------
  , mtgExit :: mode :- G.Exit FinalResult
  }
  deriving Generic
