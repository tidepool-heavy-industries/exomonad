{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Single-Node Recursive Work Graph
--
-- One node that handles everything via self-loops:
--
-- @
-- Entry(WorkInput)
--     │
--     ▼
-- ┌────────────────────────────────────────────┐
-- │                  Work                       │
-- │                                            │
-- │  Exits:                                    │
-- │    Continue ───────────┐                   │
-- │    Spawn [children] ───┼──▶ self-loop      │
-- │    AwaitNext ──────────┘                   │
-- │    Complete ───────────────▶ Exit          │
-- └────────────────────────────────────────────┘
--     │
--     ▼
-- Exit(WorkResult)
-- @
--
-- Children run the same graph recursively (forked session).
-- AwaitNext blocks until a child completes, injects result into template.
module TypesFirstDev.WorkGraph
  ( WorkGraph(..)
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

import TypesFirstDev.Types.Work
  ( WorkInput
  , WorkExit
  , WorkResult
  )
import TypesFirstDev.WorkTemplates (WorkTpl)

-- | Single-node recursive work graph.
--
-- The Work node handles all behaviors via exit type routing:
-- - Continue → self-loop (immediate)
-- - Spawn → spawn children, then self-loop (via AwaitNext)
-- - AwaitNext → self-loop after child completes
-- - Complete → exit graph
data WorkGraph mode = WorkGraph
  { --------------------------------------------------------------------------
    -- ENTRY
    --------------------------------------------------------------------------
    wgEntry :: mode :- G.EntryNode WorkInput

    --------------------------------------------------------------------------
    -- WORK: The single node that does everything
    --------------------------------------------------------------------------
    --
    -- Prompted to:
    -- - Scaffold when it helps children coordinate
    -- - Spawn parallel subtasks at todo-item granularity
    -- - Wait for children and integrate their commits
    -- - Complete when all work is done
    --
    -- Template context includes (injected by effect):
    -- - completedChild: { commitHash, commitMessage } when resuming from AwaitNext
    -- - pendingCount: number of children still running

  , wgWork :: mode :- G.LLMNode
      :@ Types.Input WorkInput
      :@ Template WorkTpl
      :@ Schema WorkExit
      :@ UsesEffects '[ Goto "wgWork" WorkInput      -- Continue/Spawn/AwaitNext self-loop
                      , Goto Exit WorkResult         -- Complete exits graph
                      ]
      :@ ClaudeCode 'Haiku

    --------------------------------------------------------------------------
    -- EXIT
    --------------------------------------------------------------------------
  , wgExit :: mode :- G.ExitNode WorkResult
  }
  deriving Generic
