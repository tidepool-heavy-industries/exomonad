{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | V2 Graph: Tree-based TDD decomposition workflow.
--
-- The graph is beautifully simple:
--
-- @
-- Entry(Spec)
--     │
--     ▼
-- hScaffold (LLM) ─── writes tests + stubs + rubric, commits
--     │
--     ▼
-- hImplement (Logic + Subgraph) ─── spawn children if needed, then implement
--     │
--     │ if subsystems identified:
--     │   1. Spawn child graph for each subsystem
--     │   2. Await loop: merge MRs as children complete
--     │   3. (Git rebase cascade happens automatically)
--     │   4. All children done: implement glue layer
--     │
--     │ if no subsystems (leaf):
--     │   Implement stubs directly
--     │
--     ▼
-- Exit(V2Result) ─── yields MR to parent
-- @
--
-- Key insight: Tests are part of scaffolding, not a parallel phase.
-- Children implement stubbed subsystems; parent implements glue.
-- Tree structure emerges from SpawnSelf calls, not graph topology.
--
-- The same 4-node graph handles both decomposition and leaf cases.
-- Behavior differs only in whether hImplement spawns children.
module TypesFirstDev.Graph.V2
  ( TypesFirstGraphV2(..)
  ) where

import GHC.Generics (Generic)

import Tidepool.Graph.Types
  ( type (:@)
  , Schema
  , Template
  , UsesEffects
  , Exit
  )
import qualified Tidepool.Graph.Types as Types (Input)
import Tidepool.Graph.Generic (GraphMode(..))
import qualified Tidepool.Graph.Generic as G
import Tidepool.Graph.Goto (Goto)

import Tidepool.Effect.Subgraph (Subgraph)

import TypesFirstDev.Types.V2
  ( Spec
  , ScaffoldingOutput
  , ScaffoldingResult
  , V2Result
  )

import TypesFirstDev.Templates.V2 (V2ScaffoldingTpl)


-- | V2 Tree-based TDD decomposition workflow.
--
-- Only 4 nodes:
-- 1. Entry - receives Spec
-- 2. Scaffold - LLM writes tests + stubs + rubric, commits
-- 3. Implement - Logic node that may spawn children, then implements
-- 4. Exit - yields V2Result (MR to parent)
--
-- The power is in the Subgraph effect used by hImplement:
-- * SpawnSelf creates child graph instances
-- * AwaitAny blocks until a child completes
-- * Child completion → merge MR → git rebase cascade to siblings
data TypesFirstGraphV2 mode = TypesFirstGraphV2
  { --------------------------------------------------------------------------
    -- ENTRY
    --------------------------------------------------------------------------
    hEntry :: mode :- G.Entry Spec

    --------------------------------------------------------------------------
    -- SCAFFOLD PHASE
    --------------------------------------------------------------------------

    -- | TDD scaffolding: writes tests, stubs, and identifies subsystems.
    --
    -- This phase does the heavy lifting:
    -- * Parse acceptance criteria
    -- * Write QuickCheck properties (TDD: tests that fail on undefined)
    -- * Write type stubs and function signatures (undefined implementations)
    -- * Identify natural decomposition boundaries (facets)
    -- * Commit all scaffolding
    --
    -- The rubric output enables decomposition decision without
    -- the LLM knowing what we'll do with it (value-neutral design).
  , hScaffold :: mode :- G.LLMNode
      :@ Types.Input Spec
      :@ Template V2ScaffoldingTpl
      :@ Schema ScaffoldingOutput
      :@ UsesEffects '[Goto "hImplement" ScaffoldingResult]

    --------------------------------------------------------------------------
    -- IMPLEMENT PHASE
    --------------------------------------------------------------------------

    -- | Implementation: maybe spawn children, then implement.
    --
    -- This is where the magic happens. The handler:
    --
    -- 1. Checks shouldDecompose on rubric (policy decision)
    --
    -- 2. If decomposing:
    --    a. Derive child specs from facets
    --    b. SpawnSelf for each child (creates parallel graph instances)
    --    c. Await loop: as each child completes, merge its MR
    --       - This advances our branch HEAD
    --       - Siblings see via git → rebase cascade
    --    d. All children done: now the stubs are real code!
    --    e. Implement glue layer using the subsystems
    --
    -- 3. If leaf (no decomposition):
    --    Implement all stubs directly
    --
    -- 4. Run CI loop until tests pass (iterate on failures)
    --
    -- 5. Return V2Result → becomes MR to parent
    --
    -- The Subgraph effect is the key enabler. It lets us spawn
    -- new instances of THIS SAME GRAPH with different Specs.
  , hImplement :: mode :- G.LogicNode
      :@ Types.Input ScaffoldingResult
      :@ UsesEffects
           '[ Subgraph Spec V2Result  -- Can spawn child instances
            , Goto Exit V2Result      -- Exit with MR
            ]

    --------------------------------------------------------------------------
    -- EXIT
    --------------------------------------------------------------------------

    -- | Exit yields V2Result which becomes MR to parent (or final result).
  , hExit :: mode :- G.Exit V2Result
  }
  deriving Generic
