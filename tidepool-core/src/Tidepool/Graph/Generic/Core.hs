{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Core types for the record-based (Servant-style) Graph DSL.
--
-- This module exists to break the circular dependency between Generic.hs
-- and RecordStructure.hs. Both modules import these shared types.
module Tidepool.Graph.Generic.Core
  ( -- * Mode Class
    GraphMode(..)
  , (:-) -- Re-export the type family for convenience

    -- * Modes
  , AsGraph

    -- * Node Markers
  , LLMNode
  , LogicNode
  , Entry
  , Exit

    -- * Parallel Execution Markers
  , ForkNode
  , BarrierNode
  ) where

import Data.Kind (Type)

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH MODE CLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Mode determines how graph record fields are interpreted.
--
-- Modes are the key to the Servant-style pattern:
--
-- @
-- data MyGraph mode = MyGraph
--   { entry    :: mode :- Entry Message
--   , classify :: mode :- LLMNode :@ Input Message :@ Schema Intent
--   , exit     :: mode :- Exit Response
--   }
-- @
--
-- With 'AsGraph' mode, fields are the node definitions themselves.
-- With 'AsHandler es' mode (in Generic.hs), fields become handler function types.
class GraphMode mode where
  type mode :- nodeDef :: Type

infixl 0 :-

-- ════════════════════════════════════════════════════════════════════════════
-- MODES
-- ════════════════════════════════════════════════════════════════════════════

-- | Identity mode - fields contain node definitions as-is.
--
-- This mode is used for:
-- * Type-level graph analysis and validation
-- * Extracting graph structure via Generic
data AsGraph

instance GraphMode AsGraph where
  type AsGraph :- nodeDef = nodeDef

-- ════════════════════════════════════════════════════════════════════════════
-- NODE MARKERS
-- ════════════════════════════════════════════════════════════════════════════

-- | LLM node marker.
--
-- In record syntax, use this instead of the 'LLM' kind from Types.hs:
--
-- @
-- data MyGraph mode = MyGraph
--   { classify :: mode :- LLMNode :@ Input Message :@ Schema Intent
--   }
-- @
--
-- LLMNode has kind 'Type' (unlike 'LLM' which has kind 'NodeKind').
data LLMNode

-- | Logic node marker.
--
-- For pure or effect-based routing logic:
--
-- @
-- data MyGraph mode = MyGraph
--   { router :: mode :- LogicNode :@ Input Intent :@ UsesEffects '[Goto "next" ...]
--   }
-- @
data LogicNode

-- | Entry point marker (parameterized by input type).
--
-- @
-- data MyGraph mode = MyGraph
--   { entry :: mode :- Entry Message
--   }
-- @
type Entry :: Type -> Type
data Entry inputType

-- | Exit point marker (parameterized by output type).
--
-- @
-- data MyGraph mode = MyGraph
--   { exit :: mode :- Exit Response
--   }
-- @
type Exit :: Type -> Type
data Exit outputType

-- ════════════════════════════════════════════════════════════════════════════
-- PARALLEL EXECUTION MARKERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Fork node marker - spawns parallel execution paths.
--
-- ForkNode receives input and spawns multiple worker paths that execute
-- in parallel. Each spawned path runs independently until it calls 'Arrive'
-- to deposit its result at a barrier.
--
-- @
-- data MyGraph mode = MyGraph
--   { fork :: mode :- ForkNode
--       :@ Input Task
--       :@ Spawn '[To "worker1" Task, To "worker2" Task]
--       :@ Barrier "merge"
--   }
-- @
--
-- The 'Spawn' annotation lists targets (using 'To' markers from Goto).
-- The 'Barrier' annotation names which BarrierNode collects the results.
data ForkNode

-- | Barrier node marker - synchronizes parallel execution paths.
--
-- BarrierNode waits for all spawned paths to arrive, then continues
-- with the collected results. It fires only when all expected arrivals
-- have been deposited.
--
-- @
-- data MyGraph mode = MyGraph
--   { merge :: mode :- BarrierNode
--       :@ Awaits '[ResultA, ResultB]
--       :@ UsesEffects '[Goto Exit (ResultA, ResultB)]
--   }
-- @
--
-- The 'Awaits' annotation lists the types expected from each spawned path.
-- The handler receives the collected results as an HList.
data BarrierNode
