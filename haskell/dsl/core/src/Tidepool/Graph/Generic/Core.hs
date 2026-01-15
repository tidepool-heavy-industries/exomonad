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
    GraphMode(..)  -- Exports :- operator via (..)

    -- * Modes
  , AsGraph

    -- * Node Markers
  , LLMNode
  , LogicNode
  , GraphNode
  , EntryNode
  , ExitNode

    -- * Parallel Execution Markers
  , ForkNode
  , BarrierNode

    -- * Phantom Wrappers
  , NodeRef(..)
  , GetNodeName
  ) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Tidepool.Graph.Types (LLMKind)

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH MODE CLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Mode determines how graph record fields are interpreted.
--
-- Modes are the key to the Servant-style pattern:
--
-- @
-- data MyGraph mode = MyGraph
--   { entry    :: mode :- EntryNode Message
--   , classify :: mode :- LLMNode :@ Input Message :@ Schema Intent
--   , exit     :: mode :- ExitNode Response
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

-- | LLM node marker with subtype parameter.
--
-- The subtype parameter determines execution model and tool format:
--
-- * 'API' - Direct Anthropic/Cloudflare API calls (JSON Schema via MCP)
-- * 'CodingAgent' - Claude Code subprocess via mantle (JSON Schema via MCP)
-- * 'Local' - FunctionGemma streaming (PEG grammar, streaming fold)
--
-- In record syntax with config records:
--
-- @
-- data MyGraph mode = MyGraph
--   { gWork :: mode :- LLMNode 'API WorkConfig
--   , gCode :: mode :- LLMNode 'CodingAgent CodeConfig
--   }
-- @
--
-- LLMNode has kind 'LLMKind -> Type' (parameterized by LLMKind from Types.hs).
type LLMNode :: LLMKind -> Type
data LLMNode subtype

-- | Graph node marker - embeds a subgraph as a node.
--
-- GraphNode allows graphs to contain graphs, enabling compositional
-- agent design at any scale. The subgraph parameter is a graph type
-- constructor (kind: Type -> Type).
--
-- @
-- data ParentGraph mode = ParentGraph
--   { scout :: mode :- GraphNode SemanticScoutGraph :@ Input Order
--   }
-- @
--
-- The Input annotation wires to the child graph's EntryNode type.
-- Exit type is inferred via GetGraphExit.
type GraphNode :: (Type -> Type) -> Type
data GraphNode subgraph

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

-- | EntryNode point marker (parameterized by input type).
--
-- @
-- data MyGraph mode = MyGraph
--   { entry :: mode :- EntryNode Message
--   }
-- @
type EntryNode :: Type -> Type
data EntryNode inputType

-- | Exit point marker (parameterized by output type).
--
-- @
-- data MyGraph mode = MyGraph
--   { exit :: mode :- ExitNode Response
--   }
-- @
type ExitNode :: Type -> Type
data ExitNode outputType

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

-- ════════════════════════════════════════════════════════════════════════════
-- PHANTOM WRAPPERS (Field-Witness Routing)
-- ════════════════════════════════════════════════════════════════════════════

-- | Phantom wrapper that carries node name as type-level Symbol.
--
-- Used for field-witness routing where field accessors act as type witnesses:
--
-- @
-- data MyGraph mode = MyGraph
--   { gWork :: NodeRef "gWork" (mode :- LLMNode 'API WorkConfig)
--   , gExit :: NodeRef "gExit" (mode :- ExitNode Result)
--   }
--
-- -- Field-witness routing (no string literals!)
-- goto (gWork graph) (retry . entries) retryInfo
-- @
--
-- The phantom parameter ensures the node name is known at compile time,
-- enabling type-safe routing validation.
type NodeRef :: Symbol -> Type -> Type
newtype NodeRef name nodeType = NodeRef nodeType

-- | Extract node name from NodeRef phantom parameter.
--
-- Used by routing type families to validate target names at compile time:
--
-- @
-- GetNodeName (NodeRef "gWork" someType) = "gWork"
-- @
type GetNodeName :: Type -> Symbol
type family GetNodeName ref where
  GetNodeName (NodeRef name _) = name
