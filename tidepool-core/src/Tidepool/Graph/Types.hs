
-- | Core types for the Tidepool Graph DSL.
--
-- This module defines core types for the Graph DSL.
-- Users should use the record-based DSL via Tidepool.Graph.Generic;
-- the NodeKind types (LLM, Logic) below are for internal use only.
module Tidepool.Graph.Types
  ( -- * Node Kind
    NodeKind(..)

    -- * Annotations
  , type (:@)
  , Input
  , Schema
  , System
  , Template
  , Vision
  , Tools
  , UsesEffects
  , Memory

    -- * Fork/Barrier Annotations
  , Spawn
  , Barrier
  , Awaits
  , Arrive(..)

    -- * Graph-Level Annotations
  , type (:&)
  , Groups
  , Requires
  , Global
  , Backend

    -- * API Backend Types
  , CloudflareAI
  , NativeAnthropic

    -- * ClaudeCode Annotation
  , ClaudeCode
  , ModelChoice(..)
  , Haiku
  , Sonnet
  , Opus

    -- * ClaudeCode Singletons (demote type-level to runtime)
  , SingModelChoice(..)
  , KnownMaybeCwd(..)

    -- * Special Goto Targets
  , Exit
  , Self

    -- * Heterogeneous Lists
  , HList(..)
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

-- ════════════════════════════════════════════════════════════════════════════
-- NODE KIND
-- ════════════════════════════════════════════════════════════════════════════

-- | The kind of a node determines its behavior:
--
-- * 'LLM' nodes call the language model and produce output via 'Schema'
-- * 'Logic' nodes run pure or effect-based code and transition via 'Goto'
--
-- Note: The bare NodeKind types (LLM, Logic) are obsolete.
-- For the record-based DSL (the only supported syntax), use LLMNode and LogicNode
-- from Tidepool.Graph.Generic.
data NodeKind
  = LLM    -- ^ Node that invokes the LLM. Output flows implicitly via Schema.
  | Logic  -- ^ Node with effect stack. Transitions explicitly via Goto.

-- ════════════════════════════════════════════════════════════════════════════
-- ANNOTATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Attach an annotation to a node. Annotations are applied left-to-right:
-- @mode :- G.LLMNode :@ Input A :@ Schema B@ has Input and Schema annotations.
type (:@) :: Type -> Type -> Type
data node :@ annotation
infixl 7 :@

-- | Declares the input type for a node. The handler receives exactly this type.
--
-- For fan-in patterns (multiple sources), use 'Either':
-- @Input (Either FromNodeA FromNodeB)@
--
-- For multiple simultaneous inputs, use tuples:
-- @Input (A, B)@
type Input :: Type -> Type
data Input inputType

-- | Declares the output type of an LLM node. This output becomes available
-- as input to downstream nodes.
type Schema :: Type -> Type
data Schema output

-- | System prompt template for an LLM node. Rendered before the user prompt.
-- Uses a separate TemplateDef from the user 'Template' annotation.
--
-- @
-- gClassify :: mode :- G.LLMNode
--     :@ System ClassifySystemTpl   -- System prompt (optional)
--     :@ Template ClassifyUserTpl   -- User prompt
--     :@ Schema Intent
-- @
type System :: Type -> Type
data System tpl

-- | User prompt template for an LLM node. This is the main prompt that
-- contains the request/context for the LLM.
type Template :: Type -> Type
data Template tpl

-- | Marker for LLM nodes that process images/vision input.
data Vision

-- | List of tools available to an LLM node during execution.
type Tools :: [Type] -> Type
data Tools tools

-- | Effect stack for Logic nodes. Contains the effects the handler can use,
-- including 'Goto' effects for transitions.
--
-- Note: This takes a list of effects with kind [Effect] where
-- Effect = (Type -> Type) -> Type -> Type
--
-- Renamed from 'Eff' to avoid conflict with Effectful's 'Eff' monad type.
--
-- @
-- UsesEffects '[State MyState, Goto "nextNode" PayloadType, Goto Exit ResultType]
-- @
type UsesEffects :: [k] -> Type
data UsesEffects effects

-- | Node-private persistent memory. Each node can declare its own state type
-- that persists across graph runs. Only this node can access its Memory.
--
-- @
-- gExplore :: mode :- G.LLMNode
--     :@ Input Query
--     :@ Schema Findings
--     :@ Memory ExploreMem   -- Private state for this node
-- @
type Memory :: Type -> Type
data Memory stateType

-- ════════════════════════════════════════════════════════════════════════════
-- FORK/BARRIER ANNOTATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Spawn targets for ForkNode. Lists which nodes to spawn in parallel.
--
-- Uses 'To' markers (from Goto module) to specify target name and payload type:
--
-- @
-- fork :: mode :- ForkNode
--     :@ Input Task
--     :@ Spawn '[To "worker1" Task, To "worker2" Task]
--     :@ Barrier "merge"
-- @
--
-- The ForkNode handler returns an HList of payloads, one per spawn target.
type Spawn :: [Type] -> Type
data Spawn targets

-- | Barrier target for ForkNode. Names which BarrierNode collects the results.
--
-- @
-- fork :: mode :- ForkNode
--     :@ Input Task
--     :@ Spawn '[To "w1" Task, To "w2" Task]
--     :@ Barrier "merge"   -- Results collected at "merge" node
-- @
type Barrier :: Symbol -> Type
data Barrier target

-- | Types expected by a BarrierNode. Lists the result types from each path.
--
-- @
-- merge :: mode :- BarrierNode
--     :@ Awaits '[ResultA, ResultB]   -- Expects two results
--     :@ UsesEffects '[Goto Exit (ResultA, ResultB)]
-- @
--
-- The BarrierNode handler receives the collected results as an HList.
-- Supports heterogeneous types: different workers can produce different types.
type Awaits :: [Type] -> Type
data Awaits resultTypes

-- | Arrive annotation for worker nodes spawned by ForkNode.
--
-- Used in UsesEffects to indicate the worker deposits a result at its barrier:
--
-- @
-- worker :: mode :- LLMNode
--     :@ Input Task
--     :@ Schema StepResult
--     :@ UsesEffects '[Goto Self Task, Arrive Result]
-- @
--
-- Unlike 'Goto Exit', which terminates the graph, 'Arrive' suspends the
-- current path and deposits the result for the barrier to collect.
-- Workers can self-loop ('Goto Self') until ready, then 'Arrive'.
--
-- Note: The extra type parameter @r@ gives 'Arrive' the same kind as other
-- effects (Type -> Type), so it can appear in 'UsesEffects' alongside 'Goto'.
type Arrive :: Type -> Type -> Type
data Arrive resultType r where
  ArriveOp :: result -> Arrive result ()

-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH-LEVEL ANNOTATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Attach a graph-level annotation (not commonly used with record-based graphs).
type (:&) :: Type -> Type -> Type
data graph :& annotation
infixl 4 :&

-- | Organize nodes into named groups for Mermaid subgraph rendering.
--
-- @
-- Groups '[
--     '("intake", '["gClassify", "gRoute"])
--   , '("handlers", '["gRefund", "gTechnical", "gBilling"])
--   ]
-- @
type Groups :: [(Symbol, [Symbol])] -> Type
data Groups groups

-- | Declare effects required by the graph at the top level.
-- Used for documentation and runner configuration.
type Requires :: [Type] -> Type
data Requires effects

-- | Graph-level shared state accessible to all nodes. Unlike node-private
-- 'Memory', Global state can be read and updated by any node in the graph.
--
-- Used with the (:&) operator for graph-level annotations.
type Global :: Type -> Type
data Global stateType

-- | API backend selection. Determines whether LLM calls go to Cloudflare AI
-- or the native Anthropic API. Used with the (:&) operator.
--
-- @
-- type MyGraph = Graph '[...] :& Backend NativeAnthropic
-- @
--
-- Note: 'ClaudeCode' annotation is only valid with 'NativeAnthropic' backend.
-- Using ClaudeCode with CloudflareAI will produce a compile-time error.
type Backend :: Type -> Type
data Backend backendType

-- ════════════════════════════════════════════════════════════════════════════
-- API BACKEND TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Cloudflare AI backend. Uses Cloudflare Workers AI for LLM calls.
-- Does not support 'ClaudeCode' annotation (no local subprocess access).
data CloudflareAI

-- | Native Anthropic API backend. Calls Anthropic API directly.
-- Supports 'ClaudeCode' annotation for spawning Claude Code sessions.
data NativeAnthropic

-- ════════════════════════════════════════════════════════════════════════════
-- CLAUDE CODE ANNOTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Model selection for Claude Code sessions.
data ModelChoice
  = Haiku   -- ^ Fast, cost-effective model
  | Sonnet  -- ^ Balanced performance/cost
  | Opus    -- ^ Most capable model

-- | Type-level aliases for promoted ModelChoice constructors.
type Haiku = 'Haiku
type Sonnet = 'Sonnet
type Opus = 'Opus

-- | Marks an LLM node as executed via Claude Code subprocess instead of API.
--
-- When present, the node's template is rendered and passed to @claude -p@
-- via zellij-cc, which spawns a Claude Code session and returns JSON output.
--
-- @
-- gWork :: mode :- G.LLMNode
--     :@ Input BeadInfo
--     :@ Template WorkTpl
--     :@ Schema WorkResult
--     :@ ClaudeCode 'Sonnet ('Just "/path/to/worktree")
-- @
--
-- Parameters:
--
-- * @model@ - Which Claude model to use (Haiku, Sonnet, Opus)
-- * @cwd@ - Working directory for file access ('Nothing' inherits from runner)
--
-- Note: Only valid with 'Backend NativeAnthropic'. Using with CloudflareAI
-- will produce a compile-time type error.
type ClaudeCode :: ModelChoice -> Maybe Symbol -> Type
data ClaudeCode model cwd


-- ════════════════════════════════════════════════════════════════════════════
-- CLAUDECODE SINGLETONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Demote type-level ModelChoice to runtime value.
--
-- This enables compile-time validated ClaudeCode handlers where the model
-- is derived from the type annotation rather than passed as a runtime argument.
class SingModelChoice (m :: ModelChoice) where
  singModelChoice :: ModelChoice

instance SingModelChoice 'Haiku where singModelChoice = Haiku
instance SingModelChoice 'Sonnet where singModelChoice = Sonnet
instance SingModelChoice 'Opus where singModelChoice = Opus

-- | Demote type-level Maybe Symbol to runtime Maybe FilePath.
--
-- Used to derive the working directory from the ClaudeCode annotation.
class KnownMaybeCwd (m :: Maybe Symbol) where
  knownMaybeCwd :: Maybe FilePath

instance KnownMaybeCwd 'Nothing where knownMaybeCwd = Nothing
instance KnownSymbol s => KnownMaybeCwd ('Just s) where
  knownMaybeCwd = Just (symbolVal (Proxy @s))


-- ════════════════════════════════════════════════════════════════════════════
-- SPECIAL GOTO TARGET
-- ════════════════════════════════════════════════════════════════════════════

-- | Special marker type used as a target for the Goto effect to exit the graph.
--
-- @
-- -- In a Logic node's effect stack:
-- UsesEffects '[State S, Goto "gNextNode" A, Goto Exit FinalResult]
-- @
--
-- This @Exit@ type is used as a Goto target. Record-based graphs use @G.Exit@
-- from "Tidepool.Graph.Generic" for their exit field definitions.
data Exit

-- | Self-loop marker for transitions back to the current node.
--
-- Used for retry/continuation patterns:
--
-- @
-- Goto Self UpdatedState
-- @
data Self

-- ════════════════════════════════════════════════════════════════════════════
-- HETEROGENEOUS LISTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Type-indexed heterogeneous list.
--
-- Used for spawn payloads and barrier awaits where each element can have
-- a different type. Enables recursive type class instances for dispatch.
--
-- @
-- -- A list containing Int, Text, and Bool:
-- myList :: HList '[Int, Text, Bool]
-- myList = 42 ::: "hello" ::: True ::: HNil
--
-- -- Pattern matching:
-- processFirst :: HList '[a, b, c] -> a
-- processFirst (x ::: _ ::: _ ::: HNil) = x
-- @
--
-- For ForkNode spawn payloads:
--
-- @
-- SpawnPayloads '[To "w1" TaskA, To "w2" TaskB]
--   = HList '[TaskA, TaskB]
-- @
--
-- For BarrierNode awaits:
--
-- @
-- AwaitsHList '[ResultA, ResultB]
--   = HList '[ResultA, ResultB]
-- @
data HList (ts :: [Type]) where
  HNil  :: HList '[]
  (:::) :: t -> HList ts -> HList (t ': ts)

infixr 5 :::
