-- | The Inter-Gas Town Routing Graph.
--
-- This module defines the RoutingGraph - a type-safe state machine for
-- routing requests between Gas Town instances. The graph:
--
-- 1. Receives a RoutingRequest at Entry
-- 2. Classifies it using an LLM (RequestCategory)
-- 3. Deduplicates against recent requests
-- 4. Routes to target rig(s) or escalates to Mayor
-- 5. Aggregates responses
-- 6. Returns RoutingResponse at Exit
--
-- See docs/inter-gastown-routing-mcp-design.md for architecture details.
module Tidepool.MCP.Routing.Graph
  ( -- * The Routing Graph
    RoutingGraph(..)

    -- * Template Types (stubs for Phase 1)
  , ClassifyTpl
  , ClassifyContext(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Graph.Types
  ( type (:@)
  , Needs
  , Schema
  , Template
  , UsesEffects
  , Memory
  , Exit
  )
import Tidepool.Graph.Generic (GraphMode(..))
import qualified Tidepool.Graph.Generic as G
  ( Entry
  , Exit
  , LLMNode
  , LogicNode
  )
import Tidepool.Graph.Goto (Goto)

import Tidepool.MCP.Routing.Types
  ( RoutingRequest
  , RequestCategory
  , RoutingResponse
  , RoutingState
  , DispatchResult
  )

-- ============================================================================
-- Template Types (stubs for Phase 1)
-- ============================================================================

-- | Phantom type for the classification template.
--
-- In Phase 2, this will be connected to a real Jinja template that
-- prompts the LLM to classify incoming requests.
data ClassifyTpl

-- | Context for the classification template.
--
-- This is rendered into the Jinja template and sent to the LLM.
-- The LLM returns a RequestCategory based on this context.
data ClassifyContext = ClassifyContext
  { ccSource  :: Text
    -- ^ Where the request came from
  , ccIntent  :: Text
    -- ^ The request's stated intent
  , ccSummary :: Text
    -- ^ Summary of the payload for classification
  }
  deriving stock (Show, Eq, Generic)

-- ============================================================================
-- The Routing Graph
-- ============================================================================

-- | Inter-Gas Town Routing Graph.
--
-- This graph routes requests between Gas Town instances. It uses:
--
-- * An LLM node for classification (determining request type)
-- * Logic nodes for routing decisions and dispatch
-- * Memory for deduplication state and rig health tracking
--
-- The graph structure is:
--
-- @
-- Entry(RoutingRequest)
--   │
--   ▼
-- classify (LLM) ──► RequestCategory
--   │
--   ▼
-- dedupe (Logic) ──► Checks request cache, exits early if duplicate
--   │
--   ▼
-- route (Logic) ──► Determines target rig(s)
--   │
--   ├──► dispatchSingle ──► Single rig dispatch
--   ├──► dispatchMulti ──► Multi-rig dispatch
--   └──► escalate ──► Mayor escalation
--         │
--         ▼
-- aggregate (Logic) ◄─── Collects responses
--   │
--   ▼
-- Exit(RoutingResponse)
-- @
data RoutingGraph mode = RoutingGraph
  { -- | Entry: receives routing request from MCP client.
    rgEntry :: mode :- G.Entry RoutingRequest

    -- | Classify: LLM determines request category.
    --
    -- Uses the classification template to ask the LLM what type of
    -- request this is (WorkDispatch, StatusQuery, etc.)
  , rgClassify :: mode :- G.LLMNode
      :@ Needs '[RoutingRequest]
      :@ Template ClassifyTpl
      :@ Schema RequestCategory

    -- | Dedupe: Check if request is a duplicate.
    --
    -- Looks up the correlation ID in recent requests. If found,
    -- exits early with Deduplicated status. Otherwise continues
    -- to routing.
  , rgDedupe :: mode :- G.LogicNode
      :@ Needs '[RequestCategory, RoutingRequest]
      :@ Memory RoutingState
      :@ UsesEffects '[ Goto "rgRoute" RoutingRequest
                      , Goto Exit RoutingResponse
                      ]

    -- | Route: Determine target Gas Town instance(s).
    --
    -- Based on request category and rig health, decides whether to:
    -- - Dispatch to a single rig
    -- - Dispatch to multiple rigs
    -- - Escalate to Mayor
  , rgRoute :: mode :- G.LogicNode
      :@ Needs '[RoutingRequest]
      :@ Memory RoutingState
      :@ UsesEffects '[ Goto "rgDispatchSingle" (RoutingRequest, Text)
                      , Goto "rgDispatchMulti" (RoutingRequest, [Text])
                      , Goto "rgEscalate" RoutingRequest
                      ]

    -- | DispatchSingle: Send request to a single rig.
    --
    -- Calls the target rig and passes the result to aggregate.
  , rgDispatchSingle :: mode :- G.LogicNode
      :@ Needs '[(RoutingRequest, Text)]
      :@ UsesEffects '[ Goto "rgAggregate" (Text, DispatchResult)
                      ]

    -- | DispatchMulti: Send request to multiple rigs.
    --
    -- Calls each target rig (potentially in parallel) and passes
    -- all results to aggregate.
  , rgDispatchMulti :: mode :- G.LogicNode
      :@ Needs '[(RoutingRequest, [Text])]
      :@ UsesEffects '[ Goto "rgAggregate" [(Text, DispatchResult)]
                      ]

    -- | Escalate: Send request to Mayor for cross-rig coordination.
    --
    -- The Mayor handles requests that span multiple rigs or require
    -- global coordination. Exits directly with the escalation result.
  , rgEscalate :: mode :- G.LogicNode
      :@ Needs '[RoutingRequest]
      :@ UsesEffects '[ Goto Exit RoutingResponse
                      ]

    -- | Aggregate: Collect and combine dispatch results.
    --
    -- Takes results from single or multi dispatch and produces
    -- the final RoutingResponse.
    --
    -- Note: This node accepts two different input shapes via a union type.
    -- For Phase 1, we use a simple approach with two separate aggregators.
  , rgAggregate :: mode :- G.LogicNode
      :@ Needs '[(Text, DispatchResult)]
      :@ UsesEffects '[ Goto Exit RoutingResponse
                      ]

    -- | AggregateMulti: Aggregate results from multi-rig dispatch.
  , rgAggregateMulti :: mode :- G.LogicNode
      :@ Needs '[[(Text, DispatchResult)]]
      :@ UsesEffects '[ Goto Exit RoutingResponse
                      ]

    -- | Exit: return routing response to MCP client.
  , rgExit :: mode :- G.Exit RoutingResponse
  }
  deriving Generic
