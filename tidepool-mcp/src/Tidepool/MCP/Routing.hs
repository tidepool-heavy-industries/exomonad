-- | Inter-Gas Town Routing MCP.
--
-- This module re-exports all types for the Inter-Gas Town Routing MCP server.
-- The routing graph classifies incoming requests, deduplicates, routes to
-- appropriate Gas Town instances, and handles escalation for cross-rig
-- coordination.
--
-- = Overview
--
-- Two Gas Town instances coordinate work via a tidepool-based MCP server.
-- The routing graph:
--
-- 1. Classifies requests using an LLM
-- 2. Deduplicates against recent requests
-- 3. Routes to target rig(s) based on category and health
-- 4. Aggregates responses
--
-- = Architecture
--
-- @
-- Entry(RoutingRequest)
--   │
--   ▼
-- classify (LLM) ──► RequestCategory
--   │
--   ▼
-- dedupe ──► route ──► dispatch ──► aggregate
--   │                    │
--   └──► Exit (dup)      └──► escalate ──► Exit
--                                           │
--                                           ▼
--                              Exit(RoutingResponse)
-- @
--
-- = Usage
--
-- @
-- import Tidepool.MCP.Routing
--
-- -- The graph type is used for validation and handler definition
-- type MyHandlers = RoutingGraph (AsHandler '[RigCall, MayorCall, Memory RoutingState])
-- @
--
-- See docs/inter-gastown-routing-mcp-design.md for full architecture details.
module Tidepool.MCP.Routing
  ( -- * The Routing Graph
    RoutingGraph(..)

    -- * Domain Types
  , RoutingRequest(..)
  , RequestCategory(..)
  , RoutingDecision(..)
  , RouteStrategy(..)

    -- * Response Types
  , RoutingResponse(..)
  , ResponseStatus(..)

    -- * State Types
  , RoutingState(..)
  , RigHealth(..)
  , HealthStatus(..)
  , RoutingMetrics(..)

    -- * Dispatch Types
  , DispatchResult(..)
  , EscalationResult(..)

    -- * Effects
  , RigCall(..)
  , callRig
  , getRigHealth
  , listRigs
  , MayorCall(..)
  , escalateToMayor
  , getRoutingHints

    -- * Template Types
  , ClassifyTpl
  , ClassifyContext(..)
  ) where

import Tidepool.MCP.Routing.Types
import Tidepool.MCP.Routing.Effects
import Tidepool.MCP.Routing.Graph
