-- | Domain types for the Inter-Gas Town Routing MCP.
--
-- This module defines the core data types for routing requests between
-- Gas Town instances. These types flow through the RoutingGraph and are
-- serialized for MCP protocol transport.
--
-- See docs/inter-gastown-routing-mcp-design.md for architecture details.
module Tidepool.MCP.Routing.Types
  ( -- * Request Types
    RoutingRequest(..)
  , RequestCategory(..)

    -- * Routing Decision
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
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- ============================================================================
-- Request Types
-- ============================================================================

-- | Incoming routing request from an MCP client or Gas Town instance.
--
-- This is the entry point to the routing graph. Each request has a
-- correlation ID for deduplication and tracing across the distributed system.
data RoutingRequest = RoutingRequest
  { reqSource      :: Text
    -- ^ Which Gas Town instance sent this request
  , reqIntent      :: Text
    -- ^ What the request wants to accomplish (human-readable description)
  , reqPayload     :: Value
    -- ^ The actual work payload (bead, mail, etc.) as JSON
  , reqPriority    :: Int
    -- ^ Priority level (0 = highest, higher = lower priority)
  , reqCorrelation :: Text
    -- ^ Unique ID for deduplication and distributed tracing
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Classification of request type.
--
-- The classify node uses an LLM to categorize incoming requests into
-- one of these categories, which determines the routing strategy.
data RequestCategory
  = WorkDispatch
    -- ^ Route work to a polecat for execution
  | StatusQuery
    -- ^ Query rig/polecat status (read-only)
  | EscalationReq
    -- ^ Cross-rig escalation requiring Mayor coordination
  | BeadSync
    -- ^ Beads synchronization between rigs
  | MailDelivery
    -- ^ Inter-rig mail delivery
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Routing Decision
-- ============================================================================

-- | The routing decision made by the route node.
--
-- Determines which Gas Town instance(s) should receive the request
-- and what strategy to use for delivery.
data RoutingDecision = RoutingDecision
  { targetRigs :: [Text]
    -- ^ Which Gas Town instances should receive this request
  , strategy   :: RouteStrategy
    -- ^ How to deliver the request
  , reason     :: Text
    -- ^ Human-readable explanation for observability
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Strategy for routing a request to Gas Town instances.
data RouteStrategy
  = Unicast
    -- ^ Send to exactly one rig
  | Multicast
    -- ^ Send to multiple specific rigs
  | Broadcast
    -- ^ Send to all available rigs
  | Escalate
    -- ^ Escalate to Mayor for cross-rig coordination
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Response Types
-- ============================================================================

-- | Final response from the routing graph.
--
-- This is returned to the MCP client after routing is complete.
data RoutingResponse = RoutingResponse
  { respStatus  :: ResponseStatus
    -- ^ What happened to the request
  , respPayload :: Maybe Value
    -- ^ Optional response payload from target rig(s)
  , respTargets :: [Text]
    -- ^ Which rigs received the request
  , respTraceId :: Text
    -- ^ Correlation ID for observability
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Status of a routing response.
data ResponseStatus
  = Routed
    -- ^ Successfully routed to target rig(s)
  | Deduplicated
    -- ^ Request was identified as a duplicate and filtered
  | Escalated
    -- ^ Request was escalated to Mayor for coordination
  | Failed Text
    -- ^ Routing failed with an error message
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- State Types
-- ============================================================================

-- | Persistent state for the routing graph.
--
-- Tracks recent requests (for deduplication), rig health status,
-- and routing metrics for observability.
data RoutingState = RoutingState
  { recentRequests :: Map Text UTCTime
    -- ^ Recent request correlations -> timestamp (for deduplication window)
  , rigStatus      :: Map Text RigHealth
    -- ^ Per-rig health status
  , routingMetrics :: RoutingMetrics
    -- ^ Aggregate metrics for observability
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Health status of a Gas Town rig.
data RigHealth = RigHealth
  { healthStatus  :: HealthStatus
    -- ^ Current health state
  , lastSeen      :: UTCTime
    -- ^ When we last heard from this rig
  , pendingWork   :: Int
    -- ^ Number of items in the rig's work queue
  , polecatCount  :: Int
    -- ^ Number of active polecats in this rig
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Health status categories.
data HealthStatus
  = Healthy
    -- ^ Rig is operating normally
  | Degraded
    -- ^ Rig is experiencing issues but still accepting work
  | Unhealthy
    -- ^ Rig is not accepting new work
  | Unknown
    -- ^ Haven't heard from rig recently
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Aggregate routing metrics for observability.
--
-- These are used by sleeptime formulas to evolve routing behavior.
data RoutingMetrics = RoutingMetrics
  { totalRequests       :: Int
    -- ^ Total requests processed
  , routedRequests      :: Int
    -- ^ Requests successfully routed
  , deduplicatedCount   :: Int
    -- ^ Requests filtered as duplicates
  , escalatedCount      :: Int
    -- ^ Requests escalated to Mayor
  , failedCount         :: Int
    -- ^ Requests that failed routing
  , avgLatencyMs        :: Double
    -- ^ Average routing latency in milliseconds
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Dispatch Types
-- ============================================================================

-- | Result of dispatching a request to a rig.
data DispatchResult
  = DispatchSuccess Value
    -- ^ Request was accepted, here's the response
  | DispatchQueued Text
    -- ^ Request was queued for async processing, here's the work ID
  | DispatchFailed Text
    -- ^ Dispatch failed with error message
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Result of escalating to the Mayor.
data EscalationResult
  = EscalationAccepted Text
    -- ^ Mayor accepted, here's the tracking ID
  | EscalationDeferred Text
    -- ^ Mayor deferred (busy), here's the reason
  | EscalationRejected Text
    -- ^ Mayor rejected the escalation, here's why
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
