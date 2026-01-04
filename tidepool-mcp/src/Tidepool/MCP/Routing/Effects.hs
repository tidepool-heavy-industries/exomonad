-- | Effects for the Inter-Gas Town Routing MCP.
--
-- These effects are used by routing graph handlers to interact with
-- Gas Town instances and the Mayor. In production, they are interpreted
-- by the TypeScript harness via the WASM yield/resume protocol.
module Tidepool.MCP.Routing.Effects
  ( -- * RigCall Effect
    RigCall(..)
  , callRig
  , getRigHealth
  , listRigs

    -- * MayorCall Effect
  , MayorCall(..)
  , escalateToMayor
  , getRoutingHints
  ) where

import Control.Monad.Freer (Eff, Member, send)
import Data.Text (Text)

import Tidepool.MCP.Routing.Types
  ( RoutingRequest
  , DispatchResult
  , RigHealth
  , EscalationResult
  )

-- ============================================================================
-- RigCall Effect
-- ============================================================================

-- | Effect for calling a Gas Town rig.
--
-- This effect enables routing handlers to:
--
-- * Dispatch work to a rig and await the response
-- * Query rig health status for load balancing
-- * List available rigs in the network
--
-- In WASM deployment, this effect yields to the TypeScript harness
-- which handles the actual HTTP/WebSocket communication with rigs.
data RigCall r where
  -- | Send a request to a specific rig and await the response.
  --
  -- The rig may accept immediately, queue for async processing,
  -- or reject the request.
  CallRig :: Text -> RoutingRequest -> RigCall DispatchResult

  -- | Get the current health status of a rig.
  --
  -- Used for load balancing and routing decisions.
  GetRigHealth :: Text -> RigCall RigHealth

  -- | List all known rigs in the network.
  --
  -- Returns rig names that can be used with 'callRig' and 'getRigHealth'.
  ListRigs :: RigCall [Text]

-- | Send a request to a specific rig.
--
-- @
-- result <- callRig "tidepool" request
-- case result of
--   DispatchSuccess payload -> handleSuccess payload
--   DispatchQueued workId   -> trackAsync workId
--   DispatchFailed reason   -> handleFailure reason
-- @
callRig :: Member RigCall effs => Text -> RoutingRequest -> Eff effs DispatchResult
callRig rigName request = send (CallRig rigName request)

-- | Get health status of a rig.
--
-- @
-- health <- getRigHealth "anemone"
-- when (health.healthStatus == Unhealthy) $
--   routeToAlternative
-- @
getRigHealth :: Member RigCall effs => Text -> Eff effs RigHealth
getRigHealth rigName = send (GetRigHealth rigName)

-- | List all available rigs.
--
-- @
-- rigs <- listRigs
-- healthyRigs <- filterM (fmap isHealthy . getRigHealth) rigs
-- @
listRigs :: Member RigCall effs => Eff effs [Text]
listRigs = send ListRigs

-- ============================================================================
-- MayorCall Effect
-- ============================================================================

-- | Effect for escalating to the Mayor.
--
-- The Mayor handles cross-rig coordination that individual rigs
-- can't handle themselves, such as:
--
-- * Work that spans multiple rigs
-- * Conflict resolution between rigs
-- * Priority decisions requiring global context
--
-- In WASM deployment, this effect yields to the TypeScript harness
-- which forwards the escalation to the Mayor instance.
data MayorCall r where
  -- | Escalate a request to the Mayor.
  --
  -- The Mayor may accept, defer (if busy), or reject the escalation.
  EscalateToMayor :: RoutingRequest -> Text -> MayorCall EscalationResult

  -- | Ask the Mayor for routing hints.
  --
  -- The Mayor may suggest target rigs based on global knowledge
  -- that the routing server doesn't have.
  GetRoutingHints :: RoutingRequest -> MayorCall [Text]

-- | Escalate a request to the Mayor with a reason.
--
-- @
-- result <- escalateToMayor request "Cross-rig dependency detected"
-- case result of
--   EscalationAccepted trackingId -> logEscalation trackingId
--   EscalationDeferred reason     -> retryLater reason
--   EscalationRejected reason     -> handleRejection reason
-- @
escalateToMayor :: Member MayorCall effs => RoutingRequest -> Text -> Eff effs EscalationResult
escalateToMayor request reason = send (EscalateToMayor request reason)

-- | Get routing hints from the Mayor.
--
-- The Mayor has global visibility and may suggest rigs that the
-- routing server wouldn't know about (e.g., newly spawned rigs,
-- rigs with special capabilities).
--
-- @
-- hints <- getRoutingHints request
-- case hints of
--   []   -> useDefaultRouting
--   rigs -> routeToSuggested rigs
-- @
getRoutingHints :: Member MayorCall effs => RoutingRequest -> Eff effs [Text]
getRoutingHints request = send (GetRoutingHints request)
