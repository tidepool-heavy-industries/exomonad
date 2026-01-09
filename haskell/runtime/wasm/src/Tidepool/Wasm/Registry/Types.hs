-- | Shared types for the graph registry.
module Tidepool.Wasm.Registry.Types
  ( -- * Types
    ActiveSession(..)
  , GraphEntry(..)
  ) where

import Data.Aeson (Value)
import Data.Text (Text)

import Tidepool.Wasm.WireTypes
  ( EffectResult
  , GraphState
  , StepOutput
  )


-- | Active session state - stored after initialize, used by step.
--
-- The 'ActiveSession' pattern (inspired by Rich Hickey critique):
-- Registry lookup happens once on 'initialize'. After that, 'step' uses
-- the stored closures directly - no Map lookup per step.
data ActiveSession = ActiveSession
  { asGraphId :: !Text
    -- ^ Graph ID for validation (catches routing bugs)
  , asStep :: EffectResult -> IO StepOutput
    -- ^ Step function, closed over graph's internal state
  , asGetState :: IO GraphState
    -- ^ Get current graph state for observability
  , asGraphInfo :: Value
    -- ^ Static graph info (cached, never changes)
  }


-- | Entry in the graph registry - how to create sessions for a graph.
data GraphEntry = GraphEntry
  { geName :: !Text
    -- ^ Human-readable name (e.g., "HabiticaRoutingGraph")
  , geGraphInfo :: Value
    -- ^ Static graph metadata (nodes, edges)
  , geCreate :: Value -> IO (Either Text (StepOutput, ActiveSession))
    -- ^ Create a new session from JSON input.
    -- Returns initial StepOutput and the session for subsequent steps.
  }
