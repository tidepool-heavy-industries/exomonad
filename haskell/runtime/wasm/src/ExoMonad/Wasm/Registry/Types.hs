-- | Shared types for the graph registry.
module ExoMonad.Wasm.Registry.Types
  ( -- * Types
    ActiveSession (..),
    GraphEntry (..),
  )
where

import Data.Aeson (Value)
import Data.Text (Text)
import ExoMonad.Wasm.WireTypes
  ( EffectResult,
    GraphState,
    StepOutput,
  )

-- | Active session state - stored after initialize, used by step.
--
-- The 'ActiveSession' pattern (inspired by Rich Hickey critique):
-- Registry lookup happens once on 'initialize'. After that, 'step' uses
-- the stored closures directly - no Map lookup per step.
data ActiveSession = ActiveSession
  { -- | Graph ID for validation (catches routing bugs)
    asGraphId :: !Text,
    -- | Step function, closed over graph's internal state
    asStep :: EffectResult -> IO StepOutput,
    -- | Get current graph state for observability
    asGetState :: IO GraphState,
    -- | Static graph info (cached, never changes)
    asGraphInfo :: Value
  }

-- | Entry in the graph registry - how to create sessions for a graph.
data GraphEntry = GraphEntry
  { -- | Human-readable name (e.g., "HabiticaRoutingGraph")
    geName :: !Text,
    -- | Static graph metadata (nodes, edges)
    geGraphInfo :: Value,
    -- | Create a new session from JSON input.
    -- Returns initial StepOutput and the session for subsequent steps.
    geCreate :: Value -> IO (Either Text (StepOutput, ActiveSession))
  }
