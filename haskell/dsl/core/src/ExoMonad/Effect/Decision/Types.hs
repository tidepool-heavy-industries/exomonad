{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ExoMonad.Effect.Decision.Types
  ( -- * Decision Type
    Decision (..),

    -- * Context and Tracing
    DecisionContext (..),
    DecisionTrace (..),
  )
where

import Data.Aeson (ToJSON)
import ExoMonad.Effect.Log (LogFields)

-- | High-level decision outcomes for agent flows.
--
-- Used by the decision effect/TUI wrapper to represent standard
-- choices available to users or supervisors during agent execution.
data Decision
  = -- | Provide textual guidance or instructions.
    ProvideGuidance Text
  | -- | Abort the current operation or flow.
    Abort
  | -- | Continue with the current plan/flow.
    Continue
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Context provided to a decision request.
--
-- This context is used to render the decision UI.
data DecisionContext = DecisionContext
  { -- | Main prompt or question for the decision.
    dcPrompt :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Audit trace for a decision.
--
-- Captures the context, the options shown, the decision made, latency, and timestamp.
data DecisionTrace = DecisionTrace
  { -- | Context at the time of decision.
    dtContext :: DecisionContext,
    -- | The options shown to the user.
    dtOptionsPresented :: [Text],
    -- | The actual decision made.
    dtDecision :: Decision,
    -- | How long it took the user to decide (in milliseconds).
    dtLatencyMs :: Int,
    -- | When the decision occurred.
    dtTimestamp :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
