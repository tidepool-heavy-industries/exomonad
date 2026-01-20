{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Tidepool.Effect.Decision.Types
  ( -- * Decision Type
    Decision(..)
    -- * Context and Tracing
  , DecisionContext(..)
  , BeadSummary(..)
  , DecisionTrace(..)
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | High-level decision outcomes for agent flows.
--
-- Used by the decision effect/TUI wrapper to represent standard
-- choices available to users or supervisors during agent execution.
data Decision
  = SelectBead Text
    -- ^ Select a specific bead ID to focus on.
  | ProvideGuidance Text
    -- ^ Provide textual guidance or instructions.
  | Abort
    -- ^ Abort the current operation or flow.
  | Continue
    -- ^ Continue with the current plan/flow.
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Summary of a bead for decision context.
data BeadSummary = BeadSummary
  { bsId :: Text
  , bsTitle :: Text
  , bsPriority :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Context provided to a decision request.
--
-- This context is used to render the decision UI.
data DecisionContext = DecisionContext
  { dcPrompt :: Text
    -- ^ Main prompt or question for the decision.
  , dcReadyBeads :: [BeadSummary]
    -- ^ List of beads that are ready for selection.
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Audit trace for a decision.
--
-- Captures the context, the options shown, the decision made, latency, and timestamp.
data DecisionTrace = DecisionTrace
  { dtContext          :: DecisionContext
    -- ^ Context at the time of decision.
  , dtOptionsPresented :: [Text]
    -- ^ The options shown to the user.
  , dtDecision         :: Decision
    -- ^ The actual decision made.
  , dtLatencyMs        :: Int
    -- ^ How long it took the user to decide (in milliseconds).
  , dtTimestamp        :: UTCTime
    -- ^ When the decision occurred.
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
