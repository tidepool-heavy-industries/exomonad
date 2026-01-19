{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Decision types for high-level agent/user interactions.
module Tidepool.Effect.Decision
  ( -- * Decision Type
    Decision(..)
  , requestDecision
    -- * Context and Tracing
  , DecisionContext(..)
  , DecisionTrace(..)
  ) where

import Control.Monad.Freer (Eff, Member, LastMember, sendM)
import Data.Aeson (ToJSON, FromJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Tidepool.Effect.TUI

-- | Request a decision from the user via the TUI.
--
-- This builds a UISpec from the context, waits for an interaction,
-- and logs the resulting decision to @.tidepool/decision_log.jsonl@.
requestDecision :: (Member TUI r, LastMember IO r) => DecisionContext -> Eff r Decision
requestDecision ctx = do
  let ui = UISpec "decision-request" $ Vertical $
        [ EText "prompt" ctx.dcPrompt
        ] ++
        [ EButton ("bead:" <> b) ("Select Bead: " <> b) | b <- ctx.dcReadyBeads ] ++
        [ EButton "continue" "Continue"
        , EButton "abort" "Abort"
        , EInput "guidance" "Provide Guidance (Enter to submit)" ""
        ]

  interaction <- showUI ui

  decision <- case interaction of
    ButtonClicked _ bid
      | "bead:" `T.isPrefixOf` bid -> pure $ SelectBead (T.drop 5 bid)
      | bid == "continue" -> pure Continue
      | bid == "abort" -> pure Abort
      | otherwise -> requestDecision ctx
    InputSubmitted _ "guidance" val -> pure $ ProvideGuidance val
    _ -> requestDecision ctx

  -- Log decision trace
  now <- sendM getCurrentTime
  let trace = DecisionTrace ctx decision now
  sendM $ logDecisionTrace trace

  pure decision

-- | Log a decision trace to the local JSONL log file.
logDecisionTrace :: DecisionTrace -> IO ()
logDecisionTrace trace = do
  let path = ".tidepool/decision_log.jsonl"
  createDirectoryIfMissing True (takeDirectory path)
  LBS.appendFile path (encode trace <> "\n")

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

-- | Context provided to a decision request.
--
-- This context is used to render the decision UI.
data DecisionContext = DecisionContext
  { dcPrompt :: Text
    -- ^ Main prompt or question for the decision.
  , dcReadyBeads :: [Text]
    -- ^ List of bead IDs that are ready for selection.
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Audit trace for a decision.
--
-- Captures the context, the decision made, and the timestamp.
data DecisionTrace = DecisionTrace
  { dtContext   :: DecisionContext
    -- ^ Context at the time of decision.
  , dtDecision  :: Decision
    -- ^ The actual decision made.
  , dtTimestamp :: UTCTime
    -- ^ When the decision occurred.
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
