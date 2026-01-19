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

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (ToJSON(..), FromJSON, toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Tidepool.Effect.Decision.Types
import Tidepool.Effect.TUI
import Tidepool.Effect.Types (Time, Log, getCurrentTime, logInfoWith)

-- | Request a decision from the user via the TUI.
--
-- This builds a UISpec from the context, waits for an interaction,
-- and logs the resulting decision via the Log effect.
requestDecision :: (Member TUI r, Member Time r, Member Log r) => DecisionContext -> Eff r Decision
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
  now <- getCurrentTime
  let trace = DecisionTrace ctx decision now
  logInfoWith "Decision made" [("trace", toJSON trace)]

  pure decision
