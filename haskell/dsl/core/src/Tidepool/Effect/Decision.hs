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
  , BeadSummary(..)
  , DecisionTrace(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.Aeson (ToJSON(..), toJSON)
import qualified Data.Text as T
import Data.Time (diffUTCTime)
import Tidepool.Effect.Decision.Types
import Tidepool.Effect.TUI
import Tidepool.Effect.Types (Time, Log, DecisionLog, getCurrentTime, logInfoWith, recordDecision)

-- | Request a decision from the user via the TUI.
--
-- This builds a UISpec from the context, waits for an interaction,
-- and logs the resulting decision via the Log and DecisionLog effects.
requestDecision :: (Member TUI r, Member Time r, Member Log r, Member DecisionLog r) => DecisionContext -> Eff r Decision
requestDecision ctx = do
  let mkButton b =
        let icon = case b.bsPriority of
              0 -> "🔴"
              1 -> "🟡"
              2 -> "🔵"
              _ -> "⚪"
            label = icon <> " [" <> b.bsId <> "] " <> b.bsTitle
        in EButton ("bead:" <> b.bsId) label

  let ui = UISpec "decision-request" $ Vertical $
        [ EText "prompt" ctx.dcPrompt
        ] ++
        map mkButton ctx.dcReadyBeads ++
        [ EButton "continue" "Continue"
        , EButton "abort" "Abort"
        , EInput "guidance" "Provide Guidance (Enter to submit)" ""
        ]

  let options =
        map (\b -> let icon = case b.bsPriority of
                         0 -> "🔴"
                         1 -> "🟡"
                         2 -> "🔵"
                         _ -> "⚪"
                   in icon <> " [" <> b.bsId <> "] " <> b.bsTitle) ctx.dcReadyBeads ++
        [ "Continue", "Abort", "Provide Guidance (Enter to submit)" ]

  start <- getCurrentTime
  interaction <- showUI ui
  end <- getCurrentTime

  decision <- case interaction of
    ButtonClicked _ bid
      | "bead:" `T.isPrefixOf` bid -> pure $ SelectBead (T.drop 5 bid)
      | bid == "continue" -> pure Continue
      | bid == "abort" -> pure Abort
      | otherwise -> requestDecision ctx
    InputSubmitted _ "guidance" val -> pure $ ProvideGuidance val
    _ -> requestDecision ctx

  -- Log decision trace
  let latencyMs = round $ realToFrac (diffUTCTime end start) * 1000
  let trace = DecisionTrace
        { dtContext = ctx
        , dtOptionsPresented = options
        , dtDecision = decision
        , dtLatencyMs = latencyMs
        , dtTimestamp = end
        }

  logInfoWith "Decision made" [("trace", toJSON trace)]
  recordDecision trace

  pure decision
