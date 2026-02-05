{-# LANGUAGE OverloadedStrings #-}

-- | Decision types for high-level agent/user interactions.
module ExoMonad.Effect.Decision
  ( -- * Decision Type
    Decision (..),
    requestDecision,

    -- * Context and Tracing
    DecisionContext (..),
    DecisionTrace (..),
  )
where

import Data.Aeson (ToJSON (..), toJSON)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import Data.Time (diffUTCTime)
import ExoMonad.Effect.Decision.Types
import ExoMonad.Effect.TUI
import ExoMonad.Effect.Types (DecisionLog, Log, Time, getCurrentTime, logInfoWith, recordDecision)
import Polysemy (Member, Sem)

-- | Request a decision from the user via the TUI.
--
-- This builds a UISpec from the context, waits for an interaction,
-- and logs the resulting decision via the Log and DecisionLog effects.
requestDecision :: (Member TUI r, Member Time r, Member Log r, Member DecisionLog r) => DecisionContext -> Sem r Decision
requestDecision ctx = do
  let ui =
        PopupDefinition
          { pdTitle = "Decision Required",
            pdComponents =
              [ mkText "prompt" ctx.dcPrompt Nothing,
                mkTextbox "guidance" "Or provide guidance" (Just "Enter custom guidance...") Nothing Nothing
              ]
          }

  let options = ["Continue", "Abort", "Provide Guidance"]

  start <- getCurrentTime
  result <- showUI ui
  end <- getCurrentTime

  let lookupValue :: T.Text -> A.Value -> Maybe A.Value
      lookupValue key (A.Object o) = KM.lookup (Key.fromText key) o
      lookupValue _ _ = Nothing

  decision <- case result.prButton of
    "submit" -> do
      -- Check if guidance was provided
      case lookupValue "guidance" result.prValues of
        Just (A.String val) | not (T.null val) -> pure $ ProvideGuidance val
        _ -> pure Continue
    "decline" -> pure Abort
    _ -> requestDecision ctx

  -- Log decision trace
  let latencyMs = round $ realToFrac (diffUTCTime end start) * (1000 :: Double)
  let trace =
        DecisionTrace
          { dtContext = ctx,
            dtOptionsPresented = options,
            dtDecision = decision,
            dtLatencyMs = latencyMs,
            dtTimestamp = end
          }

  logInfoWith "Decision made" [("trace", toJSON trace)]
  recordDecision trace

  pure decision
