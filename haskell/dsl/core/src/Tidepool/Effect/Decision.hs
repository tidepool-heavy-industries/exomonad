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
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
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
  let beadLabels = map (\b ->
        let icon = case b.bsPriority of
              0 -> "🔴"
              1 -> "🟡"
              2 -> "🔵"
              _ -> "⚪"
        in icon <> " [" <> b.bsId <> "] " <> b.bsTitle) ctx.dcReadyBeads

  let ui = PopupDefinition
        { pdTitle = "Decision Required"
        , pdComponents =
            [ mkText "prompt" ctx.dcPrompt Nothing
            ] ++
            (if null ctx.dcReadyBeads then []
             else [ mkChoice "bead_selection" "Select a bead" beadLabels Nothing Nothing ]) ++
            [ mkTextbox "guidance" "Or provide guidance" (Just "Enter custom guidance...") Nothing Nothing
            ]
        }

  let options = beadLabels ++ [ "Continue", "Abort", "Provide Guidance" ]

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
        _ -> do
          -- Check if a bead was selected
          case lookupValue "bead_selection" result.prValues of
            Just (A.String selectedLabel) -> do
              -- Extract bead ID from the label (format: "icon [ID] Title")
              case T.splitOn "] " selectedLabel of
                (prefix:_) -> case T.stripPrefix "[" (T.takeWhileEnd (/= '[') prefix) of
                  Just beadId -> pure $ SelectBead beadId
                  Nothing -> pure Continue
                _ -> pure Continue
            _ -> pure Continue
    "decline" -> pure Abort
    _ -> requestDecision ctx

  -- Log decision trace
  let latencyMs = round $ realToFrac (diffUTCTime end start) * (1000 :: Double)
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
