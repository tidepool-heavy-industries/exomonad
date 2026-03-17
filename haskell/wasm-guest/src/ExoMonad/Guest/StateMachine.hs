{-# LANGUAGE OverloadedStrings #-}

-- | Generic state machine primitive for agent lifecycle phases.
--
-- Users define sum types + transitions via the 'StateMachine' typeclass.
-- Framework handles KV persistence, logging, and stop hook integration.
-- Each machine gets a scoped KV key via 'machineName' to prevent collisions.
module ExoMonad.Guest.StateMachine
  ( -- * Typeclass
    StateMachine (..),

    -- * Transition result
    TransitionResult (..),

    -- * Stop check result
    StopCheckResult (..),
    describeStopResult,

    -- * Framework functions
    getPhase,
    applyEvent,
    checkExit,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff, Member)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import Effects.Kv qualified as KV
import Effects.Log qualified as Log
import ExoMonad.Effects.KV (KVGet, KVSet)
import ExoMonad.Effects.Log (LogInfo, LogWarn)
import ExoMonad.Guest.Tool.Suspend.Types (SuspendYield)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect, suspendEffect_)
import ExoMonad.Guest.Types (Effects)

-- | Result of checking whether an agent can exit.
data StopCheckResult
  = MustBlock Text
  | ShouldNudge Text
  | Clean

-- | Describe a stop check result for logging.
describeStopResult :: StopCheckResult -> Text
describeStopResult (MustBlock msg) = "block: " <> msg
describeStopResult (ShouldNudge msg) = "nudge: " <> msg
describeStopResult Clean = "clean"

-- | Result of a state transition.
data TransitionResult phase
  = Transitioned phase
  | InvalidTransition Text

-- | Typeclass for agent lifecycle state machines.
--
-- @phase@ is a sum type representing all possible lifecycle phases.
-- @event@ is a sum type representing all possible events/triggers.
--
-- 'transition' is pure — callers handle persistence and side effects.
-- 'machineName' scopes the KV key to prevent cross-role collisions.
class (ToJSON phase, FromJSON phase, Typeable phase, Show phase) => StateMachine phase event where
  transition :: phase -> event -> TransitionResult phase
  canExit :: phase -> StopCheckResult
  machineName :: Text

-- | KV key scoped by machine name.
phaseKey :: forall phase event. StateMachine phase event => Text
phaseKey = "phase-" <> (machineName @phase @event)

-- | Read the current phase from KV.
getPhase :: forall phase event. StateMachine phase event => Eff Effects (Maybe phase)
getPhase = do
  result <- suspendEffect @KVGet (KV.GetRequest {KV.getRequestKey = TL.fromStrict (phaseKey @phase @event)})
  case result of
    Left _ -> pure Nothing
    Right resp
      | not (KV.getResponseFound resp) -> pure Nothing
      | otherwise ->
          case Aeson.eitherDecodeStrict (encodeUtf8 (TL.toStrict (KV.getResponseValue resp))) of
            Left _ -> pure Nothing
            Right phase -> pure (Just phase)

-- | Set the current phase in KV.
setPhase :: forall phase event. StateMachine phase event => phase -> Eff Effects ()
setPhase phase = do
  let json = TL.toStrict (decodeUtf8 (Aeson.encode phase))
  _ <- suspendEffect @KVSet (KV.SetRequest {KV.setRequestKey = TL.fromStrict (phaseKey @phase @event), KV.setRequestValue = TL.fromStrict json})
  pure ()

-- | Apply an event to the current phase, persisting the result.
-- Reads current phase from KV, applies transition, writes new phase + logs.
applyEvent :: forall phase event. (StateMachine phase event, Show event) => phase -> event -> Eff Effects (Maybe phase)
applyEvent defaultPhase event = do
  mCurrent <- getPhase @phase @event
  let current = maybe defaultPhase id mCurrent
  case transition current event of
    Transitioned newPhase -> do
      setPhase @phase @event newPhase
      logTransition (machineName @phase @event) (T.pack (show current)) (T.pack (show newPhase))
      pure (Just newPhase)
    InvalidTransition reason -> do
      logWarning (machineName @phase @event) reason
      pure Nothing

-- | Check whether the agent can exit based on its current phase.
checkExit :: forall phase event. StateMachine phase event => phase -> Eff Effects StopCheckResult
checkExit defaultPhase = do
  mPhase <- getPhase @phase @event
  let current = maybe defaultPhase id mPhase
  pure (canExit current)

-- | Log a state transition.
logTransition :: Text -> Text -> Text -> Eff Effects ()
logTransition name old new =
  void $
    suspendEffect_ @LogInfo $
      Log.InfoRequest
        { Log.infoRequestMessage = TL.fromStrict $ "[" <> name <> "] " <> old <> " -> " <> new,
          Log.infoRequestFields = ""
        }

-- | Log a warning for an invalid transition.
logWarning :: Text -> Text -> Eff Effects ()
logWarning name reason =
  void $
    suspendEffect_ @LogWarn $
      Log.WarnRequest
        { Log.warnRequestMessage = TL.fromStrict $ "[" <> name <> "] Invalid transition: " <> reason,
          Log.warnRequestFields = ""
        }
