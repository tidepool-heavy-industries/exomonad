{-# LANGUAGE StandaloneDeriving #-}

-- | GADT-indexed worker agent lifecycle phases.
module ExoMonad.Guest.Lifecycle.WorkerState
  ( WorkerPhase (..),
    WorkerState (..),
    SomeWorkerState (..),
    describeWorkerPhase,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Text (Text)
import Data.Text qualified as T

-- | Worker agent lifecycle phases (promoted to kinds via DataKinds).
data WorkerPhase
  = WorkerSpawned
  | WorkerRunning
  | WorkerDone
  | WorkerFailed

-- | GADT-indexed worker state.
data WorkerState (p :: WorkerPhase) where
  SWorkerSpawned :: WorkerState 'WorkerSpawned
  SWorkerRunning :: WorkerState 'WorkerRunning
  SWorkerDone :: WorkerState 'WorkerDone
  SWorkerFailed :: Text -> WorkerState 'WorkerFailed

deriving instance Show (WorkerState p)

-- | Existential wrapper for runtime phase tracking.
data SomeWorkerState where
  SomeWorkerState :: WorkerState p -> SomeWorkerState

deriving instance Show SomeWorkerState

-- | Human-readable phase description.
describeWorkerPhase :: SomeWorkerState -> Text
describeWorkerPhase (SomeWorkerState SWorkerSpawned) = "spawned"
describeWorkerPhase (SomeWorkerState SWorkerRunning) = "running"
describeWorkerPhase (SomeWorkerState SWorkerDone) = "done"
describeWorkerPhase (SomeWorkerState (SWorkerFailed msg)) = "failed: " <> msg

instance ToJSON SomeWorkerState where
  toJSON (SomeWorkerState SWorkerSpawned) = object ["phase" .= ("worker_spawned" :: Text)]
  -- "worker_working" (not "worker_running") for backward compat with persisted KV data
  toJSON (SomeWorkerState SWorkerRunning) = object ["phase" .= ("worker_working" :: Text)]
  toJSON (SomeWorkerState SWorkerDone) = object ["phase" .= ("worker_done" :: Text)]
  toJSON (SomeWorkerState (SWorkerFailed msg)) = object ["phase" .= ("worker_failed" :: Text), "message" .= msg]

instance FromJSON SomeWorkerState where
  parseJSON = withObject "SomeWorkerState" $ \v -> do
    phase <- v .: "phase"
    case (phase :: Text) of
      "worker_spawned" -> pure (SomeWorkerState SWorkerSpawned)
      "worker_working" -> pure (SomeWorkerState SWorkerRunning)
      "worker_done" -> pure (SomeWorkerState SWorkerDone)
      "worker_failed" -> do
        msg <- v .: "message"
        pure (SomeWorkerState (SWorkerFailed msg))
      other -> fail $ "Unknown WorkerPhase: " <> T.unpack other
