{-# LANGUAGE OverloadedStrings #-}

-- | Worker agent lifecycle phases as a simple sum type with StateMachine instance.
module WorkerPhase
  ( WorkerPhase (..),
    WorkerEvent (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.StateMachine (StateMachine (..), StopCheckResult (..), TransitionResult (..))

-- | Worker agent lifecycle phases.
data WorkerPhase
  = WorkerSpawned
  | WorkerRunning
  | WorkerDone
  | WorkerFailed Text
  deriving (Show, Eq)

-- | Worker lifecycle events.
data WorkerEvent
  = WorkerStarted
  | WorkerCompleted Text
  | WorkerErrored Text
  deriving (Show, Eq)

instance StateMachine WorkerPhase WorkerEvent where
  machineName = "worker"

  transition _phase event = case event of
    WorkerStarted -> Transitioned WorkerRunning
    WorkerCompleted _ -> Transitioned WorkerDone
    WorkerErrored reason -> Transitioned (WorkerFailed reason)

  canExit _ = Clean

instance ToJSON WorkerPhase where
  toJSON WorkerSpawned = object ["phase" .= ("worker_spawned" :: Text)]
  toJSON WorkerRunning = object ["phase" .= ("worker_working" :: Text)]
  toJSON WorkerDone = object ["phase" .= ("worker_done" :: Text)]
  toJSON (WorkerFailed msg) = object ["phase" .= ("worker_failed" :: Text), "message" .= msg]

instance FromJSON WorkerPhase where
  parseJSON = withObject "WorkerPhase" $ \v -> do
    phase <- v .: "phase"
    case (phase :: Text) of
      "worker_spawned" -> pure WorkerSpawned
      "worker_working" -> pure WorkerRunning
      "worker_done" -> pure WorkerDone
      "worker_failed" -> do
        msg <- v .: "message"
        pure (WorkerFailed msg)
      other -> fail $ "Unknown WorkerPhase: " <> T.unpack other
