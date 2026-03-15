-- | Worker agent lifecycle transitions with GADT-enforced phase safety.
module ExoMonad.Guest.Lifecycle.WorkerTransitions
  ( -- * Events
    WorkerEvent (..),

    -- * Existential-level transitions
    applyWorkerEvent,

    -- * Stop hook
    canExit,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Effects.Log qualified as Log
import ExoMonad.Effects.Log (LogInfo)
import ExoMonad.Guest.Lifecycle.PhaseEffect (StopCheckResult (..), getWorkerPhase, setWorkerPhase)
import ExoMonad.Guest.Lifecycle.WorkerState
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect_)
import ExoMonad.Guest.Types (Effects)

-- | Worker lifecycle events.
data WorkerEvent
  = WorkerStarted
  | WorkerCompleted Text
  | WorkerErrored Text

-- ============================================================================
-- Existential-Level Transitions
-- ============================================================================

applyWorkerEvent :: WorkerEvent -> Eff Effects ()
applyWorkerEvent event = do
  mState <- getWorkerPhase
  let current = case mState of
        Just s -> s
        Nothing -> SomeWorkerState SWorkerSpawned
  newState <- transitionWorker current event
  setWorkerPhase newState

transitionWorker :: SomeWorkerState -> WorkerEvent -> Eff Effects SomeWorkerState
transitionWorker (SomeWorkerState old) event = case event of
  WorkerStarted -> do
    logTransition (showPhase old) "Running"
    pure (SomeWorkerState SWorkerRunning)
  WorkerCompleted msg -> do
    logTransitionWithMsg msg (showPhase old) "Done"
    pure (SomeWorkerState SWorkerDone)
  WorkerErrored reason -> do
    logTransitionWithMsg reason (showPhase old) "Failed"
    pure (SomeWorkerState (SWorkerFailed reason))

-- ============================================================================
-- Stop Hook
-- ============================================================================

canExit :: SomeWorkerState -> StopCheckResult
canExit (SomeWorkerState SWorkerSpawned) = Clean
canExit (SomeWorkerState SWorkerRunning) = Clean
canExit (SomeWorkerState SWorkerDone) = Clean
canExit (SomeWorkerState (SWorkerFailed _)) = Clean

-- ============================================================================
-- Helpers
-- ============================================================================

showPhase :: WorkerState p -> Text
showPhase SWorkerSpawned = "Spawned"
showPhase SWorkerRunning = "Running"
showPhase SWorkerDone = "Done"
showPhase (SWorkerFailed _) = "Failed"

logTransition :: Text -> Text -> Eff Effects ()
logTransition old new =
  void $
    suspendEffect_ @LogInfo $
      Log.InfoRequest
        { Log.infoRequestMessage = TL.fromStrict $ "[WorkerTransition] " <> old <> " -> " <> new,
          Log.infoRequestFields = ""
        }

logTransitionWithMsg :: Text -> Text -> Text -> Eff Effects ()
logTransitionWithMsg msg old new =
  void $
    suspendEffect_ @LogInfo $
      Log.InfoRequest
        { Log.infoRequestMessage = TL.fromStrict $ "[WorkerTransition] " <> old <> " -> " <> new <> " (" <> msg <> ")",
          Log.infoRequestFields = ""
        }
