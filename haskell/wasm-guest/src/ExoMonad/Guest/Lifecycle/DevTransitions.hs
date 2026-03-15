-- | Dev agent lifecycle transitions with GADT-enforced phase safety.
--
-- Typed transition functions guarantee valid phase changes at compile time.
-- 'applyDevEvent' provides an existential-level entry point for event handlers.
module ExoMonad.Guest.Lifecycle.DevTransitions
  ( -- * Events
    DevEvent (..),

    -- * Existential-level transitions
    applyDevEvent,

    -- * Typed transition functions
    startWork,
    filePR,
    reviewReceived,
    pushFixes,
    approve,
    complete,
    failDev,

    -- * Stop hook
    canExit,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.Log qualified as Log
import ExoMonad.Effects.Log (LogEmitEvent, LogInfo)
import ExoMonad.Guest.Lifecycle.DevState
import ExoMonad.Guest.Lifecycle.PhaseEffect (StopCheckResult (..), getDevPhase, setDevPhase)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect_)
import ExoMonad.Guest.Types (Effects)

-- | Dev lifecycle events.
data DevEvent
  = PRCreated PRNumber Text Text
  | NotifyParentSuccess Text
  | NotifyParentFailure Text
  | ShutdownRequested
  | ReviewReceivedEv PRNumber Text
  | ReviewApprovedEv PRNumber
  | FixesPushedEv PRNumber Text
  | CommitsPushedEv PRNumber Text

-- ============================================================================
-- Typed Transition Functions
-- ============================================================================

startWork :: DevState 'Spawned -> Eff Effects (DevState 'Working)
startWork SSpawned = do
  logTransition "Spawned" "Working"
  pure SWorking

filePR :: DevState 'Working -> PRNumber -> URL -> Text -> Eff Effects (DevState 'PRFiled)
filePR SWorking prNum url branch = do
  logTransition "Working" "PRFiled"
  emitPREvent prNum url branch
  pure (SPRFiled prNum url)

reviewReceived :: DevState 'PRFiled -> [Text] -> Eff Effects (DevState 'ChangesRequested)
reviewReceived (SPRFiled n _) comments = do
  logTransition "PRFiled" "ChangesRequested"
  pure (SChangesRequested n comments)

pushFixes :: DevState 'ChangesRequested -> Eff Effects (DevState 'UnderReview)
pushFixes (SChangesRequested n _) = do
  logTransition "ChangesRequested" "UnderReview"
  pure (SUnderReview n 1)

approve :: DevState 'UnderReview -> Eff Effects (DevState 'Approved)
approve (SUnderReview n _) = do
  logTransition "UnderReview" "Approved"
  pure (SApproved n)

complete :: DevState 'Approved -> Eff Effects (DevState 'Done)
complete (SApproved _) = do
  logTransition "Approved" "Done"
  pure SDone

failDev :: DevState p -> Text -> Eff Effects (DevState 'Failed)
failDev _ msg = do
  logTransition "any" "Failed"
  pure (SFailed msg)

-- ============================================================================
-- Existential-Level Transitions
-- ============================================================================

-- | Apply a dev event, reading and writing phase from KV.
-- Handles any current phase gracefully (logs unexpected transitions).
applyDevEvent :: DevEvent -> Eff Effects ()
applyDevEvent event = do
  mState <- getDevPhase
  let current = case mState of
        Just s -> s
        Nothing -> SomeDevState SSpawned
  newState <- transitionDev current event
  setDevPhase newState

transitionDev :: SomeDevState -> DevEvent -> Eff Effects SomeDevState
transitionDev (SomeDevState old) event = case event of
  PRCreated prNum url branch -> do
    logTransition (showPhase old) "PRFiled"
    emitPREvent prNum url branch
    pure (SomeDevState (SPRFiled prNum url))
  NotifyParentSuccess _ -> do
    logTransition (showPhase old) "Done"
    pure (SomeDevState SDone)
  NotifyParentFailure msg -> do
    logTransition (showPhase old) "Failed"
    pure (SomeDevState (SFailed msg))
  ShutdownRequested -> do
    logTransition (showPhase old) "Done"
    pure (SomeDevState SDone)
  ReviewReceivedEv prNum comments -> do
    logTransition (showPhase old) "ChangesRequested"
    pure (SomeDevState (SChangesRequested prNum [comments]))
  ReviewApprovedEv prNum -> do
    logTransition (showPhase old) "Approved"
    pure (SomeDevState (SApproved prNum))
  FixesPushedEv prNum _ci -> do
    let round = case old of
          SUnderReview _ r -> r + 1
          _ -> 1
    logTransition (showPhase old) "UnderReview"
    pure (SomeDevState (SUnderReview prNum round))
  CommitsPushedEv prNum _ci -> do
    let round = case old of
          SUnderReview _ r -> r + 1
          _ -> 1
    logTransition (showPhase old) "UnderReview"
    pure (SomeDevState (SUnderReview prNum round))

-- ============================================================================
-- Stop Hook
-- ============================================================================

canExit :: SomeDevState -> StopCheckResult
canExit (SomeDevState (SChangesRequested pr _)) =
  MustBlock $ "PR #" <> T.pack (show pr) <> " has changes requested. Address review comments before stopping."
canExit (SomeDevState (SPRFiled pr _)) =
  ShouldNudge $ "PR #" <> T.pack (show pr) <> " awaiting review. System will auto-notify parent."
canExit (SomeDevState (SUnderReview pr _)) =
  ShouldNudge $ "PR #" <> T.pack (show pr) <> " under review. System will auto-notify parent."
canExit (SomeDevState SSpawned) = Clean
canExit (SomeDevState SWorking) = Clean
canExit (SomeDevState (SApproved _)) = Clean
canExit (SomeDevState SDone) = Clean
canExit (SomeDevState (SFailed _)) = Clean

-- ============================================================================
-- Helpers
-- ============================================================================

showPhase :: DevState p -> Text
showPhase SSpawned = "Spawned"
showPhase SWorking = "Working"
showPhase (SPRFiled _ _) = "PRFiled"
showPhase (SUnderReview _ _) = "UnderReview"
showPhase (SChangesRequested _ _) = "ChangesRequested"
showPhase (SApproved _) = "Approved"
showPhase SDone = "Done"
showPhase (SFailed _) = "Failed"

logTransition :: Text -> Text -> Eff Effects ()
logTransition old new =
  void $
    suspendEffect_ @LogInfo $
      Log.InfoRequest
        { Log.infoRequestMessage = TL.fromStrict $ "[DevTransition] " <> old <> " -> " <> new,
          Log.infoRequestFields = ""
        }

emitPREvent :: PRNumber -> Text -> Text -> Eff Effects ()
emitPREvent prNum url branch = do
  let eventPayload =
        BSL.toStrict $
          Aeson.encode $
            object
              [ "pr_number" .= prNum,
                "pr_url" .= url,
                "head_branch" .= branch
              ]
  void $
    suspendEffect_ @LogEmitEvent
      ( Log.EmitEventRequest
          { Log.emitEventRequestEventType = "pr.filed",
            Log.emitEventRequestPayload = eventPayload,
            Log.emitEventRequestTimestamp = 0
          }
      )
