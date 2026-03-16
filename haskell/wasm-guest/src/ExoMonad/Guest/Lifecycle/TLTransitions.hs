-- | TL agent lifecycle transitions with GADT-enforced phase safety.
module ExoMonad.Guest.Lifecycle.TLTransitions
  ( -- * Events
    TLEvent (..),

    -- * Existential-level transitions
    applyTLEvent,

    -- * Stop hook
    canExit,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.Log qualified as Log
import ExoMonad.Effects.Log (LogInfo)
import ExoMonad.Guest.Lifecycle.DevState (PRNumber, URL)
import ExoMonad.Guest.Lifecycle.PhaseEffect (StopCheckResult (..), getTLPhase, setTLPhase)
import ExoMonad.Guest.Lifecycle.TLState
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect_)
import ExoMonad.Guest.Types (Effects)

-- | TL lifecycle events.
data TLEvent
  = ChildSpawned ChildHandle
  | ChildCompleted Text
  | ChildFailed Text Text
  | PRMerged PRNumber Text
  | AllChildrenDone
  | OwnPRFiled PRNumber URL Text

-- ============================================================================
-- Existential-Level Transitions
-- ============================================================================

applyTLEvent :: TLEvent -> Eff Effects ()
applyTLEvent event = do
  mState <- getTLPhase
  let current = case mState of
        Just s -> s
        Nothing -> SomeTLState STLPlanning
  newState <- transitionTL current event
  setTLPhase newState

transitionTL :: SomeTLState -> TLEvent -> Eff Effects SomeTLState
transitionTL (SomeTLState old) event = case event of
  ChildSpawned handle -> do
    let slug = chSlug handle
        newState = case old of
          STLWaiting children ->
            SomeTLState (STLWaiting (Map.insert slug handle children))
          STLDispatching ->
            SomeTLState (STLWaiting (Map.singleton slug handle))
          _ ->
            SomeTLState (STLWaiting (Map.singleton slug handle))
    logTransitionWithSlug slug (showPhase old) (showSomePhase newState)
    pure newState
  ChildCompleted slug -> do
    let newState = case old of
          STLWaiting children ->
            let remaining = Map.delete slug children
             in if Map.null remaining
                  then SomeTLState STLAllMerged
                  else SomeTLState (STLWaiting remaining)
          _ -> SomeTLState old
    logTransitionWithSlug slug (showPhase old) (showSomePhase newState)
    pure newState
  ChildFailed slug reason -> do
    let newPhase = STLFailed (slug <> ": " <> reason)
    logTransitionWithSlug slug (showPhase old) "Failed"
    pure (SomeTLState newPhase)
  PRMerged _prNum slug -> do
    let newState = case old of
          STLMerging _ children ->
            let remaining = Map.delete slug children
             in if Map.null remaining
                  then SomeTLState STLAllMerged
                  else SomeTLState (STLWaiting remaining)
          STLWaiting children ->
            let remaining = Map.delete slug children
             in if Map.null remaining
                  then SomeTLState STLAllMerged
                  else SomeTLState (STLWaiting remaining)
          _ -> SomeTLState old
    logTransitionWithSlug slug (showPhase old) (showSomePhase newState)
    pure newState
  AllChildrenDone -> do
    logTransition (showPhase old) "Done"
    pure (SomeTLState STLDone)
  OwnPRFiled pr url branch -> do
    let newState = SomeTLState (STLPRFiled pr url)
    logTransitionWithSlug branch (showPhase old) (showSomePhase newState)
    pure newState

-- ============================================================================
-- Stop Hook
-- ============================================================================

canExit :: SomeTLState -> StopCheckResult
canExit (SomeTLState STLPlanning) = Clean
canExit (SomeTLState STLDispatching) = ShouldNudge "Still dispatching children."
canExit (SomeTLState (STLWaiting children)) =
  ShouldNudge $ T.pack (show (Map.size children)) <> " children still pending."
canExit (SomeTLState (STLMerging pr children)) =
  ShouldNudge $ "Merging PR #" <> T.pack (show pr) <> ", " <> T.pack (show (Map.size children)) <> " remaining."
canExit (SomeTLState STLAllMerged) = Clean
canExit (SomeTLState (STLPRFiled pr _url)) =
  MustBlock $ "PR #" <> T.pack (show pr) <> " filed, waiting for parent to merge. Do not exit."
canExit (SomeTLState STLDone) = Clean
canExit (SomeTLState (STLFailed _)) = Clean

-- ============================================================================
-- Helpers
-- ============================================================================

showPhase :: TLState p -> Text
showPhase STLPlanning = "Planning"
showPhase STLDispatching = "Dispatching"
showPhase (STLWaiting _) = "Waiting"
showPhase (STLMerging _ _) = "Merging"
showPhase STLAllMerged = "AllMerged"
showPhase (STLPRFiled _ _) = "PRFiled"
showPhase STLDone = "Done"
showPhase (STLFailed _) = "Failed"

showSomePhase :: SomeTLState -> Text
showSomePhase (SomeTLState s) = showPhase s

logTransition :: Text -> Text -> Eff Effects ()
logTransition old new =
  void $
    suspendEffect_ @LogInfo $
      Log.InfoRequest
        { Log.infoRequestMessage = TL.fromStrict $ "[TLTransition] " <> old <> " -> " <> new,
          Log.infoRequestFields = ""
        }

logTransitionWithSlug :: Text -> Text -> Text -> Eff Effects ()
logTransitionWithSlug slug old new =
  void $
    suspendEffect_ @LogInfo $
      Log.InfoRequest
        { Log.infoRequestMessage = TL.fromStrict $ "[TLTransition:" <> slug <> "] " <> old <> " -> " <> new,
          Log.infoRequestFields = ""
        }
