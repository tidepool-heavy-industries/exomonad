{-# LANGUAGE OverloadedStrings #-}

module DevTransitions
  ( DevEvent(..)
  , canExit
  ) where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Aeson (ToJSON)
import Data.ByteString.Lazy qualified as BSL
import Data.Aeson qualified as Aeson
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.Log qualified as Log
import ExoMonad.Effects.Log (LogEmitEvent, LogInfo)
import ExoMonad.Guest.Lifecycle (DevPhase(..), PRNumber, getDevPhase)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect_)
import ExoMonad.Guest.Types (Effects)
import AgentTransition (AgentTransition(..), StopCheckResult(..))

data DevEvent
  = PRCreated PRNumber Text Text
  | NotifyParentSuccess Text
  | NotifyParentFailure Text
  | ShutdownRequested
  | ReviewReceived PRNumber Text
  | ReviewApproved PRNumber
  | FixesPushed PRNumber Text
  | CommitsPushed PRNumber Text

instance AgentTransition DevPhase DevEvent where
  transition phase event = case event of
    PRCreated prNum url branch -> do
      let newPhase = DevPRFiled prNum
      pure (newPhase, [logTransition phase newPhase, emitPREvent prNum url branch])
    NotifyParentSuccess msg ->
      pure (DevDone, [logTransition phase DevDone])
    NotifyParentFailure msg ->
      let newPhase = DevFailed msg
      in pure (newPhase, [logTransition phase newPhase])
    ShutdownRequested ->
      pure (DevDone, [logTransition phase DevDone])
    ReviewReceived prNum comments -> do
      let newPhase = DevChangesRequested prNum [comments]
      pure (newPhase, [logTransition phase newPhase])
    ReviewApproved prNum -> do
      let newPhase = DevApproved prNum
      pure (newPhase, [logTransition phase newPhase])
    FixesPushed prNum ci -> do
      let round = case phase of
            DevUnderReview _ r -> r + 1
            _ -> 1
          newPhase = DevUnderReview prNum round
      pure (newPhase, [logTransition phase newPhase])
    CommitsPushed prNum ci -> do
      let round = case phase of
            DevUnderReview _ r -> r + 1
            _ -> 1
          newPhase = DevUnderReview prNum round
      pure (newPhase, [logTransition phase newPhase])

canExit :: DevPhase -> StopCheckResult
canExit (DevChangesRequested pr _) =
  MustBlock $ "PR #" <> show pr <> " has changes requested. Address review comments before stopping."
canExit (DevPRFiled pr) =
  ShouldNudge $ "PR #" <> show pr <> " awaiting review. System will auto-notify parent."
canExit (DevUnderReview pr _) =
  ShouldNudge $ "PR #" <> show pr <> " under review. System will auto-notify parent."
canExit DevWorking = ShouldNudge "Still in working phase."
canExit DevSpawned = Clean
canExit (DevApproved _) = Clean
canExit DevDone = Clean
canExit (DevFailed _) = Clean

logTransition :: DevPhase -> DevPhase -> Eff Effects ()
logTransition old new =
  void $ suspendEffect_ @LogInfo $ Log.InfoRequest
    { Log.infoRequestMessage = TL.fromStrict $
        "[DevTransition] " <> T.pack (show old) <> " -> " <> T.pack (show new)
    , Log.infoRequestFields = ""
    }

emitPREvent :: PRNumber -> Text -> Text -> Eff Effects ()
emitPREvent prNum url branch = do
  let eventPayload = BSL.toStrict $ Aeson.encode $ object
        [ "pr_number" .= prNum
        , "pr_url" .= url
        , "head_branch" .= branch
        ]
  void $ suspendEffect_ @LogEmitEvent (Log.EmitEventRequest
    { Log.emitEventRequestEventType = "agent.phase_transition"
    , Log.emitEventRequestPayload = eventPayload
    , Log.emitEventRequestTimestamp = 0
    })
