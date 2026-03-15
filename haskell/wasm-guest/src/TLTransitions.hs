{-# LANGUAGE OverloadedStrings #-}

module TLTransitions
  ( TLEvent(..)
  , canExit
  ) where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.Log qualified as Log
import ExoMonad.Effects.Log (LogInfo)
import ExoMonad.Guest.Lifecycle (TLPhase(..), PRNumber)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect_)
import ExoMonad.Guest.Types (Effects)
import AgentTransition (AgentTransition(..), StopCheckResult(..))

data TLEvent
  = ChildSpawned Text
  | ChildCompleted Text
  | ChildFailed Text Text
  | PRMerged PRNumber
  | AllChildrenDone

instance AgentTransition TLPhase TLEvent where
  transition phase event = case event of
    ChildSpawned slug ->
      let newPhase = TLDispatching
      in pure (newPhase, [logTransition phase newPhase])
    ChildCompleted slug ->
      let newPhase = case phase of
            TLIdle n | n <= 1 -> TLAllMerged
            TLIdle n -> TLIdle (n - 1)
            _ -> phase
      in pure (newPhase, [logTransition phase newPhase])
    ChildFailed slug reason ->
      let newPhase = TLFailed (slug <> ": " <> reason)
      in pure (newPhase, [logTransition phase newPhase])
    PRMerged prNum ->
      let newPhase = case phase of
            TLMerging _ n | n <= 1 -> TLAllMerged
            TLMerging _ n -> TLIdle (n - 1)
            _ -> phase
      in pure (newPhase, [logTransition phase newPhase])
    AllChildrenDone ->
      pure (TLDone, [logTransition phase TLDone])

canExit :: TLPhase -> StopCheckResult
canExit TLPlanning = Clean
canExit TLDispatching = ShouldNudge "Still dispatching children."
canExit (TLIdle n) = ShouldNudge $ show n <> " children still pending."
canExit (TLMerging pr n) = ShouldNudge $ "Merging PR #" <> show pr <> ", " <> show n <> " remaining."
canExit TLAllMerged = Clean
canExit TLDone = Clean
canExit (TLFailed _) = Clean

logTransition :: TLPhase -> TLPhase -> Eff Effects ()
logTransition old new =
  void $ suspendEffect_ @LogInfo $ Log.InfoRequest
    { Log.infoRequestMessage = TL.fromStrict $
        "[TLTransition] " <> T.pack (show old) <> " -> " <> T.pack (show new)
    , Log.infoRequestFields = ""
    }
