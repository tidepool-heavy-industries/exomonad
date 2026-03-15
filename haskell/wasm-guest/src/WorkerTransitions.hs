{-# LANGUAGE OverloadedStrings #-}

module WorkerTransitions
  ( WorkerEvent(..)
  , canExit
  ) where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.Log qualified as Log
import ExoMonad.Effects.Log (LogInfo)
import ExoMonad.Guest.Lifecycle (WorkerPhase(..))
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect_)
import ExoMonad.Guest.Types (Effects)
import AgentTransition (AgentTransition(..), StopCheckResult(..))

data WorkerEvent
  = WorkerStarted
  | WorkerCompleted Text
  | WorkerErrored Text

instance AgentTransition WorkerPhase WorkerEvent where
  transition phase event = case event of
    WorkerStarted ->
      pure (WorkerWorking, [logTransition phase WorkerWorking])
    WorkerCompleted msg ->
      pure (WorkerDone, [logTransition phase WorkerDone])
    WorkerErrored reason ->
      let newPhase = WorkerFailed reason
      in pure (newPhase, [logTransition phase newPhase])

canExit :: WorkerPhase -> StopCheckResult
canExit WorkerSpawned = Clean
canExit WorkerWorking = Clean
canExit WorkerDone = Clean
canExit (WorkerFailed _) = Clean

logTransition :: WorkerPhase -> WorkerPhase -> Eff Effects ()
logTransition old new =
  void $ suspendEffect_ @LogInfo $ Log.InfoRequest
    { Log.infoRequestMessage = TL.fromStrict $
        "[WorkerTransition] " <> T.pack (show old) <> " -> " <> T.pack (show new)
    , Log.infoRequestFields = ""
    }
