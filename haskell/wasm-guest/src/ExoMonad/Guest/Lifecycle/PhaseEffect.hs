{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

-- | Shim for legacy phase effect.
module ExoMonad.Guest.Lifecycle.PhaseEffect
  ( StopCheckResult (..),
    getDevPhase,
    setDevPhase,
    getTLPhase,
    describeStopResult,
  )
where

import Control.Monad.Freer (Eff)
import Data.Text (Text)
import ExoMonad.Guest.StateMachine (StopCheckResult (..), getPhase, setPhase)
import ExoMonad.Guest.Lifecycle.DevState (SomeDevState (..), DevState (..))
import qualified DevPhase as NewDev
import qualified TLPhase as NewTL
import ExoMonad.Guest.Types (Effects)

getDevPhase :: Eff Effects (Maybe SomeDevState)
getDevPhase = do
  mPhase <- getPhase @NewDev.DevPhase @NewDev.DevEvent NewDev.DevSpawned
  case mPhase of
    Just NewDev.DevSpawned -> pure $ Just (SomeDevState SSpawned)
    Just NewDev.DevWorking -> pure $ Just (SomeDevState SWorking)
    Just (NewDev.DevPRFiled n url) -> pure $ Just (SomeDevState (SPRFiled n url))
    Just (NewDev.DevUnderReview n r) -> pure $ Just (SomeDevState (SUnderReview n r))
    Just (NewDev.DevChangesRequested n cs) -> pure $ Just (SomeDevState (SChangesRequested n cs))
    Just (NewDev.DevApproved n) -> pure $ Just (SomeDevState (SApproved n))
    Just NewDev.DevDone -> pure $ Just (SomeDevState SDone)
    Just (NewDev.DevFailed msg) -> pure $ Just (SomeDevState (SFailed msg))
    Nothing -> pure Nothing

setDevPhase :: SomeDevState -> Eff Effects ()
setDevPhase (SomeDevState SSpawned) = setPhase @NewDev.DevPhase @NewDev.DevEvent NewDev.DevSpawned
setDevPhase (SomeDevState SWorking) = setPhase @NewDev.DevPhase @NewDev.DevEvent NewDev.DevWorking
setDevPhase (SomeDevState (SPRFiled n url)) = setPhase @NewDev.DevPhase @NewDev.DevEvent (NewDev.DevPRFiled n url)
setDevPhase (SomeDevState (SUnderReview n r)) = setPhase @NewDev.DevPhase @NewDev.DevEvent (NewDev.DevUnderReview n r)
setDevPhase (SomeDevState (SChangesRequested n cs)) = setPhase @NewDev.DevPhase @NewDev.DevEvent (NewDev.DevChangesRequested n cs)
setDevPhase (SomeDevState (SApproved n)) = setPhase @NewDev.DevPhase @NewDev.DevEvent (NewDev.DevApproved n)
setDevPhase (SomeDevState SDone) = setPhase @NewDev.DevPhase @NewDev.DevEvent NewDev.DevDone
setDevPhase (SomeDevState (SFailed msg)) = setPhase @NewDev.DevPhase @NewDev.DevEvent (NewDev.DevFailed msg)

getTLPhase :: Eff Effects (Maybe NewTL.TLPhase)
getTLPhase = getPhase @NewTL.TLPhase @NewTL.TLEvent NewTL.TLPlanning

describeStopResult :: StopCheckResult -> Text
describeStopResult (MustBlock msg) = "block: " <> msg
describeStopResult (ShouldNudge msg) = "nudge: " <> msg
describeStopResult Clean = "clean"
