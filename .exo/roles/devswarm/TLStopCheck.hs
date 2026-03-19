{-# LANGUAGE TypeApplications #-}

-- | Shared stop-hook logic for TL and Root roles.
module TLStopCheck (tlStopCheck) where

import Control.Monad.Freer (Eff)
import ExoMonad.Guest.StateMachine (StopCheckResult(..), checkExit)
import ExoMonad.Guest.Effects.StopHook (checkUncommittedWork, getCurrentBranch)
import ExoMonad.Guest.Types (StopDecision(..), StopHookOutput(..), blockStopResponse, allowStopResponse)
import ExoMonad.Types (Effects)
import TLPhase (TLPhase (..), TLEvent (..))

-- | Standard TL stop check: blocks if PR filed, nudges if children pending or uncommitted work.
tlStopCheck :: Eff Effects StopHookOutput
tlStopCheck = do
  branch <- getCurrentBranch
  if branch `elem` ["main", "master"]
    then pure allowStopResponse
    else do
      result <- checkExit @TLPhase @TLEvent branch TLPlanning
      case result of
        MustBlock msg -> pure $ blockStopResponse msg
        ShouldNudge msg -> pure $ StopHookOutput Allow (Just msg)
        Clean -> do
          nudge <- checkUncommittedWork branch
          case nudge of
            Just msg -> pure $ StopHookOutput Allow (Just msg)
            Nothing -> pure allowStopResponse
