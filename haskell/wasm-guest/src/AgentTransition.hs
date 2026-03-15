{-# LANGUAGE OverloadedStrings #-}

module AgentTransition
  ( AgentTransition(..)
  , applyTransition
  , StopCheckResult(..)
  ) where

import Control.Monad.Freer (Eff)
import Data.Aeson (FromJSON, ToJSON)
import ExoMonad.Guest.Lifecycle (getPhase, setPhase)
import ExoMonad.Guest.Types (Effects)

data StopCheckResult
  = MustBlock String
  | ShouldNudge String
  | Clean

class (FromJSON phase, ToJSON phase) => AgentTransition phase event where
  transition :: phase -> event -> Eff Effects (phase, [Eff Effects ()])

applyTransition :: forall phase event. AgentTransition phase event
                => event -> Eff Effects ()
applyTransition event = do
  mPhase <- getPhase @phase
  case mPhase of
    Nothing -> pure ()
    Just phase -> do
      (newPhase, effects) <- transition phase event
      setPhase newPhase
      sequence_ effects
