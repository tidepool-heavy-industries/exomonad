{-# LANGUAGE OverloadedStrings #-}

module AgentTransition
  ( AgentLifecycle(..)
  , AgentTransition(..)
  , applyTransition
  , StopCheckResult(..)
  ) where

import Control.Monad.Freer (Eff)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import ExoMonad.Guest.Lifecycle (getPhase, setPhase)
import ExoMonad.Guest.Types (Effects)

data StopCheckResult
  = MustBlock Text
  | ShouldNudge Text
  | Clean

class (FromJSON phase, ToJSON phase) => AgentLifecycle phase where
  initialPhase :: phase

class AgentLifecycle phase => AgentTransition phase event where
  transition :: phase -> event -> Eff Effects (phase, [Eff Effects ()])

applyTransition :: forall phase event. AgentTransition phase event
                => event -> Eff Effects ()
applyTransition event = do
  mPhase <- getPhase @phase
  let phase = fromMaybe (initialPhase @phase) mPhase
  (newPhase, effects) <- transition phase event
  setPhase newPhase
  sequence_ effects

fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing = d
fromMaybe _ (Just x) = x
