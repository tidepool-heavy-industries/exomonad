{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

-- | Shim for legacy dev transitions.
module ExoMonad.Guest.Lifecycle.DevTransitions
  ( DevEvent (..),
    applyDevEvent,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import ExoMonad.Guest.StateMachine (applyEvent)
import DevPhase (DevPhase(..), DevEvent(..))
import ExoMonad.Guest.Types (Effects)

-- | Apply dev event by delegating to new state machine.
applyDevEvent :: DevEvent -> Eff Effects ()
applyDevEvent e = void $ applyEvent @DevPhase @DevEvent DevSpawned e
