{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

-- | Shim for legacy TL transitions.
module ExoMonad.Guest.Lifecycle.TLTransitions
  ( TLEvent (..),
    applyTLEvent,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import ExoMonad.Guest.StateMachine (applyEvent)
import TLPhase (TLPhase(..), TLEvent(..))
import ExoMonad.Guest.Types (Effects)

-- | Apply TL event by delegating to new state machine.
applyTLEvent :: TLEvent -> Eff Effects ()
applyTLEvent e = void $ applyEvent @TLPhase @TLEvent TLPlanning e
