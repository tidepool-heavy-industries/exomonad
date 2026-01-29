{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module ExoMonad.Effect.CircuitBreaker where

import Control.Monad.Freer.TH (makeEffect)
import Data.Map.Strict (Map)
import ExoMonad.Control.Hook.CircuitBreaker (CircuitBreakerState, SessionId)

data CircuitBreaker r where
  GetCBState :: SessionId -> CircuitBreaker (Maybe CircuitBreakerState)
  GetAllCBStates :: CircuitBreaker (Map SessionId CircuitBreakerState)
  ResetCBSession :: SessionId -> CircuitBreaker ()
  ResetAllCB :: CircuitBreaker ()

makeEffect ''CircuitBreaker
