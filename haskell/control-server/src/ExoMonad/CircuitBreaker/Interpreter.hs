{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.CircuitBreaker.Interpreter
  ( runCircuitBreakerIO,
  )
where

import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import ExoMonad.Control.Hook.CircuitBreaker (CircuitBreakerMap, getAllCircuitBreakerStates, getCircuitBreakerState, resetAll, resetSession)
import ExoMonad.Effect.CircuitBreaker

runCircuitBreakerIO :: (LastMember IO effs) => CircuitBreakerMap -> Eff (CircuitBreaker ': effs) a -> Eff effs a
runCircuitBreakerIO cbMap = interpret $ \case
  GetCBState sessionId -> sendM $ getCircuitBreakerState cbMap sessionId
  GetAllCBStates -> sendM $ getAllCircuitBreakerStates cbMap
  ResetCBSession sessionId -> sendM $ resetSession cbMap sessionId
  ResetAllCB -> sendM $ resetAll cbMap
