{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module ExoMonad.CircuitBreaker.Interpreter
  ( runCircuitBreakerIO
  ) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.Freer (Eff, Member, interpret, LastMember, sendM)
import qualified Data.Map.Strict as Map

import ExoMonad.Effect.CircuitBreaker
import ExoMonad.Control.Hook.CircuitBreaker (CircuitBreakerMap, getCircuitBreakerState, getAllCircuitBreakerStates, resetSession, resetAll)

runCircuitBreakerIO :: LastMember IO effs => CircuitBreakerMap -> Eff (CircuitBreaker ': effs) a -> Eff effs a
runCircuitBreakerIO cbMap = interpret $ \case
  GetCBState sessionId -> sendM $ getCircuitBreakerState cbMap sessionId
  GetAllCBStates -> sendM $ getAllCircuitBreakerStates cbMap
  ResetCBSession sessionId -> sendM $ resetSession cbMap sessionId
  ResetAllCB -> sendM $ resetAll cbMap
