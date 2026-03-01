{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Effects.Coordination
  ( CoordinationAcquireMutex,
    CoordinationReleaseMutex,
    module Effects.Coordination,
  )
where

import Effects.Coordination
import ExoMonad.Effect.Class (Effect (..))

data CoordinationAcquireMutex

instance Effect CoordinationAcquireMutex where
  type Input CoordinationAcquireMutex = AcquireMutexRequest
  type Output CoordinationAcquireMutex = AcquireMutexResponse
  effectId = "coordination.acquire_mutex"

data CoordinationReleaseMutex

instance Effect CoordinationReleaseMutex where
  type Input CoordinationReleaseMutex = ReleaseMutexRequest
  type Output CoordinationReleaseMutex = ReleaseMutexResponse
  effectId = "coordination.release_mutex"
