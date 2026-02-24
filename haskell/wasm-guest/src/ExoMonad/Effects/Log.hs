{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Logging effects for structured logging and event emission.
--
-- All effects are dispatched via the @log@ namespace.
-- Request and response types are proto-generated from @proto/effects/log.proto@.
--
-- = Example
--
-- @
-- import ExoMonad.Effects.Log
--
-- main :: IO ()
-- main = do
--   _ <- logInfo (InfoRequest "Starting process" "")
--   _ <- emitEvent (EmitEventRequest "agent.started" payload 0)
--   emitStructuredEvent "agent.spawned" (object ["slug" .= "feature-a"])
-- @
module ExoMonad.Effects.Log
  ( -- * Effect Types
    LogInfo,
    LogError,
    LogDebug,
    LogWarn,
    LogEmitEvent,

    -- * Re-exported proto types
    module Effects.Log,
  )
where

import Effects.Log
import ExoMonad.Effect.Class (Effect (..))

-- ============================================================================
-- Effect phantom types + instances
-- ============================================================================

data LogInfo

instance Effect LogInfo where
  type Input LogInfo = InfoRequest
  type Output LogInfo = LogResponse
  effectId = "log.info"

data LogError

instance Effect LogError where
  type Input LogError = ErrorRequest
  type Output LogError = LogResponse
  effectId = "log.error"

data LogDebug

instance Effect LogDebug where
  type Input LogDebug = DebugRequest
  type Output LogDebug = LogResponse
  effectId = "log.debug"

data LogWarn

instance Effect LogWarn where
  type Input LogWarn = WarnRequest
  type Output LogWarn = LogResponse
  effectId = "log.warn"

data LogEmitEvent

instance Effect LogEmitEvent where
  type Input LogEmitEvent = EmitEventRequest
  type Output LogEmitEvent = EmitEventResponse
  effectId = "log.emit_event"
