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

    -- * Smart Constructors
    logInfo,
    logError,
    logDebug,
    logWarn,
    emitEvent,
    emitStructuredEvent,

    -- * Re-exported proto types
    module Effects.Log,
  )
where

import Control.Monad (void)
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Effects.EffectError (EffectError)
import Effects.Log
import ExoMonad.Effect.Class (Effect (..), runEffect, runEffect_)

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

-- ============================================================================
-- Smart constructors
-- ============================================================================

logInfo :: InfoRequest -> IO (Either EffectError LogResponse)
logInfo = runEffect @LogInfo

logError :: ErrorRequest -> IO (Either EffectError LogResponse)
logError = runEffect @LogError

logDebug :: DebugRequest -> IO (Either EffectError LogResponse)
logDebug = runEffect @LogDebug

logWarn :: WarnRequest -> IO (Either EffectError LogResponse)
logWarn = runEffect @LogWarn

emitEvent :: EmitEventRequest -> IO (Either EffectError EmitEventResponse)
emitEvent = runEffect @LogEmitEvent

-- | Fire-and-forget structured event emission.
--
-- Encodes the payload as JSON and sends it via the @log.emit_event@ effect.
-- Failures are silently ignored (fire-and-forget).
emitStructuredEvent :: Text -> Value -> IO ()
emitStructuredEvent eventType payload =
  void $
    runEffect_ @LogEmitEvent
      EmitEventRequest
        { emitEventRequestEventType = TL.fromStrict eventType,
          emitEventRequestPayload = BSL.toStrict (Aeson.encode payload),
          emitEventRequestTimestamp = 0
        }
