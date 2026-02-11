{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Effects.Events
  ( EventsWaitForEvent
  , EventsNotifyEvent
  , waitForEvent
  , notifyEvent
  ) where

import Effects.Events qualified as Proto
import ExoMonad.Effect.Class (Effect (..), runEffect, EffectError)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V

-- | Wait for event effect
data EventsWaitForEvent

instance Effect EventsWaitForEvent where
  type Input EventsWaitForEvent = Proto.WaitForEventRequest
  type Output EventsWaitForEvent = Proto.WaitForEventResponse
  effectId = "events.wait_for_event"

-- | Smart constructor for wait_for_event
waitForEvent :: [Text] -> Int -> IO (Either EffectError Proto.WaitForEventResponse)
waitForEvent types timeout =
  runEffect @EventsWaitForEvent $
    Proto.WaitForEventRequest
      { Proto.waitForEventRequestTypes = V.fromList (map TL.fromStrict types)
      , Proto.waitForEventRequestTimeoutSecs = fromIntegral timeout
      }

-- | Notify event effect
data EventsNotifyEvent

instance Effect EventsNotifyEvent where
  type Input EventsNotifyEvent = Proto.NotifyEventRequest
  type Output EventsNotifyEvent = Proto.NotifyEventResponse
  effectId = "events.notify_event"

-- | Smart constructor for notify_event
notifyEvent :: Text -> Proto.Event -> IO (Either EffectError Proto.NotifyEventResponse)
notifyEvent sessionId event =
  runEffect @EventsNotifyEvent $
    Proto.NotifyEventRequest
      { Proto.notifyEventRequestSessionId = TL.fromStrict sessionId
      , Proto.notifyEventRequestEvent = Just event
      }