{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Effects.Events
  ( EventsWaitForEvent
  , EventsNotifyEvent
  , EventsNotifyParent
  , waitForEvent
  , notifyEvent
  , notifyParent
  ) where

import Effects.Events qualified as Proto
import ExoMonad.Effect.Class (Effect (..), runEffect, EffectError)
import ExoMonad.Guest.Proto (fromText)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word (Word64)

-- | Wait for event effect
data EventsWaitForEvent

instance Effect EventsWaitForEvent where
  type Input EventsWaitForEvent = Proto.WaitForEventRequest
  type Output EventsWaitForEvent = Proto.WaitForEventResponse
  effectId = "events.wait_for_event"

-- | Smart constructor for wait_for_event
waitForEvent :: [Text] -> Int -> Word64 -> IO (Either EffectError Proto.WaitForEventResponse)
waitForEvent types timeout afterEventId =
  runEffect @EventsWaitForEvent $
    Proto.WaitForEventRequest
      { Proto.waitForEventRequestTypes = V.fromList (map fromText types)
      , Proto.waitForEventRequestTimeoutSecs = fromIntegral timeout
      , Proto.waitForEventRequestAfterEventId = afterEventId
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
      { Proto.notifyEventRequestSessionId = fromText sessionId
      , Proto.notifyEventRequestEvent = Just event
      }

-- | Notify parent effect
data EventsNotifyParent

instance Effect EventsNotifyParent where
  type Input EventsNotifyParent = Proto.NotifyParentRequest
  type Output EventsNotifyParent = Proto.NotifyParentResponse
  effectId = "events.notify_parent"

-- | Smart constructor for notify_parent
notifyParent :: Text -> Text -> IO (Either EffectError Proto.NotifyParentResponse)
notifyParent status message =
  runEffect @EventsNotifyParent $
    Proto.NotifyParentRequest
      { Proto.notifyParentRequestStatus = fromText status
      , Proto.notifyParentRequestMessage = fromText message
      }
