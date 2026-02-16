{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Effects.Events
  ( EventsNotifyEvent,
    EventsNotifyParent,
    notifyEvent,
    notifyParent,
  )
where

import Data.Text (Text)
import Effects.Events qualified as Proto
import ExoMonad.Effect.Class (Effect (..), EffectError, runEffect)
import ExoMonad.Guest.Proto (fromText)

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
      { Proto.notifyEventRequestSessionId = fromText sessionId,
        Proto.notifyEventRequestEvent = Just event
      }

-- | Notify parent effect
data EventsNotifyParent

instance Effect EventsNotifyParent where
  type Input EventsNotifyParent = Proto.NotifyParentRequest
  type Output EventsNotifyParent = Proto.NotifyParentResponse
  effectId = "events.notify_parent"

-- | Smart constructor for notify_parent
notifyParent :: Text -> Text -> Text -> IO (Either EffectError Proto.NotifyParentResponse)
notifyParent agentId status message =
  runEffect @EventsNotifyParent $
    Proto.NotifyParentRequest
      { Proto.notifyParentRequestStatus = fromText status,
        Proto.notifyParentRequestMessage = fromText message,
        Proto.notifyParentRequestAgentId = fromText agentId
      }
