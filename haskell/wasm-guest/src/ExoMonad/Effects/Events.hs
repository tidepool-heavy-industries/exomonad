{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Effects.Events
  ( EventsNotifyEvent,
    EventsNotifyParent,

    -- * Proto types
    module Effects.Events,
  )
where

import Effects.Events
import ExoMonad.Effect.Class (Effect (..))

-- | Notify event effect
data EventsNotifyEvent

instance Effect EventsNotifyEvent where
  type Input EventsNotifyEvent = NotifyEventRequest
  type Output EventsNotifyEvent = NotifyEventResponse
  effectId = "events.notify_event"

-- | Notify parent effect
data EventsNotifyParent

instance Effect EventsNotifyParent where
  type Input EventsNotifyParent = NotifyParentRequest
  type Output EventsNotifyParent = NotifyParentResponse
  effectId = "events.notify_parent"
