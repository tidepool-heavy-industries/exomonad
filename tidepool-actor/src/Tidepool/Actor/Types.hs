-- | Core types for the actor runtime.
--
-- Minimal surface area: just Actor and ActorId.
-- Ki scope owns lifecycle - no need to track threads.
module Tidepool.Actor.Types
  ( -- * Actor Types
    Actor(..)
  , ActorId
  ) where

import Data.Aeson (Value)
import Data.Text (Text)

import Tidepool.Actor.Mailbox (Mailbox)


-- | Unique identifier for an actor (corresponds to graph node name).
type ActorId = Text

-- | An actor is a running computation with a mailbox.
--
-- No thread field - ki's scope manages lifecycle.
-- When the scope exits, all actors are automatically cleaned up.
data Actor = Actor
  { actorId      :: !ActorId
    -- ^ Unique identifier (node name)
  , actorMailbox :: !(Mailbox Value)
    -- ^ Incoming message queue
  }
