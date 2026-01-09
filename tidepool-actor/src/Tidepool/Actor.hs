-- | Actor runtime backend for Tidepool graphs.
--
-- This module provides a concurrent execution backend where each graph node
-- runs as an independent actor, communicating via message passing.
--
-- = Overview
--
-- In actor mode:
--
-- * Each graph node runs as an independent actor with its own mailbox
-- * @Goto \"target\" payload@ becomes an async message send
-- * Multiple nodes can be active concurrently
-- * Messages are JSON 'Value' (serializable, cross-backend ready)
--
-- = Basic Usage
--
-- @
-- import Tidepool.Actor
--
-- main :: IO ()
-- main = do
--   result <- runActorSystem myGraph (toJSON initialInput)
--   print result
-- @
module Tidepool.Actor
  ( -- * Core Types
    module Tidepool.Actor.Types
    -- * Mailbox Operations
  , module Tidepool.Actor.Mailbox
    -- * Actor Spawning
  , module Tidepool.Actor.Spawn
    -- * Runtime
  , module Tidepool.Actor.Runtime
  ) where

import Tidepool.Actor.Types
import Tidepool.Actor.Mailbox
import Tidepool.Actor.Spawn
import Tidepool.Actor.Runtime
