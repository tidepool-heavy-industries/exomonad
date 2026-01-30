-- | Actor runtime backend for ExoMonad graphs.
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
-- import ExoMonad.Actor.Graph (runGraphAsActors, pureHandler)
-- import qualified Data.Map.Strict as Map
--
-- main :: IO ()
-- main = do
--   let handlers = Map.fromList [(\"entry\", pureHandler myHandler)]
--   result <- runGraphAsActors handlers (toJSON initialInput)
--   print result
-- @
module ExoMonad.Actor
  ( -- * Core Types
    module ExoMonad.Actor.Types,

    -- * Mailbox Operations
    module ExoMonad.Actor.Mailbox,

    -- * Actor Spawning
    module ExoMonad.Actor.Spawn,

    -- * Runtime
    module ExoMonad.Actor.Runtime,
  )
where

import ExoMonad.Actor.Mailbox
import ExoMonad.Actor.Runtime
import ExoMonad.Actor.Spawn
import ExoMonad.Actor.Types
