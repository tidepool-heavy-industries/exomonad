-- | Actor spawning using ki's structured concurrency.
--
-- Actors are spawned within a ki Scope - when the scope exits,
-- all actors are automatically cancelled.
module Tidepool.Actor.Spawn
  ( -- * Spawning
    spawnActor
  , spawnActorWithMailbox
    -- * Handler Types
  , ActorHandler
    -- * Re-exports
  , Ki.Scope
  , Ki.scoped
  ) where

import Control.Exception (AsyncException, SomeException, catch, fromException, throwIO)
import Control.Monad (forever)
import Data.Aeson (Value)
import qualified Data.Text as T
import qualified Ki

import Tidepool.Actor.Mailbox (Mailbox, newMailboxIO, receive)
import Tidepool.Actor.Types (Actor(..), ActorId)


-- | Handler function type for actors.
--
-- Takes a message and returns an IO action (effects run inline).
type ActorHandler = Value -> IO ()

-- | Spawn an actor within a ki scope.
--
-- The actor runs a loop: receive message -> call handler -> repeat.
-- When the scope exits, the actor is automatically cancelled.
--
-- @
-- Ki.scoped $ \\scope -> do
--   actor <- spawnActor scope "compute" $ \\msg -> do
--     putStrLn $ "Received: " <> show msg
--   -- actor runs until scope exits
-- @
spawnActor :: Ki.Scope -> ActorId -> ActorHandler -> IO Actor
spawnActor scope actorId handler = do
  mailbox <- newMailboxIO
  spawnActorWithMailbox scope actorId mailbox handler

-- | Spawn an actor with an existing mailbox.
--
-- Useful when you need to set up the mailbox before spawning
-- (e.g., for cross-actor references during setup).
spawnActorWithMailbox :: Ki.Scope -> ActorId -> Mailbox Value -> ActorHandler -> IO Actor
spawnActorWithMailbox scope aid mailbox handler = do
  _ <- Ki.fork scope $ actorLoop aid mailbox handler
  pure Actor
    { actorId      = aid
    , actorMailbox = mailbox
    }

-- | The main actor loop.
--
-- Runs forever, receiving messages and calling the handler.
-- Logs synchronous exceptions and continues.
-- IMPORTANT: Re-throws async exceptions to allow ki cancellation.
actorLoop :: ActorId -> Mailbox Value -> ActorHandler -> IO ()
actorLoop actorId mailbox handler = forever $ do
  msg <- receive mailbox
  catchSync (handler msg) (logException actorId)
  where
    logException :: ActorId -> SomeException -> IO ()
    logException aid e =
      putStrLn $ "Actor " <> T.unpack aid <> " caught exception: " <> show e

-- | Catch only synchronous exceptions; re-throw async exceptions.
--
-- This is critical for ki's structured concurrency - async exceptions
-- (like ThreadKilled) must propagate for proper cancellation.
catchSync :: IO a -> (SomeException -> IO a) -> IO a
catchSync action handler = action `catch` \e ->
  if isAsyncException e
    then throwIO e  -- Re-throw async exceptions
    else handler e

-- | Check if an exception is asynchronous.
--
-- Ki uses standard async exceptions (ThreadKilled) for cancellation.
isAsyncException :: SomeException -> Bool
isAsyncException e = case fromException e of
  Just (_ :: AsyncException) -> True
  Nothing -> False
