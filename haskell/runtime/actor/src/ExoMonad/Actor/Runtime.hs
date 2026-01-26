-- | Actor system runtime built on ki's structured concurrency.
--
-- When the ki scope exits, all actors are automatically cleaned up.
module ExoMonad.Actor.Runtime
  ( -- * Running Actor Systems
    withActorSystem
    -- * Types
  , Router
  ) where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Data.Aeson (Value, FromJSON, fromJSON, Result(..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Ki

import ExoMonad.Actor.Mailbox (send)
import ExoMonad.Actor.Types (Actor(..), ActorId)


-- | A router sends messages to actors by ID.
--
-- Special target \"exit\" delivers the final result.
type Router = ActorId -> Value -> IO ()


-- | Run an actor system with structured concurrency.
--
-- This is the main entry point:
--
-- 1. Creates a ki scope and IORef-based router
-- 2. Runs the setup action to spawn actors
-- 3. Initializes the router with the actor map
-- 4. Sends initial message to \"entry\"
-- 5. Waits for \"exit\" to receive the result
-- 6. Scope exit automatically cleans up all actors
--
-- The setup function receives a router that can be used during actor
-- construction. The router is initialized after all actors are spawned.
--
-- @
-- result <- withActorSystem (toJSON initialInput) $ \\scope router -> do
--   compute <- spawnActor scope \"compute\" $ \\msg -> do
--     let result = processMessage msg
--     router \"exit\" (toJSON result)
--   pure $ Map.singleton \"compute\" compute
-- @
withActorSystem
  :: forall result
   . FromJSON result
  => Value  -- ^ Initial message to send to \"entry\"
  -> (Ki.Scope -> Router -> IO (Map ActorId Actor))
     -- ^ Setup: spawn actors, return the actor map
  -> IO result
withActorSystem entryPayload setup = do
  exitChan <- newEmptyMVar
  routerRef <- newIORef uninitializedRouter

  Ki.scoped $ \scope -> do
    -- Pass a router that reads from the ref
    let router target payload = do
          r <- readIORef routerRef
          r target payload

    -- User spawns actors
    actors <- setup scope router

    -- Install the real router
    writeIORef routerRef (realRouter actors exitChan)

    -- Kick off the system
    router "entry" entryPayload

    -- Wait for exit signal
    takeMVar exitChan

  where
    uninitializedRouter :: Router
    uninitializedRouter _ _ = error "Router not initialized (message sent during setup before actors registered)"

    realRouter :: Map ActorId Actor -> MVar result -> Router
    realRouter actors exitChan target payload
      | target == "exit" = case fromJSON @result payload of
          Success r -> putMVar exitChan r
          Error e   -> error $ "Exit actor failed to parse result: " <> e
      | otherwise = case Map.lookup target actors of
          Just actor -> send (actorMailbox actor) payload
          Nothing    -> error $ "Unknown actor: " <> T.unpack target
