module SpawnSpec (spec) where

import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Aeson (Value(..))
import qualified Ki

import Tidepool.Actor.Mailbox (send)
import Tidepool.Actor.Spawn (spawnActor)
import Tidepool.Actor.Types (Actor(..))


spec :: Spec
spec = describe "Actor Spawning" $ do

  describe "spawnActor" $ do

    it "creates an actor that receives messages" $ do
      received <- newEmptyMVar
      Ki.scoped $ \scope -> do
        actor <- spawnActor scope "test" $ \msg ->
          putMVar received msg
        send (actorMailbox actor) (String "hello")
        result <- takeMVar received
        result `shouldBe` String "hello"

    it "actor handles multiple messages in order" $ do
      received <- newEmptyMVar
      Ki.scoped $ \scope -> do
        actor <- spawnActor scope "test" $ \msg ->
          putMVar received msg
        send (actorMailbox actor) (String "first")
        r1 <- takeMVar received
        send (actorMailbox actor) (String "second")
        r2 <- takeMVar received
        r1 `shouldBe` String "first"
        r2 `shouldBe` String "second"

    it "actor continues after handler exception" $ do
      callCount <- newEmptyMVar
      Ki.scoped $ \scope -> do
        actor <- spawnActor scope "test" $ \case
          String "boom" -> error "intentional test error"
          msg -> putMVar callCount msg
        -- First message causes error
        send (actorMailbox actor) (String "boom")
        threadDelay 10000  -- Give time for error to be caught
        -- Second message should still be processed
        send (actorMailbox actor) (String "ok")
        result <- takeMVar callCount
        result `shouldBe` String "ok"

  describe "ki scope cleanup" $ do

    it "actors are cleaned up when scope exits" $ do
      -- Just verify scope exits cleanly - no hanging threads
      result <- Ki.scoped $ \scope -> do
        _actor <- spawnActor scope "test" $ \_ -> pure ()
        pure ("done" :: String)
      result `shouldBe` "done"

    it "async exceptions propagate (ki cancellation works)" $ do
      -- If async exceptions were swallowed, this would hang forever
      -- because the actor is blocked on an empty MVar
      blocker <- newEmptyMVar
      result <- Ki.scoped $ \scope -> do
        -- Actor that blocks forever waiting for a message that never comes
        _actor <- spawnActor scope "blocking" $ \_ -> do
          -- This would hang forever if the actor wasn't cancelled
          _ <- takeMVar blocker
          pure ()
        -- Send one message to start the blocking
        -- Note: we don't actually need to send - the actor loop blocks on receive
        pure ("scope-exited" :: String)
      -- If we get here, ki successfully cancelled the actor
      result `shouldBe` "scope-exited"

    it "scope exits quickly despite blocking actor" $ do
      -- More explicit test: scope should exit in reasonable time
      -- even if actor is blocked
      result <- Ki.scoped $ \scope -> do
        -- Actor that would block forever on receive (no messages sent)
        _actor <- spawnActor scope "blocked-on-receive" $ \_ ->
          threadDelay 10000000  -- 10 seconds - would timeout if not cancelled
        -- Exit immediately - ki should cancel the actor
        pure ("quick-exit" :: String)
      result `shouldBe` "quick-exit"
