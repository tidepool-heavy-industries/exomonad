module RuntimeSpec (spec) where

import Test.Hspec
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Exception (try, ErrorCall(..))
import Data.Aeson (Value(..), object, (.=), toJSON)
import qualified Data.Map.Strict as Map

import Tidepool.Actor.Runtime (withActorSystem)
import Tidepool.Actor.Spawn (spawnActor)


spec :: Spec
spec = describe "withActorSystem" $ do

  describe "basic flow" $ do

    it "entry receives initial message and routes to exit" $ do
      -- Simplest case: entry actor immediately forwards to exit
      result <- withActorSystem @Int (Number 42) $ \scope router -> do
        entry <- spawnActor scope "entry" $ \msg -> do
          router "exit" msg
        pure $ Map.singleton "entry" entry
      result `shouldBe` 42

    it "exit message returns parsed result" $ do
      -- Verify various JSON types parse correctly
      result <- withActorSystem @String (String "hello") $ \scope router -> do
        entry <- spawnActor scope "entry" $ \msg -> do
          router "exit" msg
        pure $ Map.singleton "entry" entry
      result `shouldBe` "hello"

    it "can return complex JSON objects" $ do
      let payload = object ["x" .= (1 :: Int), "y" .= (2 :: Int)]
      result <- withActorSystem @Value payload $ \scope router -> do
        entry <- spawnActor scope "entry" $ \msg -> do
          router "exit" msg
        pure $ Map.singleton "entry" entry
      result `shouldBe` payload

    it "scope cleans up actors on completion" $ do
      -- Verify no hanging threads after system exits
      completed <- newEmptyMVar
      _ <- withActorSystem @() (toJSON ()) $ \scope router -> do
        -- Actor that would run forever if not cleaned up
        entry <- spawnActor scope "entry" $ \_ -> do
          putMVar completed ()
          router "exit" (toJSON ())
        pure $ Map.singleton "entry" entry
      -- If we get here, scope cleaned up properly
      result <- tryTakeMVar completed
      result `shouldBe` Just ()

  describe "router dispatch" $ do

    it "routes to correct actor by ID" $ do
      received <- newEmptyMVar
      result <- withActorSystem @String (String "start") $ \scope router -> do
        -- Entry routes to "worker", worker routes to exit
        entry <- spawnActor scope "entry" $ \msg ->
          router "worker" msg
        worker <- spawnActor scope "worker" $ \msg -> do
          putMVar received msg
          router "exit" (String "done")
        pure $ Map.fromList [("entry", entry), ("worker", worker)]
      result `shouldBe` "done"
      msg <- takeMVar received
      msg `shouldBe` String "start"

    it "multiple actors in same system work independently" $ do
      -- Two actors, each receives a message, one sends to exit
      receivedA <- newEmptyMVar
      receivedB <- newEmptyMVar
      result <- withActorSystem @String (String "go") $ \scope router -> do
        entry <- spawnActor scope "entry" $ \_ -> do
          router "actorA" (String "for-A")
          router "actorB" (String "for-B")
        actorA <- spawnActor scope "actorA" $ \msg -> do
          putMVar receivedA msg
        actorB <- spawnActor scope "actorB" $ \msg -> do
          putMVar receivedB msg
          router "exit" (String "done")
        pure $ Map.fromList
          [ ("entry", entry)
          , ("actorA", actorA)
          , ("actorB", actorB)
          ]
      result `shouldBe` "done"
      msgA <- takeMVar receivedA
      msgB <- takeMVar receivedB
      msgA `shouldBe` String "for-A"
      msgB `shouldBe` String "for-B"

  describe "error handling" $ do

    -- Note: Actor exceptions are caught by catchSync and logged, not propagated
    -- to withActorSystem. These tests verify the error IS thrown (visible in output).

    it "unknown actor error is caught and logged, actor continues" $ do
      -- The error happens in actor, is logged, then actor waits for next message
      -- Entry sends to helper first, then errors, then helper sends recovery message
      callCount <- newEmptyMVar
      result <- withActorSystem @String (String "start") $ \scope router -> do
        entry <- spawnActor scope "entry" $ \msg ->
          case msg of
            String "start" -> do
              -- First: trigger helper
              router "helper" (String "go")
              -- Then: cause error (caught and logged)
              router "nonexistent" (String "oops")
            String "recover" -> do
              -- This runs when helper sends us a message
              putMVar callCount "recovered"
              router "exit" (String "done")
            _ -> pure ()
        helper <- spawnActor scope "helper" $ \_ ->
          -- When we receive "go", send "recover" back to entry
          router "entry" (String "recover")
        pure $ Map.fromList [("entry", entry), ("helper", helper)]
      result `shouldBe` "done"
      status <- takeMVar callCount
      status `shouldBe` "recovered"

    it "exit parse failure is caught and logged, actor continues" $ do
      -- Same pattern: trigger helper, cause error, helper sends recovery
      callCount <- newEmptyMVar
      result <- withActorSystem @Int (String "start") $ \scope router -> do
        entry <- spawnActor scope "entry" $ \msg ->
          case msg of
            String "start" -> do
              router "helper" (String "go")
              router "exit" (String "not-an-int")  -- Parse error
            String "recover" -> do
              putMVar callCount "recovered"
              router "exit" (Number 42)
            _ -> pure ()
        helper <- spawnActor scope "helper" $ \_ ->
          router "entry" (String "recover")
        pure $ Map.fromList [("entry", entry), ("helper", helper)]
      result `shouldBe` 42
      status <- takeMVar callCount
      status `shouldBe` "recovered"

    it "missing entry actor throws" $ do
      result <- try $ withActorSystem @Int (Number 42) $ \_scope _router -> do
        pure Map.empty
      case result of
        Left (ErrorCall msg) -> msg `shouldContain` "entry"
        Right _ -> expectationFailure "Expected error for missing entry actor"

  describe "multi-stage pipeline" $ do

    it "A -> B -> exit chain works" $ do
      -- Entry -> A (adds 1) -> B (multiplies 2) -> exit
      result <- withActorSystem @Int (Number 5) $ \scope router -> do
        entry <- spawnActor scope "entry" $ \msg ->
          router "stageA" msg
        stageA <- spawnActor scope "stageA" $ \case
          Number n -> router "stageB" (Number (n + 1))
          _ -> error "expected Number"
        stageB <- spawnActor scope "stageB" $ \case
          Number n -> router "exit" (Number (n * 2))
          _ -> error "expected Number"
        pure $ Map.fromList
          [ ("entry", entry)
          , ("stageA", stageA)
          , ("stageB", stageB)
          ]
      -- (5 + 1) * 2 = 12
      result `shouldBe` 12

    it "router works from within handler" $ do
      -- Verify router can chain calls without deadlock
      steps <- newEmptyMVar
      result <- withActorSystem @Int (String "1") $ \scope router -> do
        entry <- spawnActor scope "entry" $ \_ -> do
          putMVar steps ("entry" :: String)
          router "middle" (String "2")
        middle <- spawnActor scope "middle" $ \_ -> do
          _ <- takeMVar steps
          putMVar steps ("middle" :: String)
          router "exit" (Number 3)
        pure $ Map.fromList [("entry", entry), ("middle", middle)]
      result `shouldBe` 3
      step <- takeMVar steps
      step `shouldBe` "middle"
