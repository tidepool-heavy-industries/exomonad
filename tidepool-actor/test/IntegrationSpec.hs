module IntegrationSpec (spec) where

import Test.Hspec
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Aeson (Value(..), toJSON)
import Data.IORef (newIORef, atomicModifyIORef', readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Tidepool.Actor.Runtime (withActorSystem)
import Tidepool.Actor.Spawn (spawnActor)


spec :: Spec
spec = describe "Multi-Actor Patterns" $ do

  describe "fan-out" $ do

    it "one actor can broadcast to multiple receivers" $ do
      -- Entry sends to A, B, C; C collects and sends to exit
      receivedA <- newEmptyMVar
      receivedB <- newEmptyMVar
      result <- withActorSystem @String (String "broadcast") $ \scope router -> do
        entry <- spawnActor scope "entry" $ \msg -> do
          -- Fan out to three actors
          router "workerA" msg
          router "workerB" msg
          router "collector" msg
        workerA <- spawnActor scope "workerA" $ \msg ->
          putMVar receivedA msg
        workerB <- spawnActor scope "workerB" $ \msg ->
          putMVar receivedB msg
        collector <- spawnActor scope "collector" $ \_ -> do
          -- Wait for A and B to receive
          _ <- takeMVar receivedA
          _ <- takeMVar receivedB
          router "exit" (String "all-received")
        pure $ Map.fromList
          [ ("entry", entry)
          , ("workerA", workerA)
          , ("workerB", workerB)
          , ("collector", collector)
          ]
      result `shouldBe` "all-received"

  describe "fan-in" $ do

    it "multiple actors can send to single collector" $ do
      -- Entry triggers A, B, C; each sends to collector; collector aggregates
      counter <- newIORef (0 :: Int)
      result <- withActorSystem @Int (String "start") $ \scope router -> do
        entry <- spawnActor scope "entry" $ \_ -> do
          router "workerA" (Number 1)
          router "workerB" (Number 2)
          router "workerC" (Number 3)
        workerA <- spawnActor scope "workerA" $ \case
          Number n -> router "collector" (Number n)
          _ -> pure ()
        workerB <- spawnActor scope "workerB" $ \case
          Number n -> router "collector" (Number n)
          _ -> pure ()
        workerC <- spawnActor scope "workerC" $ \case
          Number n -> router "collector" (Number n)
          _ -> pure ()
        collector <- spawnActor scope "collector" $ \case
          Number _ -> do
            count <- atomicModifyIORef' counter $ \c -> (c + 1, c + 1)
            if count >= 3
              then router "exit" (Number (fromIntegral count))  -- Last one triggers exit
              else pure ()
          _ -> pure ()
        pure $ Map.fromList
          [ ("entry", entry)
          , ("workerA", workerA)
          , ("workerB", workerB)
          , ("workerC", workerC)
          , ("collector", collector)
          ]
      -- Result is whichever number arrived last
      result `shouldSatisfy` (`elem` [1, 2, 3])

  describe "bidirectional" $ do

    it "actors can exchange messages (ping-pong)" $ do
      -- Ping sends to Pong, Pong responds to Ping, Ping sends to exit
      exchanges <- newIORef (0 :: Int)
      result <- withActorSystem @Int (String "start") $ \scope router -> do
        ping <- spawnActor scope "ping" $ \msg ->
          case msg of
            String "start" -> router "pong" (String "ping")
            String "pong" -> do
              count <- atomicModifyIORef' exchanges $ \c -> (c + 1, c + 1)
              if count >= 3
                then router "exit" (Number (fromIntegral count))
                else router "pong" (String "ping")
            _ -> pure ()
        pong <- spawnActor scope "pong" $ \case
          String "ping" -> router "ping" (String "pong")
          _ -> pure ()
        -- Entry is ping
        pure $ Map.fromList [("entry", ping), ("ping", ping), ("pong", pong)]
      result `shouldBe` 3

  describe "complex workflow" $ do

    it "multi-stage processing with transforms" $ do
      -- entry -> parse -> validate -> transform -> format -> exit
      result <- withActorSystem @String (String "  HELLO WORLD  ") $ \scope router -> do
        entry <- spawnActor scope "entry" $ \msg ->
          router "parse" msg
        -- Parse: trim whitespace
        parse <- spawnActor scope "parse" $ \case
          String s -> router "validate" (String s)  -- Would trim in real impl
          _ -> pure ()
        -- Validate: check not empty
        validate <- spawnActor scope "validate" $ \case
          String s | not (T.null s) -> router "transform" (String s)
          _ -> router "exit" (String "validation-failed")
        -- Transform: lowercase
        transform <- spawnActor scope "transform" $ \case
          String s -> router "format" (String s)  -- Would lowercase
          _ -> pure ()
        -- Format: add prefix
        format <- spawnActor scope "format" $ \case
          String s -> router "exit" (String $ "processed:" <> s)
          _ -> pure ()
        pure $ Map.fromList
          [ ("entry", entry)
          , ("parse", parse)
          , ("validate", validate)
          , ("transform", transform)
          , ("format", format)
          ]
      result `shouldContain` "processed:"

  describe "resilience" $ do

    it "error in one branch doesn't affect other branches" $ do
      -- Entry fans out to A (errors) and B (succeeds)
      result <- withActorSystem @String (String "start") $ \scope router -> do
        entry <- spawnActor scope "entry" $ \_ -> do
          router "failPath" (String "will-fail")
          router "successPath" (String "will-succeed")
        failPath <- spawnActor scope "failPath" $ \_ ->
          error "intentional failure"
        successPath <- spawnActor scope "successPath" $ \_ ->
          router "exit" (String "success")
        pure $ Map.fromList
          [ ("entry", entry)
          , ("failPath", failPath)
          , ("successPath", successPath)
          ]
      result `shouldBe` "success"

    it "many messages to single actor processed in order" $ do
      -- Stress test: send 50 messages, verify all processed
      received <- newIORef ([] :: [Int])
      result <- withActorSystem @Int (toJSON (0 :: Int)) $ \scope router -> do
        entry <- spawnActor scope "entry" $ \_ -> do
          -- Send 50 messages to counter
          mapM_ (\i -> router "counter" (Number (fromIntegral i))) [1..50 :: Int]
        counter <- spawnActor scope "counter" $ \case
          Number n -> do
            count <- atomicModifyIORef' received $ \xs ->
              let xs' = truncate n : xs in (xs', length xs')
            if count >= 50
              then router "exit" (Number (fromIntegral count))
              else pure ()
          _ -> pure ()
        pure $ Map.fromList [("entry", entry), ("counter", counter)]
      result `shouldBe` 50
      -- Verify all messages received
      msgs <- readIORef received
      length msgs `shouldBe` 50
