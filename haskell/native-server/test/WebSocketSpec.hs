module WebSocketSpec (spec) where

import Control.Concurrent.STM
import Data.Aeson (encode, decode, eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import qualified Network.WebSockets as WS
import Test.Hspec

import ExoMonad.Wire.Types
import ExoMonad.Server.WebSocket

spec :: Spec
spec = do
  describe "WebSocket connection" $ do
    it "accepts WebSocket connections" $ do
      withTestServer defaultTestConfig $ \port -> do
        -- Connect to the server
        connected <- newTVarIO False
        WS.runClient "127.0.0.1" port "/" $ \_conn -> do
          atomically $ writeTVar connected True
          -- Just connect and disconnect
          pure ()
        result <- atomically $ readTVar connected
        result `shouldBe` True

    it "receives initial UIState on connect" $ do
      withTestServer defaultTestConfig $ \port -> do
        receivedState <- newTVarIO Nothing
        WS.runClient "127.0.0.1" port "/" $ \conn -> do
          msg <- WS.receiveData conn
          case decode msg of
            Just state -> atomically $ writeTVar receivedState (Just state)
            Nothing -> pure ()
        result <- atomically $ readTVar receivedState
        case result of
          Just (state :: UIState) -> do
            usGraphNode state `shouldBe` "greeting"
            usThinking state `shouldBe` False
          Nothing -> expectationFailure "Did not receive initial UIState"

  describe "Message handling" $ do
    it "echoes TextAction back as assistant message" $ do
      withTestServer defaultTestConfig $ \port -> do
        responseState <- newTVarIO Nothing
        WS.runClient "127.0.0.1" port "/" $ \conn -> do
          -- Receive initial greeting
          _ <- WS.receiveData conn :: IO LBS.ByteString

          -- Send text action
          let action = TextAction "Hello, server!"
          WS.sendTextData conn (encode action)

          -- Receive response
          msg <- WS.receiveData conn
          case decode msg of
            Just state -> atomically $ writeTVar responseState (Just state)
            Nothing -> pure ()

        result <- atomically $ readTVar responseState
        case result of
          Just (state :: UIState) -> do
            usGraphNode state `shouldBe` "echo"
            -- Should contain echo response
            let msgs = usMessages state
            length msgs `shouldSatisfy` (>= 1)
          Nothing -> expectationFailure "Did not receive response UIState"

    it "handles ButtonAction" $ do
      withTestServer defaultTestConfig $ \port -> do
        responseState <- newTVarIO Nothing
        WS.runClient "127.0.0.1" port "/" $ \conn -> do
          -- Receive initial greeting
          _ <- WS.receiveData conn :: IO LBS.ByteString

          -- Send button action
          let action = ButtonAction "confirm"
          WS.sendTextData conn (encode action)

          -- Receive response
          msg <- WS.receiveData conn
          case decode msg of
            Just state -> atomically $ writeTVar responseState (Just state)
            Nothing -> pure ()

        result <- atomically $ readTVar responseState
        case result of
          Just (state :: UIState) -> do
            usGraphNode state `shouldBe` "button"
          Nothing -> expectationFailure "Did not receive response UIState"

    it "handles PhotoAction" $ do
      withTestServer defaultTestConfig $ \port -> do
        responseState <- newTVarIO Nothing
        WS.runClient "127.0.0.1" port "/" $ \conn -> do
          -- Receive initial greeting
          _ <- WS.receiveData conn :: IO LBS.ByteString

          -- Send photo action
          let action = PhotoAction "base64data" "image/jpeg"
          WS.sendTextData conn (encode action)

          -- Receive response
          msg <- WS.receiveData conn
          case decode msg of
            Just state -> atomically $ writeTVar responseState (Just state)
            Nothing -> pure ()

        result <- atomically $ readTVar responseState
        case result of
          Just (state :: UIState) -> do
            usGraphNode state `shouldBe` "photo"
          Nothing -> expectationFailure "Did not receive response UIState"

  describe "Protocol compliance" $ do
    it "sends valid JSON for UIState" $ do
      withTestServer defaultTestConfig $ \port -> do
        WS.runClient "127.0.0.1" port "/" $ \conn -> do
          msg <- WS.receiveData conn
          case eitherDecode msg of
            Left err -> expectationFailure $ "Invalid JSON: " ++ err
            Right (_ :: UIState) -> pure ()

    it "accepts valid JSON for UserAction" $ do
      withTestServer defaultTestConfig $ \port -> do
        WS.runClient "127.0.0.1" port "/" $ \conn -> do
          -- Receive greeting first
          _ <- WS.receiveData conn :: IO LBS.ByteString

          -- Send valid action - should not throw
          let action = TextAction "test"
          WS.sendTextData conn (encode action)

          -- Should receive response without error
          _ <- WS.receiveData conn :: IO LBS.ByteString
          pure ()
