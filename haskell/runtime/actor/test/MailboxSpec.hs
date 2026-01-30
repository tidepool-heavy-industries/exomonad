module MailboxSpec (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, replicateM)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V
import ExoMonad.Actor.Mailbox
import Test.Hspec

spec :: Spec
spec = describe "Mailbox" $ do
  describe "basic operations" $ do
    it "can send and receive a message" $ do
      mailbox <- newMailboxIO @Value
      send mailbox (String "hello")
      msg <- receive mailbox
      msg `shouldBe` String "hello"

    it "preserves message order (FIFO)" $ do
      mailbox <- newMailboxIO @Value
      send mailbox (String "first")
      send mailbox (String "second")
      send mailbox (String "third")
      m1 <- receive mailbox
      m2 <- receive mailbox
      m3 <- receive mailbox
      [m1, m2, m3] `shouldBe` [String "first", String "second", String "third"]

  describe "concurrent access" $ do
    it "handles concurrent sends" $ do
      mailbox <- newMailboxIO @Value
      done <- newEmptyMVar
      -- Send from multiple threads
      _ <- forkIO $ send mailbox (Number 1) >> putMVar done ()
      _ <- forkIO $ send mailbox (Number 2) >> putMVar done ()
      _ <- forkIO $ send mailbox (Number 3) >> putMVar done ()
      -- Wait for all sends
      takeMVar done
      takeMVar done
      takeMVar done
      -- All messages should arrive
      m1 <- receive mailbox
      m2 <- receive mailbox
      m3 <- receive mailbox
      -- Order may vary due to concurrency, but all should be present
      [m1, m2, m3] `shouldMatchList` [Number 1, Number 2, Number 3]

    it "receive blocks until message available" $ do
      mailbox <- newMailboxIO @Value
      result <- newEmptyMVar
      -- Start receiver (will block)
      _ <- forkIO $ do
        msg <- receive mailbox
        putMVar result msg
      -- Small delay to ensure receiver is blocked
      threadDelay 10000 -- 10ms
      -- Send message
      send mailbox (String "unblock")
      -- Receiver should now complete
      msg <- takeMVar result
      msg `shouldBe` String "unblock"

  describe "JSON Value types" $ do
    it "handles all JSON Value types" $ do
      mailbox <- newMailboxIO @Value
      -- Send all types
      send mailbox Null
      send mailbox (Bool True)
      send mailbox (Bool False)
      send mailbox (Number 42)
      send mailbox (Number 3.14)
      send mailbox (String "text")
      send mailbox (Array $ V.fromList [Number 1, Number 2])
      send mailbox (Object $ KM.fromList [("key", String "value")])
      -- Receive and verify
      r1 <- receive mailbox
      r2 <- receive mailbox
      r3 <- receive mailbox
      r4 <- receive mailbox
      r5 <- receive mailbox
      r6 <- receive mailbox
      r7 <- receive mailbox
      r8 <- receive mailbox
      r1 `shouldBe` Null
      r2 `shouldBe` Bool True
      r3 `shouldBe` Bool False
      r4 `shouldBe` Number 42
      r5 `shouldBe` Number 3.14
      r6 `shouldBe` String "text"
      r7 `shouldBe` Array (V.fromList [Number 1, Number 2])
      r8 `shouldBe` Object (KM.fromList [("key", String "value")])

    it "handles nested objects" $ do
      mailbox <- newMailboxIO @Value
      let nested =
            object
              [ "level1"
                  .= object
                    [ "level2"
                        .= object
                          [ "value" .= (42 :: Int)
                          ]
                    ]
              ]
      send mailbox nested
      result <- receive mailbox
      result `shouldBe` nested

  describe "stress" $ do
    it "handles 100 queued messages" $ do
      mailbox <- newMailboxIO @Value
      -- Send 100 messages
      forM_ [1 .. 100 :: Int] $ \i ->
        send mailbox (Number (fromIntegral i))
      -- Receive all 100
      msgs <- replicateM 100 (receive mailbox)
      length msgs `shouldBe` 100
      -- Verify order preserved
      msgs !! 0 `shouldBe` Number 1
      msgs !! 99 `shouldBe` Number 100

    it "handles high concurrency sends" $ do
      mailbox <- newMailboxIO @Value
      dones <- replicateM 20 newEmptyMVar
      -- 20 threads each sending 5 messages
      forM_ (zip [1 .. 20 :: Int] dones) $ \(tid, done) ->
        forkIO $ do
          forM_ [1 .. 5 :: Int] $ \i ->
            send mailbox (Number (fromIntegral $ tid * 100 + i))
          putMVar done ()
      -- Wait for all senders
      mapM_ takeMVar dones
      -- Receive all 100 messages
      msgs <- replicateM 100 (receive mailbox)
      length msgs `shouldBe` 100
