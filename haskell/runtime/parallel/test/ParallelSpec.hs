{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ParallelSpec (spec) where

import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Monad (forM_)
import Data.Aeson (ToJSON(..), FromJSON(..), toJSON)
import Data.Hashable (Hashable(..))
import Data.IORef (newIORef, atomicModifyIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import qualified Data.HashSet as HS
import GHC.Generics (Generic)
import qualified Ki

import Tidepool.Graph.Goto (GotoAll, gotoAll, To)
import Tidepool.Graph.Types (HList(..), CorrelateBy(..), From)
import Tidepool.Parallel.Dispatch
  ( SpawnWorkers(..), dispatchAll, spawnWorker
  , WorkerResult(..), ParallelConfig(..), defaultParallelConfig
  )
import Tidepool.Parallel.Merge
  ( ExpectedSources(..), MergeAccumulator(..)
  , newMergeAccumulator, addResult, checkComplete, getCompletedResults
  )
import Tidepool.Parallel.Retry (withRetry, RetryResult(..), RetryConfig(..), defaultRetryConfig)


-- ════════════════════════════════════════════════════════════════════════════
-- TEST TYPES
-- ════════════════════════════════════════════════════════════════════════════

newtype OrderId = OrderId Text
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

instance Hashable OrderId where
  hashWithSalt salt (OrderId t) = hashWithSalt salt t

data PaymentReq = PaymentReq { prOrderId :: OrderId, prAmount :: Double }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data InventoryReq = InventoryReq { irOrderId :: OrderId, irItems :: [Text] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data PaymentResult = PaymentResult { payOrderId :: OrderId, paySuccess :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data InventoryResult = InventoryResult { invOrderId :: OrderId, invReserved :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- CorrelateBy instances
instance CorrelateBy OrderId PaymentResult where
  correlationKey = (.payOrderId)

instance CorrelateBy OrderId InventoryResult where
  correlationKey = (.invOrderId)


-- ════════════════════════════════════════════════════════════════════════════
-- TESTS
-- ════════════════════════════════════════════════════════════════════════════

spec :: Spec
spec = describe "Parallel Execution" $ do

  describe "GotoAll" $ do

    it "can construct GotoAll with HList" $ do
      let payReq = PaymentReq (OrderId "order-1") 100.0
          invReq = InventoryReq (OrderId "order-1") ["item-a", "item-b"]
          gotoAllValue :: GotoAll '[To "payment" PaymentReq, To "inventory" InventoryReq]
          gotoAllValue = gotoAll (payReq ::: invReq ::: HNil)
      -- Just verify it type-checks (use seq to force evaluation)
      gotoAllValue `seq` True `shouldBe` True


  describe "SpawnWorkers" $ do

    it "extracts targets from GotoAll" $ do
      let payReq = PaymentReq (OrderId "order-1") 100.0
          invReq = InventoryReq (OrderId "order-1") ["item-a"]
          gotoAllValue :: GotoAll '[To "payment" PaymentReq, To "inventory" InventoryReq]
          gotoAllValue = gotoAll (payReq ::: invReq ::: HNil)
          targets = extractTargets gotoAllValue
      length targets `shouldBe` 2
      map fst targets `shouldBe` ["payment", "inventory"]


  describe "ExpectedSources" $ do

    it "extracts source names from From markers" $ do
      let sources = expectedSources @'[From "payment" PaymentResult, From "inventory" InventoryResult]
      sources `shouldBe` HS.fromList ["payment", "inventory"]


  describe "Retry" $ do

    it "succeeds on first attempt" $ do
      let config = defaultRetryConfig
      result <- withRetry config $ pure (42 :: Int)
      result `shouldBe` RetrySuccess 42 1

    it "retries on failure and eventually succeeds" $ do
      attemptRef <- newIORef (0 :: Int)
      let config = defaultRetryConfig { rcMaxAttempts = 3, rcInitialDelayMicros = 1000 }
      result <- withRetry config $ do
        attempt <- atomicModifyIORef attemptRef $ \n -> (n + 1, n + 1)
        if attempt < 3
          then error "Simulated failure"
          else pure attempt
      result `shouldBe` RetrySuccess 3 3

    it "fails after max attempts" $ do
      let config = defaultRetryConfig { rcMaxAttempts = 2, rcInitialDelayMicros = 1000 }
      result <- withRetry config $ error "Always fails" :: IO (RetryResult Int)
      case result of
        RetryFailure errors -> length errors `shouldBe` 2
        RetrySuccess _ _ -> expectationFailure "Should have failed"


  describe "MergeAccumulator" $ do

    it "creates accumulator with expected sources" $ do
      acc <- newMergeAccumulator @'[From "payment" PaymentResult, From "inventory" InventoryResult] @OrderId
      acc.maExpectedSources `shouldBe` HS.fromList ["payment", "inventory"]

    it "returns Nothing when incomplete" $ do
      acc <- newMergeAccumulator @'[From "a" PaymentResult, From "b" InventoryResult] @OrderId
      result <- addResult acc (OrderId "key1") "a" (toJSON $ PaymentResult (OrderId "key1") True)
      result `shouldBe` Nothing

    it "returns Just when all sources complete" $ do
      acc <- newMergeAccumulator @'[From "a" PaymentResult, From "b" InventoryResult] @OrderId
      _ <- addResult acc (OrderId "key1") "a" (toJSON $ PaymentResult (OrderId "key1") True)
      result <- addResult acc (OrderId "key1") "b" (toJSON $ InventoryResult (OrderId "key1") True)
      result `shouldBe` Just (HS.fromList ["a", "b"])

    it "tracks multiple keys independently" $ do
      acc <- newMergeAccumulator @'[From "a" PaymentResult, From "b" InventoryResult] @OrderId
      _ <- addResult acc (OrderId "key1") "a" (toJSON $ PaymentResult (OrderId "key1") True)
      _ <- addResult acc (OrderId "key2") "a" (toJSON $ PaymentResult (OrderId "key2") True)
      checkComplete acc (OrderId "key1") >>= (`shouldBe` False)
      checkComplete acc (OrderId "key2") >>= (`shouldBe` False)

    it "completes each key independently" $ do
      acc <- newMergeAccumulator @'[From "a" PaymentResult, From "b" InventoryResult] @OrderId
      -- Complete key1
      _ <- addResult acc (OrderId "key1") "a" (toJSON $ PaymentResult (OrderId "key1") True)
      _ <- addResult acc (OrderId "key1") "b" (toJSON $ InventoryResult (OrderId "key1") True)
      -- Partial key2
      _ <- addResult acc (OrderId "key2") "a" (toJSON $ PaymentResult (OrderId "key2") True)
      -- Check
      checkComplete acc (OrderId "key1") >>= (`shouldBe` True)
      checkComplete acc (OrderId "key2") >>= (`shouldBe` False)


  describe "dispatchAll" $ do

    it "spawns workers and collects results" $ do
      Ki.scoped $ \scope -> do
        let mockDispatch target payload = pure $ WorkerResult target payload
            targets = [("worker1", toJSON ("task1" :: Text)), ("worker2", toJSON ("task2" :: Text))]

        results <- dispatchAll defaultParallelConfig scope mockDispatch targets
        length results `shouldBe` 2
        map (.wrSource) results `shouldMatchList` ["worker1", "worker2"]

    it "runs workers concurrently" $ do
      startTime <- getCurrentTime
      Ki.scoped $ \scope -> do
        let slowWorker target payload = do
              threadDelay 50000  -- 50ms
              pure $ WorkerResult target payload
            targets = [("w1", toJSON ()), ("w2", toJSON ()), ("w3", toJSON ())]

        _ <- dispatchAll defaultParallelConfig scope slowWorker targets
        pure ()
      endTime <- getCurrentTime
      let elapsed = diffUTCTime endTime startTime
      -- Should complete in ~50ms (parallel), not ~150ms (sequential)
      elapsed `shouldSatisfy` (< 0.12)  -- Allow some slack for test overhead


  describe "spawnWorker" $ do

    it "retries failing workers and eventually succeeds" $ do
      attemptRef <- newIORef (0 :: Int)
      Ki.scoped $ \scope -> do
        let config = defaultParallelConfig { pcRetry = defaultRetryConfig { rcMaxAttempts = 3, rcInitialDelayMicros = 1000 } }
            failingWorker target payload = do
              attempt <- atomicModifyIORef attemptRef (\n -> (n+1, n+1))
              if attempt < 3
                then error "Simulated failure"
                else pure $ WorkerResult target payload

        thread <- spawnWorker config scope failingWorker ("test", toJSON ("payload" :: Text))
        result <- atomically $ Ki.await thread
        result.wrSource `shouldBe` "test"


  describe "Fan-out/Fan-in Integration" $ do

    it "fans out to workers and collects in accumulator" $ do
      -- Create accumulator
      acc <- newMergeAccumulator @'[From "payment" PaymentResult, From "inventory" InventoryResult] @OrderId

      -- Simulate fan-out with GotoAll
      let orderId = OrderId "order-123"
          payReq = PaymentReq orderId 100.0
          invReq = InventoryReq orderId ["item"]
          gotoAllVal :: GotoAll '[To "payment" PaymentReq, To "inventory" InventoryReq]
          gotoAllVal = gotoAll (payReq ::: invReq ::: HNil)

      -- Extract and dispatch
      Ki.scoped $ \scope -> do
        let targets = extractTargets gotoAllVal
            mockWorker target _payload = do
              -- Simulate work, return typed result
              let result = case target of
                    "payment" -> toJSON $ PaymentResult orderId True
                    "inventory" -> toJSON $ InventoryResult orderId True
                    _ -> error $ "Unknown target: " <> show target
              pure $ WorkerResult target result

        results <- dispatchAll defaultParallelConfig scope mockWorker targets

        -- Add results to accumulator
        forM_ results $ \wr -> do
          addResult acc orderId wr.wrSource wr.wrPayload

        -- Check completion
        complete <- checkComplete acc orderId
        complete `shouldBe` True


  describe "ExtractMergeResults" $ do

    it "extracts typed HList from complete results" $ do
      acc <- newMergeAccumulator @'[From "payment" PaymentResult, From "inventory" InventoryResult] @OrderId
      let orderId = OrderId "order-1"
      _ <- addResult acc orderId "payment" (toJSON $ PaymentResult orderId True)
      _ <- addResult acc orderId "inventory" (toJSON $ InventoryResult orderId True)

      result <- getCompletedResults @'[From "payment" PaymentResult, From "inventory" InventoryResult] acc orderId
      case result of
        Nothing -> expectationFailure "Expected completed results"
        Just (Left err) -> expectationFailure $ "Parse error: " <> show err
        Just (Right (payResult ::: invResult ::: HNil)) -> do
          payResult.payOrderId `shouldBe` orderId
          payResult.paySuccess `shouldBe` True
          invResult.invOrderId `shouldBe` orderId
          invResult.invReserved `shouldBe` True

    it "returns Nothing for incomplete results" $ do
      acc <- newMergeAccumulator @'[From "a" PaymentResult, From "b" InventoryResult] @OrderId
      let orderId = OrderId "order-1"
      _ <- addResult acc orderId "a" (toJSON $ PaymentResult orderId True)
      -- Don't add "b"

      result <- getCompletedResults @'[From "a" PaymentResult, From "b" InventoryResult] acc orderId
      case result of
        Just (Left err) -> T.unpack err `shouldContain` "Missing source: b"
        Just (Right _) -> expectationFailure "Should have failed with missing source"
        Nothing -> pure ()  -- Also acceptable - key might not exist

    it "returns Left for malformed JSON" $ do
      acc <- newMergeAccumulator @'[From "a" PaymentResult] @OrderId
      let orderId = OrderId "order-1"
      -- Add wrong type of JSON
      _ <- addResult acc orderId "a" (toJSON ("not a payment result" :: Text))

      result <- getCompletedResults @'[From "a" PaymentResult] acc orderId
      case result of
        Just (Left err) -> T.unpack err `shouldContain` "Failed to parse a"
        Just (Right _) -> expectationFailure "Should have failed parsing"
        Nothing -> expectationFailure "Expected result to exist"


  describe "Full Fan-out/Fan-in with Typed Extraction" $ do

    it "completes full cycle: GotoAll -> dispatch -> accumulate -> extract typed HList" $ do
      -- Create accumulator
      acc <- newMergeAccumulator @'[From "payment" PaymentResult, From "inventory" InventoryResult] @OrderId

      -- Simulate fan-out with GotoAll
      let orderId = OrderId "order-456"
          payReq = PaymentReq orderId 250.0
          invReq = InventoryReq orderId ["widget", "gadget"]
          gotoAllVal :: GotoAll '[To "payment" PaymentReq, To "inventory" InventoryReq]
          gotoAllVal = gotoAll (payReq ::: invReq ::: HNil)

      -- Extract and dispatch
      Ki.scoped $ \scope -> do
        let targets = extractTargets gotoAllVal
            mockWorker target _payload = do
              let result = case target of
                    "payment" -> toJSON $ PaymentResult orderId True
                    "inventory" -> toJSON $ InventoryResult orderId True
                    _ -> error $ "Unknown target: " <> show target
              pure $ WorkerResult target result

        results <- dispatchAll defaultParallelConfig scope mockWorker targets

        -- Add results to accumulator
        forM_ results $ \wr -> do
          _ <- addResult acc orderId wr.wrSource wr.wrPayload
          pure ()

      -- Extract typed results
      result <- getCompletedResults @'[From "payment" PaymentResult, From "inventory" InventoryResult] acc orderId
      case result of
        Nothing -> expectationFailure "Expected completed results"
        Just (Left err) -> expectationFailure $ "Parse error: " <> show err
        Just (Right (payResult ::: invResult ::: HNil)) -> do
          -- Verify typed access works correctly
          payResult.payOrderId `shouldBe` orderId
          payResult.paySuccess `shouldBe` True
          invResult.invOrderId `shouldBe` orderId
          invResult.invReserved `shouldBe` True
