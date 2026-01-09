{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ParallelSpec (spec) where

import Test.Hspec
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Hashable (Hashable(..))
import Data.IORef (newIORef, atomicModifyIORef)
import Data.Text (Text)
import qualified Data.HashSet as HS
import GHC.Generics (Generic)

import Tidepool.Graph.Goto (GotoAll, gotoAll, To)
import Tidepool.Graph.Types (HList(..), CorrelateBy(..), From)
import Tidepool.Parallel.Dispatch (SpawnWorkers(..))
import Tidepool.Parallel.Merge (ExpectedSources(..))
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
