{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import Data.Either (isLeft, isRight)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Tidepool.Control.Hook.CircuitBreaker

main :: IO ()
main = defaultMain spec

spec :: TestTree
spec = testGroup "Circuit Breaker Tests"
  [ testCase "New session initializes and acquires lock" $ do
      cbMap <- initCircuitBreaker
      res <- withCircuitBreaker cbMap "session1" (pure "ok")
      res @?= Right "ok"

  , testCase "Lock prevents concurrent execution" $ do
      cbMap <- initCircuitBreaker
      -- Start a long-running action
      _ <- withCircuitBreaker cbMap "session1" $ do
        -- While this is running, try another one
        res <- withCircuitBreaker cbMap "session1" (pure "fail")
        res @?= Left "Stop hook already running"
        pure "first-ok"
      pure ()

  , testCase "Global limit is enforced" $ do
      cbMap <- initCircuitBreaker
      -- Run 15 times (incrementing global stops manually to simulate retries)
      replicateM_ 15 $ do
        _ <- withCircuitBreaker cbMap "session1" (pure ())
        now <- getCurrentTime
        incrementStage cbMap "session1" "test-stage" now

      -- 16th time should fail
      res <- withCircuitBreaker cbMap "session1" (pure ())
      case res of
        Left err -> assertBool "Should be global limit error" ("Global stop limit reached" `T.isPrefixOf` err)
        Right _ -> assertFailure "Should have failed with global limit"

  , testCase "Stage limit is enforced" $ do
      cbMap <- initCircuitBreaker
      -- Run 5 times for a specific stage
      replicateM_ 5 $ do
        _ <- withCircuitBreaker cbMap "session1" (pure ())
        now <- getCurrentTime
        incrementStage cbMap "session1" "fail-stage" now

      -- 6th time should fail
      res <- withCircuitBreaker cbMap "session1" (pure ())
      case res of
        Left err -> assertBool "Should be stage limit error" ("Stage limit reached" `T.isPrefixOf` err)
        Right _ -> assertFailure "Should have failed with stage limit"

  , testCase "Staleness allows reset" $ do
      cbMap <- initCircuitBreaker
      -- 1. Create a "stale" state
      let sessionId = "stale-session"
      now <- getCurrentTime
      let staleTime = addUTCTime (-600) now -- 10 minutes ago
      let staleState = CircuitBreakerState
            { cbSessionId = sessionId
            , cbGlobalStops = 0
            , cbStageRetries = Map.empty
            , cbLastStopTime = staleTime
            , cbStopHookActive = True -- Locked
            }
      
      atomically $ modifyTVar' cbMap (Map.insert sessionId staleState)
      
      -- 2. Try to run - should succeed because it's stale
      res <- withCircuitBreaker cbMap sessionId (pure "recovered")
      res @?= Right "recovered"
  ]