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
import ExoMonad.Control.Hook.CircuitBreaker

main :: IO ()
main = defaultMain spec

defaultConfig :: CircuitBreakerConfig
defaultConfig = CircuitBreakerConfig 15 5 300

spec :: TestTree
spec = testGroup "Circuit Breaker Tests"
  [ testGroup "Config Validation"
    [ testCase "Valid config" $ do
        mkCircuitBreakerConfig 15 5 300 @?= Right (CircuitBreakerConfig 15 5 300)
    , testCase "Invalid globalMax" $ do
        mkCircuitBreakerConfig 0 5 300 @?= Left "globalMax must be >= 1"
    , testCase "Invalid stageMax" $ do
        mkCircuitBreakerConfig 15 0 300 @?= Left "stageMax must be >= 1"
    , testCase "Invalid staleTimeout" $ do
        mkCircuitBreakerConfig 15 5 50 @?= Left "staleTimeout must be >= 60 seconds"
    ]

  , testCase "New session initializes and acquires lock" $ do
      cbMap <- initCircuitBreaker
      now <- getCurrentTime
      res <- withCircuitBreaker cbMap defaultConfig now "session1" (pure "ok")
      res @?= Right "ok"

  , testCase "Lock prevents concurrent execution" $ do
      cbMap <- initCircuitBreaker
      now <- getCurrentTime
      -- Start a long-running action
      _ <- withCircuitBreaker cbMap defaultConfig now "session1" $ do
        -- While this is running, try another one with same time (so not stale)
        res <- withCircuitBreaker cbMap defaultConfig now "session1" (pure "fail")
        res @?= Left "Stop hook already running"
        pure "first-ok"
      pure ()

  , testCase "Global limit is enforced" $ do
      cbMap <- initCircuitBreaker
      now <- getCurrentTime
      -- Run 15 times (incrementing global stops manually to simulate retries)
      replicateM_ 15 $ do
        _ <- withCircuitBreaker cbMap defaultConfig now "session1" (pure ())
        incrementStage cbMap "session1" "test-stage" now

      -- 16th time should fail
      res <- withCircuitBreaker cbMap defaultConfig now "session1" (pure ())
      case res of
        Left err -> assertBool "Should be global limit error" ("Global stop limit reached" `T.isPrefixOf` err)
        Right _ -> assertFailure "Should have failed with global limit"

  , testCase "Stage limit is enforced" $ do
      cbMap <- initCircuitBreaker
      now <- getCurrentTime
      -- Run 5 times for a specific stage
      replicateM_ 5 $ do
        _ <- withCircuitBreaker cbMap defaultConfig now "session1" (pure ())
        incrementStage cbMap "session1" "fail-stage" now

      -- 6th time should fail
      res <- withCircuitBreaker cbMap defaultConfig now "session1" (pure ())
      case res of
        Left err -> assertBool "Should be stage limit error" ("Stage limit reached" `T.isPrefixOf` err)
        Right _ -> assertFailure "Should have failed with stage limit"

  , testCase "Staleness allows reset (Atomic Check)" $ do
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
      res <- withCircuitBreaker cbMap defaultConfig now sessionId (pure "recovered")
      res @?= Right "recovered"
      
      -- 3. Check state is updated
      finalStates <- readTVarIO cbMap
      case Map.lookup sessionId finalStates of
        Nothing -> assertFailure "Session missing"
        Just s -> do
          assertBool "Should be inactive after run" (not (cbStopHookActive s))
          assertBool "Time should be updated" (cbLastStopTime s >= now)

  , testCase "Reset session clears state" $ do
      cbMap <- initCircuitBreaker
      now <- getCurrentTime
      _ <- withCircuitBreaker cbMap defaultConfig now "session1" (pure ())
      
      -- Verify exists
      s1 <- getCircuitBreakerState cbMap "session1"
      case s1 of
        Just _ -> pure ()
        Nothing -> assertFailure "Session should exist"
        
      -- Reset
      resetSession cbMap "session1"
      
      -- Verify gone
      s2 <- getCircuitBreakerState cbMap "session1"
      case s2 of
        Nothing -> pure ()
        Just _ -> assertFailure "Session should be gone"
  ]