-- | Retry logic with exponential backoff for parallel workers.
--
-- This module provides configurable retry behavior for failed workers.
-- Supports exponential backoff with configurable max delay.
--
-- Moved from exomonad-parallel for runtime consolidation.
--
-- TODO: Currently included for completeness but not used in MVP.
--       Add retry integration to barrier handlers in future iteration.
module ExoMonad.Actor.Retry
  ( -- * Configuration
    RetryConfig (..),
    defaultRetryConfig,

    -- * Retry Execution
    withRetry,
    RetryResult (..),
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, try)
import Data.Text (Text)
import Data.Text qualified as T

-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for retry behavior.
data RetryConfig = RetryConfig
  { -- | Maximum number of attempts (including initial). Minimum is 1.
    rcMaxAttempts :: Int,
    -- | Initial delay between retries in microseconds
    rcInitialDelayMicros :: Int,
    -- | Multiplier for exponential backoff (e.g., 2.0 doubles delay each retry)
    rcBackoffMultiplier :: Double,
    -- | Maximum delay between retries in microseconds
    rcMaxDelayMicros :: Int
  }
  deriving stock (Show, Eq)

-- | Default retry configuration: 3 attempts with exponential backoff.
--
-- * 3 total attempts (1 initial + 2 retries)
-- * 100ms initial delay
-- * 2x exponential backoff
-- * 5s maximum delay
defaultRetryConfig :: RetryConfig
defaultRetryConfig =
  RetryConfig
    { rcMaxAttempts = 3,
      rcInitialDelayMicros = 100_000, -- 100ms
      rcBackoffMultiplier = 2.0,
      rcMaxDelayMicros = 5_000_000 -- 5s
    }

-- ════════════════════════════════════════════════════════════════════════════
-- RETRY EXECUTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Result of a retry operation.
data RetryResult a
  = -- | Succeeded with the result and the attempt number (1-based)
    RetrySuccess a Int
  | -- | Failed after all attempts with list of (attempt, error message)
    RetryFailure [(Int, Text)]
  deriving stock (Show, Eq, Functor)

-- | Execute an action with retry logic.
--
-- Returns 'RetrySuccess' on first success, or 'RetryFailure' after all
-- attempts are exhausted.
--
-- @
-- result <- withRetry defaultRetryConfig $ do
--   response <- httpGet url
--   parseResponse response
--
-- case result of
--   RetrySuccess value attempt -> putStrLn $ "Succeeded on attempt " <> show attempt
--   RetryFailure errors -> mapM_ print errors
-- @
withRetry :: RetryConfig -> IO a -> IO (RetryResult a)
withRetry config action = go 1 (config.rcInitialDelayMicros) []
  where
    go attempt delay errors
      | attempt > config.rcMaxAttempts =
          pure $ RetryFailure (reverse errors)
      | otherwise = do
          result <- try action
          case result of
            Right a -> pure $ RetrySuccess a attempt
            Left (err :: SomeException) -> do
              let errMsg = T.pack $ displayException err
                  errors' = (attempt, errMsg) : errors

              if attempt >= config.rcMaxAttempts
                then pure $ RetryFailure (reverse errors')
                else do
                  -- Wait before retry
                  threadDelay delay
                  -- Calculate next delay with exponential backoff
                  let nextDelay =
                        min
                          (round $ fromIntegral delay * config.rcBackoffMultiplier)
                          (config.rcMaxDelayMicros)
                  go (attempt + 1) nextDelay errors'
