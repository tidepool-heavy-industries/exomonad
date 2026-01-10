{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

-- | Parallel dispatch for ForkNode fan-out.
--
-- This module provides the runtime execution of parallel worker dispatch.
-- When a ForkNode handler returns an HList of payloads, workers are spawned
-- concurrently using ki, and results are collected at the BarrierNode.
--
-- Moved from tidepool-parallel for runtime consolidation.
module Tidepool.Actor.Dispatch
  ( -- * Configuration
    ParallelConfig(..)
  , defaultParallelConfig

    -- * Execution
  , dispatchAll

    -- * Worker Management
  , WorkerResult(..)
  , spawnWorker

    -- * Type-level extraction
  , SpawnWorkers(..)
  ) where

import Control.Concurrent.STM (atomically)
import Data.Aeson (Value, ToJSON(..))
import Data.Kind (Type, Constraint)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy (Proxy(..))
import qualified Ki

import Tidepool.Graph.Goto (To)
import Tidepool.Graph.Goto.Internal (GotoAll(..))
import Tidepool.Graph.Types (HList(..))
import Tidepool.Actor.Retry (RetryConfig(..), defaultRetryConfig, withRetry, RetryResult(..))


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for parallel execution.
data ParallelConfig = ParallelConfig
  { pcRetry :: RetryConfig
    -- ^ Retry configuration for failed workers
  , pcMaxConcurrency :: Maybe Int
    -- ^ Maximum number of concurrent workers (Nothing = unlimited)
  }

-- | Default parallel configuration.
defaultParallelConfig :: ParallelConfig
defaultParallelConfig = ParallelConfig
  { pcRetry = defaultRetryConfig
  , pcMaxConcurrency = Nothing
  }


-- ════════════════════════════════════════════════════════════════════════════
-- WORKER TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Result from a worker, tagged with its source.
data WorkerResult = WorkerResult
  { wrSource :: Text
    -- ^ Source node name
  , wrPayload :: Value
    -- ^ JSON-serialized result
  }
  deriving stock (Show, Eq)


-- ════════════════════════════════════════════════════════════════════════════
-- DISPATCH ALL
-- ════════════════════════════════════════════════════════════════════════════

-- | Dispatch to all targets in a ForkNode concurrently.
--
-- Spawns one worker per target, waits for all to complete, returns results.
-- Uses ki for structured concurrency (all workers cleaned up on scope exit).
--
-- @
-- results <- Ki.scoped $ \scope ->
--   dispatchAll defaultParallelConfig scope workerDispatch
--     [("hTests", testsPayload), ("hImpl", implPayload)]
-- @
dispatchAll
  :: ParallelConfig
  -> Ki.Scope
  -> (Text -> Value -> IO WorkerResult)  -- ^ Single target dispatcher
  -> [(Text, Value)]                      -- ^ Targets with payloads
  -> IO [WorkerResult]
dispatchAll config scope dispatch targets = do
  -- Spawn all workers concurrently
  threads <- mapM (spawnWorker config scope dispatch) targets
  -- Wait for all results (Ki.await returns STM, so wrap in atomically)
  mapM (atomically . Ki.await) threads

-- | Spawn a single worker with retry logic.
spawnWorker
  :: ParallelConfig
  -> Ki.Scope
  -> (Text -> Value -> IO WorkerResult)
  -> (Text, Value)
  -> IO (Ki.Thread WorkerResult)
spawnWorker config scope dispatch (target, payload) =
  Ki.fork scope $ runWithRetryOrFail (config.pcRetry) $ dispatch target payload


-- ════════════════════════════════════════════════════════════════════════════
-- SPAWN WORKERS TYPECLASS
-- ════════════════════════════════════════════════════════════════════════════

-- | Typeclass for spawning workers from a GotoAll.
--
-- Extracts target names and payloads from the type-level list, serializes
-- payloads to JSON, and returns (target, payload) pairs for dispatch.
--
-- @
-- let targets = gotoAll (payReq ::: invReq ::: HNil)
--     extracted = extractTargets targets  -- [(Text, Value)]
-- @
type SpawnWorkers :: [Type] -> Constraint
class SpawnWorkers targets where
  -- | Extract (targetName, jsonPayload) pairs from a GotoAll.
  extractTargets :: GotoAll targets -> [(Text, Value)]

-- | Base case: empty target list.
instance SpawnWorkers '[] where
  extractTargets (GotoAll HNil) = []

-- | Recursive case: named target.
instance
  ( KnownSymbol name
  , ToJSON payload
  , SpawnWorkers rest
  ) => SpawnWorkers (To name payload ': rest) where
  extractTargets (GotoAll (payload ::: restPayloads)) =
    let targetName = T.pack (symbolVal (Proxy @name))
        jsonPayload = toJSON payload
        restTargets = extractTargets @rest (GotoAll restPayloads)
    in (targetName, jsonPayload) : restTargets


-- ════════════════════════════════════════════════════════════════════════════
-- HELPER FUNCTIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Run an action with retry, failing if all attempts exhausted.
--
-- TODO: Better error handling for MVP - currently crashes on failure.
--       Consider returning Either and letting barrier handle failures.
runWithRetryOrFail :: RetryConfig -> IO a -> IO a
runWithRetryOrFail config action = do
  result <- withRetry config action
  case result of
    RetrySuccess a _ -> pure a
    RetryFailure errs -> error $ "All retry attempts failed: " <> show errs
