-- | Parallel execution backend for tidepool graphs.
--
-- This module provides parallel fan-out/fan-in execution using ki for
-- structured concurrency. Key concepts:
--
-- * 'GotoAll' - Fan-out to multiple targets simultaneously (product type)
-- * 'Merge' - Gather results from parallel workers, grouped by correlation key
-- * 'CorrelateBy' - Typeclass for extracting correlation keys from results
--
-- = Usage Pattern
--
-- @
-- -- 1. Create fan-out with GotoAll
-- let targets = gotoAll (payReq ::: invReq ::: HNil)
--     extractedTargets = extractTargets targets  -- [(Text, Value)]
--
-- -- 2. Dispatch workers in parallel
-- results <- Ki.scoped $ \\scope ->
--   dispatchAll defaultParallelConfig scope workerDispatch extractedTargets
--
-- -- 3. Collect results in accumulator
-- acc <- newMergeAccumulator @'[From "payment" PayResult, From "inventory" InvResult]
-- forM_ results $ \\wr -> addResult acc correlationKey wr.wrSource wr.wrPayload
--
-- -- 4. Extract typed results when complete
-- Just (Right (payResult ::: invResult ::: HNil)) <-
--   getCompletedResults @'[From "payment" PayResult, From "inventory" InvResult] acc key
-- @
--
-- = Correlation Keys
--
-- Results are grouped by correlation key, enabling multiple in-flight fan-outs:
--
-- @
-- instance CorrelateBy OrderId PayResult where correlationKey = (.orderId)
-- instance CorrelateBy OrderId InvResult where correlationKey = (.orderId)
-- @
module Tidepool.Parallel
  ( -- * Parallel Execution
    runParallel
  , ParallelConfig(..)
  , defaultParallelConfig

    -- * Re-exports from tidepool-core
  , GotoAll
  , gotoAll
  , Merge
  , From
  , GroupBy
  , CorrelateBy(..)
  , HList(..)
  ) where

import Tidepool.Parallel.Dispatch (runParallel, ParallelConfig(..), defaultParallelConfig)
import Tidepool.Graph.Goto (GotoAll, gotoAll)
import Tidepool.Graph.Types (Merge, From, GroupBy, CorrelateBy(..), HList(..))
