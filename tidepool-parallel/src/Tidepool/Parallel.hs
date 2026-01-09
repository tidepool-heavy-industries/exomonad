-- | Parallel execution backend for tidepool graphs.
--
-- This module provides parallel fan-out/fan-in execution using ki for
-- structured concurrency. Key concepts:
--
-- * 'GotoAll' - Fan-out to multiple targets simultaneously
-- * 'Merge' - Gather results from parallel workers, grouped by correlation key
-- * 'CorrelateBy' - Typeclass for extracting correlation keys from results
--
-- = Example
--
-- @
-- -- Define a graph with parallel processing
-- data OrderGraph mode = OrderGraph
--   { entry   :: mode :- Entry Order
--   , fanout  :: mode :- LogicNode :@ Input Order
--                    :@ UsesEffects '[GotoAllEffect '[To "payment" PayReq, To "inventory" InvReq]]
--   , payment :: mode :- LLMNode :@ Input PayReq :@ Schema PayResult
--                    :@ UsesEffects '[Arrive PayResult]
--   , inventory :: mode :- LLMNode :@ Input InvReq :@ Schema InvResult
--                    :@ UsesEffects '[Arrive InvResult]
--   , gather  :: mode :- LogicNode
--                    :@ Input (Merge '[From "payment" PayResult, From "inventory" InvResult])
--                    :@ GroupBy OrderId
--                    :@ UsesEffects '[Goto Exit OrderResult]
--   , exit    :: mode :- Exit OrderResult
--   }
--
-- -- Correlation instances
-- instance CorrelateBy OrderId PayResult where correlationKey = (.orderId)
-- instance CorrelateBy OrderId InvResult where correlationKey = (.orderId)
--
-- -- Run with parallel execution
-- result <- runParallel handlers (toJSON order)
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
