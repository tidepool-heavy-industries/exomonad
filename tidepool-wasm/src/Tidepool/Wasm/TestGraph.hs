{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Test graph for proving WASM integration works.
--
-- A minimal graph that:
-- 1. Takes an Int
-- 2. Logs a message
-- 3. Returns the Int + 1
--
-- This proves the effect yield/resume cycle works over the WASM boundary.
module Tidepool.Wasm.TestGraph
  ( TestGraph(..)
  , testHandlers
  ) where

import Data.Proxy (Proxy(..))
import Data.Text qualified as T
import GHC.Generics (Generic)

import Effectful (Eff)

import Tidepool.Effect (Log, logInfo)
import Tidepool.Graph.Types (type (:@), Needs, UsesEffects)
import Tidepool.Graph.Generic
  ( GraphMode(..)
  , AsHandler
  , Entry
  , Exit
  , LogicNode
  )
import Tidepool.Graph.Goto (Goto, goto)


-- | Minimal test graph: Int in, Int+1 out, logs during computation.
--
-- Structure:
--   Entry(Int) → compute → Exit(Int)
--
-- The compute node uses Log and Goto effects:
-- - Log: yields to TypeScript for handling
-- - Goto Exit: terminates with result
data TestGraph mode = TestGraph
  { entry   :: mode :- Entry Int
  , compute :: mode :- LogicNode :@ Needs '[Int] :@ UsesEffects '[Log, Goto Exit Int]
  , exit    :: mode :- Exit Int
  }
  deriving Generic


-- | Handler implementation for TestGraph.
--
-- The compute handler:
-- 1. Logs "Computing: <n>" (yields Log effect to TypeScript)
-- 2. Gotos Exit with n+1 (terminates graph)
testHandlers :: TestGraph (AsHandler es)
testHandlers = TestGraph
  { entry   = Proxy @Int
  , compute = computeHandler
  , exit    = Proxy @Int
  }
  where
    computeHandler :: Int -> Eff '[Log, Goto Exit Int] ()
    computeHandler n = do
      logInfo ("Computing: " <> T.pack (show n))
      goto @Exit (n + 1)
