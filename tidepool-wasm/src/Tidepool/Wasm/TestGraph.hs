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
import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Needs, UsesEffects, Exit)
import Tidepool.Graph.Generic (GraphMode(..), AsHandler)
import qualified Tidepool.Graph.Generic as G (Entry, Exit, LogicNode)
import Tidepool.Graph.Goto (Goto, gotoExit)


-- | Minimal test graph: Int in, Int+1 out.
--
-- Structure:
--   Entry(Int) → compute → Exit(Int)
--
-- The compute node uses Goto to transition to Exit with n+1.
data TestGraph mode = TestGraph
  { entry   :: mode :- G.Entry Int
  , compute :: mode :- G.LogicNode :@ Needs '[Int] :@ UsesEffects '[Goto Exit Int]
  , exit    :: mode :- G.Exit Int
  }
  deriving Generic


-- | Handler implementation for TestGraph.
--
-- The compute handler returns gotoExit with n+1.
testHandlers :: TestGraph (AsHandler es)
testHandlers = TestGraph
  { entry   = Proxy @Int
  , compute = \n -> pure $ gotoExit (n + 1)
  , exit    = Proxy @Int
  }
