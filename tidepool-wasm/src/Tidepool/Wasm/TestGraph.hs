{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Test graph for proving WASM integration works.
--
-- A minimal graph that:
-- 1. Takes an Int
-- 2. Logs a message (yields to TypeScript)
-- 3. Returns the Int + 1
--
-- This proves the effect yield/resume cycle works over the WASM boundary.
module Tidepool.Wasm.TestGraph
  ( -- * Graph Type
    TestGraph(..)
    -- * WASM Handler
  , computeHandlerWasm
  ) where

import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Needs, UsesEffects, Exit)
import Tidepool.Graph.Generic (GraphMode(..), type (:-))
import qualified Tidepool.Graph.Generic as G (Entry, Exit, LogicNode)
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit)

import Tidepool.Wasm.Effect (WasmM, logInfo)


-- | Minimal test graph: Int in, Int+1 out.
--
-- Structure:
--   Entry(Int) → compute → Exit(Int)
--
-- The compute node logs a message (yielding to TypeScript) then exits with n+1.
data TestGraph mode = TestGraph
  { entry   :: mode :- G.Entry Int
  , compute :: mode :- G.LogicNode :@ Needs '[Int] :@ UsesEffects '[Goto Exit Int]
  , exit    :: mode :- G.Exit Int
  }
  deriving Generic


-- | WASM handler for the compute node.
--
-- This handler:
-- 1. Logs "Computing: n" (yields to TypeScript)
-- 2. Returns gotoExit (n+1)
--
-- The log effect causes execution to suspend. TypeScript executes the log,
-- then calls resume. Execution continues and returns the GotoChoice.
computeHandlerWasm :: Int -> WasmM (GotoChoice '[To Exit Int])
computeHandlerWasm n = do
  logInfo $ "Computing: " <> T.pack (show n)
  pure $ gotoExit (n + 1)
