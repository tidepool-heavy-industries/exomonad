{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
    -- * WASM Handlers
  , computeHandlerWasm
  , computeMultiEffectWasm
  ) where

import qualified Data.Text as T

import Tidepool.Wasm.Prelude


-- | Minimal test graph: Int in, Int+1 out.
--
-- Structure:
--   Entry(Int) → compute → Exit(Int)
--
-- The compute node logs a message (yielding to TypeScript) then exits with n+1.
data TestGraph mode = TestGraph
  { entry   :: mode :- EntryNode Int
  , compute :: mode :- LogicNode :@ Input Int :@ UsesEffects '[Goto ExitTarget Int]
  , exit    :: mode :- ExitNode Int
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
computeHandlerWasm :: Int -> WasmM (GotoChoice '[To ExitTarget Int])
computeHandlerWasm n = do
  logInfo $ "Computing: " <> T.pack (show n)
  pure $ gotoExit (n + 1)


-- | Multi-effect WASM handler for E2E testing.
--
-- This handler yields 3 Log effects before completing, proving that
-- the yield/resume cycle works correctly across multiple steps:
--
-- 1. Logs "Step 1: received n" (yields)
-- 2. Logs "Step 2: computing" (yields)
-- 3. Logs "Step 3: returning n+1" (yields)
-- 4. Returns gotoExit (n+1)
--
-- This tests the full multi-step flow: init -> step -> step -> step -> done
computeMultiEffectWasm :: Int -> WasmM (GotoChoice '[To ExitTarget Int])
computeMultiEffectWasm n = do
  logInfo $ "Step 1: received " <> T.pack (show n)
  logInfo "Step 2: computing"
  logInfo $ "Step 3: returning " <> T.pack (show (n + 1))
  pure $ gotoExit (n + 1)
