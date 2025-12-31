{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Graph runner with explicit continuation encoding.
--
-- This module implements the yield/resume pattern for WASM execution.
-- Since GHC WASM cannot block mid-computation (WouldBlockException),
-- we encode "what to do next" as explicit data structures.
--
-- For TestGraph, this is a simple 2-state machine:
--   1. Initialize: emit Log effect, store continuation
--   2. Step: after Log completes, emit final result
--
-- For real graphs, the continuation would encode current node + accumulated context.
module Tidepool.Wasm.Runner
  ( -- * Continuation Types
    GraphContinuation(..)
  , GraphYield(..)
  , RunnerState(..)

    -- * Graph Execution
  , initializeGraph
  , stepGraph
  ) where

import Data.Aeson (ToJSON, FromJSON, Value, toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Wasm.WireTypes (SerializableEffect(..), EffectResult(..), ExecutionPhase(..))


-- | Explicit continuation - what to do after effect completes.
--
-- For TestGraph: simple 2-state machine
-- For real graphs: would encode current node + accumulated context
data GraphContinuation
  = ContAfterLog Int
    -- ^ We've logged "Computing: n", waiting for TypeScript to acknowledge.
    --   On resume, we'll complete with n+1.
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)


-- | Result of running until yield or completion.
data GraphYield
  = YieldEffect SerializableEffect GraphContinuation
    -- ^ Yielding an effect to TypeScript, with continuation for resume
  | YieldComplete Value
    -- ^ Graph execution complete, final result
  | YieldError Text
    -- ^ Graph execution failed
  deriving stock (Show, Eq)


-- | Persistent state across FFI calls.
--
-- Stored in a global IORef in Ffi.hs.
data RunnerState = RunnerState
  { rsContinuation :: GraphContinuation
    -- ^ What to do when step() is called
  , rsPhase :: ExecutionPhase
    -- ^ Current phase for GraphState reporting
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)


-- | Start graph execution, run until first yield.
--
-- For TestGraph: Entry(Int) → compute → Exit(Int)
-- The compute node logs then exits, so we yield the Log effect.
initializeGraph :: Int -> GraphYield
initializeGraph n =
  -- TestGraph: compute handler is \n -> logInfo msg >> gotoExit (n+1)
  -- We run until the logInfo effect, then yield
  YieldEffect
    (EffLogInfo $ "Computing: " <> T.pack (show n))
    (ContAfterLog n)


-- | Resume from continuation with effect result.
--
-- Dispatches based on which continuation we're in and processes the result.
stepGraph :: GraphContinuation -> EffectResult -> GraphYield
stepGraph (ContAfterLog n) (ResSuccess _) =
  -- Log completed successfully, now execute gotoExit(n+1)
  YieldComplete (toJSON (n + 1))
stepGraph (ContAfterLog _) (ResError msg) =
  -- Log failed (unusual, but handle it)
  YieldError $ "Log effect failed: " <> msg
