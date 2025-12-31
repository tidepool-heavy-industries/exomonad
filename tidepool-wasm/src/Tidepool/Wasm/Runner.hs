{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | Graph runner using freer-simple's Coroutine effect.
--
-- This module implements the yield/resume pattern for WASM execution using
-- freer-simple's built-in 'Status' type. Unlike effectful (which is ReaderT IO),
-- freer-simple reifies continuations as data, enabling step-by-step execution.
--
-- = Design
--
-- The runner uses 'runC' to step through a 'WasmM' computation:
--
-- @
-- runC :: Eff (Yield a b ': effs) r -> Eff effs (Status effs a b r)
-- @
--
-- 'Status' is either:
--
-- * @Done result@ - Computation completed
-- * @Continue yielded (resume -> Status)@ - Yielded, here's the continuation
--
-- The continuation is stored in an existential wrapper ('SomeWasmCont') in
-- a global 'IORef', allowing resume across FFI boundaries.
module Tidepool.Wasm.Runner
  ( -- * Running Computations
    initializeWasm
  , stepWasm

    -- * Result Types
  , WasmResult(..)

    -- * Re-exports for convenience
  , WasmM
  , WasmStatus
  ) where

import Control.Monad.Freer (Eff, run)
import Control.Monad.Freer.Coroutine (Status(..))
import Data.Text (Text)

import Tidepool.Wasm.Effect (WasmM, WasmStatus, runWasmM)
import Tidepool.Wasm.WireTypes (SerializableEffect, EffectResult(..))


-- ════════════════════════════════════════════════════════════════════════════
-- RESULT TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Result of running a WASM computation step.
--
-- This is the high-level result type used by the FFI layer.
data WasmResult a
  = WasmYield SerializableEffect (EffectResult -> WasmResult a)
    -- ^ Yielded an effect. Call the continuation with the result to resume.
  | WasmComplete a
    -- ^ Computation completed with a result.
  | WasmError Text
    -- ^ Computation failed with an error.


-- ════════════════════════════════════════════════════════════════════════════
-- RUNNING COMPUTATIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Initialize a WASM computation and run until the first yield or completion.
--
-- Takes a 'WasmM' computation and returns a 'WasmResult':
--
-- * 'WasmYield' if the computation yielded an effect
-- * 'WasmComplete' if the computation completed
--
-- Example:
--
-- @
-- case initializeWasm (computeHandlerWasm 5) of
--   WasmYield eff resume ->
--     -- eff is EffLogInfo "Computing: 5"
--     -- Call resume (ResSuccess Nothing) to continue
--     case resume (ResSuccess Nothing) of
--       WasmComplete choice -> handleChoice choice
--       ...
--   WasmComplete choice ->
--     handleChoice choice
-- @
initializeWasm :: WasmM a -> WasmResult a
initializeWasm wasm = statusToResult (runWasmM wasm)

-- | Step a WASM computation by providing an effect result.
--
-- This is used internally by the continuation in 'WasmResult'.
-- External callers should use the continuation returned by 'initializeWasm'.
stepWasm :: (EffectResult -> Eff '[] (WasmStatus a)) -> EffectResult -> WasmResult a
stepWasm k result = statusToResult (run (k result))


-- ════════════════════════════════════════════════════════════════════════════
-- INTERNAL HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Convert freer-simple's 'Status' to our 'WasmResult'.
statusToResult :: WasmStatus a -> WasmResult a
statusToResult (Done a) = WasmComplete a
statusToResult (Continue eff k) = WasmYield eff (stepWasm k)
