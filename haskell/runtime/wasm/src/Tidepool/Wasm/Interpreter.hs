-- | Interpreters for running tidepool-core effects in WASM.
--
-- This module re-exports all effect interpreters that convert tidepool-core
-- effects (@LLM@, @Log@, @State@) into WASM-compatible @Yield@ effects.
--
-- = Usage
--
-- Compose interpreters to run handlers that use tidepool-core effects:
--
-- @
-- import Tidepool.Wasm.Interpreter
--
-- -- Handler using standard tidepool effects
-- myHandler :: (Member LLM effs, Member Log effs, Member (State MyState) effs)
--           => Input -> Eff effs Output
-- myHandler input = do
--   logInfo "Starting processing"
--   result <- runTurn systemPrompt content schema tools
--   modify (\s -> s { lastResult = result })
--   pure (processResult result)
--
-- -- Run in WASM context by composing interpreters
-- runInWasm :: Input -> Eff '[Yield SerializableEffect EffectResult] Output
-- runInWasm input =
--     runStateAsYield initialState
--   . runLogAsYield
--   . runLLMAsYield
--   $ myHandler input
-- @
--
-- = Interpreter Ordering
--
-- The interpreters should typically be composed in this order (outermost first):
--
-- 1. @runStateAsYield@ - State effects
-- 2. @runLogAsYield@ - Logging effects
-- 3. @runLLMAsYield@ - LLM effects (innermost)
--
-- This ensures that LLM handlers can use Log and State effects.
module Tidepool.Wasm.Interpreter
  ( -- * LLM Interpreter
    runLLMAsYield

    -- * Log Interpreter
  , runLogAsYield

    -- * State Interpreter
  , runStateAsYield
  , runStateAsYieldWith
  ) where

import Tidepool.Wasm.Interpreter.LLM (runLLMAsYield)
import Tidepool.Wasm.Interpreter.Log (runLogAsYield)
import Tidepool.Wasm.Interpreter.State (runStateAsYield, runStateAsYieldWith)
