-- | Interpreters for running exomonad-core effects in WASM.
--
-- This module re-exports all effect interpreters that convert exomonad-core
-- effects (@LLM@, @Log@, @State@) into WASM-compatible @Yield@ effects.
--
-- = Usage
--
-- Compose interpreters to run handlers that use exomonad-core effects:
--
-- @
-- import ExoMonad.Wasm.Interpreter
--
-- -- Handler using standard exomonad effects
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
module ExoMonad.Wasm.Interpreter
  ( -- * LLM Interpreter
    runLLMAsYield,

    -- * Log Interpreter
    runLogAsYield,

    -- * State Interpreter
    runStateAsYield,
    runStateAsYieldWith,
  )
where

import ExoMonad.Wasm.Interpreter.LLM (runLLMAsYield)
import ExoMonad.Wasm.Interpreter.Log (runLogAsYield)
import ExoMonad.Wasm.Interpreter.State (runStateAsYield, runStateAsYieldWith)
