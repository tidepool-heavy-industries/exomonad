{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | WASM FFI exports for graph execution.
--
-- These are the entry points that TypeScript calls into the WASM module.
-- The functions are exported when compiling for WASM target (wasm32_HOST_ARCH).
--
-- For native builds, we provide the same interface without FFI exports,
-- allowing the same code to be tested natively.
--
-- __Note__: The actual implementations are stubs. The runner implementation
-- is handled by the larger LLM swarm.
module Tidepool.Wasm.Ffi
  ( -- * FFI Exports
    initialize
  , step
  , getGraphInfo
  , getGraphState
  ) where

import Data.Text (Text)

#if defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim (JSString, fromJSString, toJSString)
#endif


-- ════════════════════════════════════════════════════════════════════════════
-- FFI EXPORTS (WASM target)
-- ════════════════════════════════════════════════════════════════════════════

#if defined(wasm32_HOST_ARCH)

-- | Start graph execution with JSON input.
--
-- Takes JSON-encoded entry value, returns JSON-encoded StepOutput.
-- The StepOutput contains either:
-- - An effect to execute (caller should handle, then call 'step')
-- - done=True with final result
foreign export javascript "initialize" initialize :: JSString -> IO JSString

-- | Continue graph execution with effect result.
--
-- Takes JSON-encoded EffectResult from the previous effect.
-- Returns JSON-encoded StepOutput for next step.
foreign export javascript "step" step :: JSString -> IO JSString

-- | Get compile-time graph structure.
--
-- Returns JSON-encoded GraphInfo describing nodes, edges, types.
foreign export javascript "getGraphInfo" getGraphInfo :: IO JSString

-- | Get current runtime graph state.
--
-- Returns JSON-encoded GraphState showing execution progress.
foreign export javascript "getGraphState" getGraphState :: IO JSString

#endif


-- ════════════════════════════════════════════════════════════════════════════
-- IMPLEMENTATION (stubs - runner handled by swarm)
-- ════════════════════════════════════════════════════════════════════════════

-- | Initialize graph execution.
--
-- TODO: Swarm implements actual runner that:
-- 1. Parses entry value from JSON
-- 2. Initializes graph state
-- 3. Runs until first effect yield or completion
-- 4. Returns StepOutput
#if defined(wasm32_HOST_ARCH)
initialize :: JSString -> IO JSString
initialize input = do
  let _inputText = fromJSString input
  -- TODO: Swarm implements runner
  toJSString <$> initializeImpl _inputText
#else
initialize :: Text -> IO Text
initialize = initializeImpl
#endif

initializeImpl :: Text -> IO Text
initializeImpl _input =
  error "TODO: Runner implementation (handled by swarm)"


-- | Step graph execution with effect result.
--
-- TODO: Swarm implements actual runner that:
-- 1. Parses EffectResult from JSON
-- 2. Resumes graph execution with result
-- 3. Runs until next effect yield or completion
-- 4. Returns StepOutput
#if defined(wasm32_HOST_ARCH)
step :: JSString -> IO JSString
step result = do
  let _resultText = fromJSString result
  toJSString <$> stepImpl _resultText
#else
step :: Text -> IO Text
step = stepImpl
#endif

stepImpl :: Text -> IO Text
stepImpl _result =
  error "TODO: Runner implementation (handled by swarm)"


-- | Get graph metadata.
--
-- TODO: Swarm implements using Reify infrastructure
#if defined(wasm32_HOST_ARCH)
getGraphInfo :: IO JSString
getGraphInfo = toJSString <$> getGraphInfoImpl
#else
getGraphInfo :: IO Text
getGraphInfo = getGraphInfoImpl
#endif

getGraphInfoImpl :: IO Text
getGraphInfoImpl =
  error "TODO: GraphInfo reification (handled by swarm)"


-- | Get current graph state.
--
-- TODO: Swarm implements state tracking
#if defined(wasm32_HOST_ARCH)
getGraphState :: IO JSString
getGraphState = toJSString <$> getGraphStateImpl
#else
getGraphState :: IO Text
getGraphState = getGraphStateImpl
#endif

getGraphStateImpl :: IO Text
getGraphStateImpl =
  error "TODO: GraphState tracking (handled by swarm)"
