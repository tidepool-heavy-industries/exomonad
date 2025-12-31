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
-- Implementation uses a global IORef to persist state across FFI calls.
-- This works in WASM because IORef is synchronous (non-blocking) and each
-- WASM module instance is isolated (one per Durable Object).
module Tidepool.Wasm.Ffi
  ( -- * FFI Exports
    initialize
  , step
  , getGraphInfo
  , getGraphState
    -- * Testing (native only)
  , resetState
  ) where

import Data.Aeson (eitherDecodeStrict, encode, object, (.=))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.IO.Unsafe (unsafePerformIO)

import Tidepool.Wasm.Runner
  ( GraphYield(..)
  , RunnerState(..)
  , initializeGraph
  , stepGraph
  )
import Tidepool.Wasm.WireTypes
  ( EffectResult(..)
  , ExecutionPhase(..)
  , GraphState(..)
  , StepOutput(..)
  )

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
-- GLOBAL STATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Global state persisted across FFI calls.
--
-- In WASM: Each module instance is isolated (one per Durable Object),
-- so "global" really means "session-scoped". Single-threaded, no races.
--
-- Note: NOINLINE is critical - ensures single IORef across all call sites.
{-# NOINLINE globalState #-}
globalState :: IORef (Maybe RunnerState)
globalState = unsafePerformIO $ newIORef Nothing


-- | Reset state (for testing only).
resetState :: IO ()
resetState = writeIORef globalState Nothing


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Encode value to JSON Text.
encodeText :: StepOutput -> Text
encodeText = TL.toStrict . TLE.decodeUtf8 . encode

-- | Create error StepOutput.
mkErrorOutput :: Text -> StepOutput
mkErrorOutput msg = StepOutput
  { soEffect = Nothing
  , soDone = True
  , soStepResult = Nothing
  , soGraphState = GraphState (PhaseFailed msg) []
  }

-- | Convert GraphYield to (StepOutput, Maybe RunnerState).
--
-- YieldEffect: Return effect with continuation stored in state
-- YieldComplete: Return done=True with result, clear state
-- YieldError: Return done=True with error, clear state
yieldToOutput :: GraphYield -> (StepOutput, Maybe RunnerState)
yieldToOutput (YieldEffect eff cont) =
  ( StepOutput
      { soEffect = Just eff
      , soDone = False
      , soStepResult = Nothing
      , soGraphState = GraphState (PhaseInNode "compute") []
      }
  , Just (RunnerState cont (PhaseInNode "compute"))
  )
yieldToOutput (YieldComplete result) =
  ( StepOutput
      { soEffect = Nothing
      , soDone = True
      , soStepResult = Just result
      , soGraphState = GraphState (PhaseCompleted result) ["compute"]
      }
  , Nothing  -- Clear state on completion
  )
yieldToOutput (YieldError msg) =
  ( StepOutput
      { soEffect = Nothing
      , soDone = True
      , soStepResult = Nothing
      , soGraphState = GraphState (PhaseFailed msg) []
      }
  , Nothing  -- Clear state on error
  )


-- ════════════════════════════════════════════════════════════════════════════
-- IMPLEMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Initialize graph execution.
--
-- 1. Parses entry value (Int) from JSON
-- 2. Runs graph until first effect yield
-- 3. Stores continuation in global state
-- 4. Returns StepOutput with effect to execute
#if defined(wasm32_HOST_ARCH)
initialize :: JSString -> IO JSString
initialize input = toJSString <$> initializeImpl (fromJSString input)
#else
initialize :: Text -> IO Text
initialize = initializeImpl
#endif

initializeImpl :: Text -> IO Text
initializeImpl inputJson =
  case eitherDecodeStrict (encodeUtf8 inputJson) of
    Left err -> pure $ encodeText $ mkErrorOutput $ "JSON parse error: " <> T.pack err
    Right entry -> do
      let yield = initializeGraph entry
          (output, mState) = yieldToOutput yield
      writeIORef globalState mState
      pure $ encodeText output


-- | Step graph execution with effect result.
--
-- 1. Loads continuation from global state
-- 2. Parses EffectResult from JSON
-- 3. Resumes graph execution with result
-- 4. Stores new continuation or clears state on completion
-- 5. Returns StepOutput
#if defined(wasm32_HOST_ARCH)
step :: JSString -> IO JSString
step result = toJSString <$> stepImpl (fromJSString result)
#else
step :: Text -> IO Text
step = stepImpl
#endif

stepImpl :: Text -> IO Text
stepImpl resultJson = do
  mState <- readIORef globalState
  case mState of
    Nothing ->
      pure $ encodeText $ mkErrorOutput "Not initialized (no continuation)"
    Just state ->
      case eitherDecodeStrict (encodeUtf8 resultJson) of
        Left err ->
          pure $ encodeText $ mkErrorOutput $ "JSON parse error: " <> T.pack err
        Right effectResult -> do
          let yield = stepGraph state.rsContinuation effectResult
              (output, mNewState) = yieldToOutput yield
          writeIORef globalState mNewState
          pure $ encodeText output


-- | Get graph metadata.
--
-- Returns static graph structure. For TestGraph:
-- Entry(Int) → compute → Exit(Int)
#if defined(wasm32_HOST_ARCH)
getGraphInfo :: IO JSString
getGraphInfo = toJSString <$> getGraphInfoImpl
#else
getGraphInfo :: IO Text
getGraphInfo = getGraphInfoImpl
#endif

getGraphInfoImpl :: IO Text
getGraphInfoImpl =
  -- Static graph info for TestGraph
  pure $ TL.toStrict $ TLE.decodeUtf8 $ encode $
    object
      [ "name" .= ("TestGraph" :: Text)
      , "nodes" .= (["entry", "compute", "exit"] :: [Text])
      , "edges" .= object
          [ "entry" .= ("compute" :: Text)
          , "compute" .= ("exit" :: Text)
          ]
      ]


-- | Get current graph state.
--
-- Returns runtime state from global IORef.
#if defined(wasm32_HOST_ARCH)
getGraphState :: IO JSString
getGraphState = toJSString <$> getGraphStateImpl
#else
getGraphState :: IO Text
getGraphState = getGraphStateImpl
#endif

getGraphStateImpl :: IO Text
getGraphStateImpl = do
  mState <- readIORef globalState
  let graphState = case mState of
        Nothing -> GraphState PhaseIdle []
        Just state -> GraphState state.rsPhase []
  pure $ TL.toStrict $ TLE.decodeUtf8 $ encode graphState
