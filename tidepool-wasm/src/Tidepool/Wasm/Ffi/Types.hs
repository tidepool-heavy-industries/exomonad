{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

-- | Shared types and helpers for FFI exports.
--
-- This module provides the infrastructure used by TH-generated FFI code:
--
-- * 'WasmState' - The per-graph state (idle or waiting for effect result)
-- * 'SomeContinuation' - Existential wrapper for continuations
-- * Helper functions for encoding/decoding
module Tidepool.Wasm.Ffi.Types
  ( -- * State Types
    WasmState(..)
  , SomeContinuation(..)

    -- * Encoding Helpers
  , encodeStepOutput
  , mkErrorOutput

    -- * Result Conversion
  , wasmResultToOutputGeneric

    -- * Imports (re-exported for convenience)
  , module Tidepool.Wasm.WireTypes
  , module Tidepool.Wasm.Runner
  ) where

import Data.Aeson (encode)
import Data.IORef (IORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Tidepool.Wasm.Runner (WasmResult(..), initializeWasm)
import Tidepool.Wasm.WireTypes
  ( EffectResult(..)
  , ExecutionPhase(..)
  , GraphState(..)
  , StepOutput(..)
  , SerializableEffect
  )


-- ════════════════════════════════════════════════════════════════════════════
-- STATE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Existential wrapper for the continuation.
--
-- The continuation's result type varies by graph, so we wrap it existentially.
-- The 'toOutput' function converts the final value to StepOutput.
data SomeContinuation where
  SomeCont
    :: (EffectResult -> WasmResult a)  -- The continuation
    -> (a -> StepOutput)               -- How to convert result to StepOutput
    -> SomeContinuation

-- | Global state: either idle or waiting for an effect result.
data WasmState
  = Idle
  | Waiting SomeContinuation ExecutionPhase


-- ════════════════════════════════════════════════════════════════════════════
-- ENCODING HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Encode StepOutput to JSON Text.
encodeStepOutput :: StepOutput -> Text
encodeStepOutput = TL.toStrict . TLE.decodeUtf8 . encode

-- | Create error StepOutput.
mkErrorOutput :: Text -> StepOutput
mkErrorOutput msg = StepFailed msg (GraphState (PhaseFailed msg) [])


-- ════════════════════════════════════════════════════════════════════════════
-- RESULT CONVERSION
-- ════════════════════════════════════════════════════════════════════════════

-- | Generic version that uses the stored toOutput function.
--
-- This is called by TH-generated step functions to convert a WasmResult
-- to StepOutput, updating the appropriate state ref.
wasmResultToOutputGeneric
  :: IORef WasmState    -- ^ State ref for this graph
  -> Text               -- ^ Node name for phase tracking
  -> WasmResult a       -- ^ The result to convert
  -> (a -> StepOutput)  -- ^ How to convert the final value
  -> IO StepOutput
wasmResultToOutputGeneric stateRef _nodeName (WasmComplete a) toOutput = do
  writeIORef stateRef Idle
  pure $ toOutput a

wasmResultToOutputGeneric stateRef nodeName (WasmYield eff resume) toOutput = do
  let phase = PhaseInNode nodeName
  writeIORef stateRef (Waiting (SomeCont resume toOutput) phase)
  pure $ StepYield eff (GraphState phase [])

wasmResultToOutputGeneric stateRef _nodeName (WasmError msg) _toOutput = do
  writeIORef stateRef Idle
  pure $ mkErrorOutput msg
