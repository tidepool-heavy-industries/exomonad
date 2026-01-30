-- | Error types for WASM graph execution.
--
-- This module provides rich error types that preserve context for debugging.
-- Instead of using raw @error@ calls that crash with minimal information,
-- we use 'WasmError' which captures:
--
-- - Which effect failed
-- - What was expected vs what was received
-- - Parse errors with the actual value for debugging
--
-- Errors propagate through the existing 'StepFailed' wire type, preserving
-- fail-fast semantics while providing actionable debugging information.
module ExoMonad.Wasm.Error
  ( -- * Error Types
    WasmError (..),
    WasmEffectError (..),

    -- * Smart Constructors
    effectFailed,
    parseFailed,
    emptyResult,

    -- * Error Formatting
    formatWasmError,
    formatEffectError,
    effectName,
  )
where

import Data.Aeson (Value, encode)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import ExoMonad.Wasm.WireTypes (SerializableEffect (..))

-- ════════════════════════════════════════════════════════════════════════════
-- ERROR TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Errors during WASM graph execution.
data WasmError
  = -- | Error executing an effect
    WasmEffectError WasmEffectError
  | -- | Unexpected error (should not happen in validated graphs)
    WasmUnexpectedError Text
  deriving (Show, Eq)

-- | Errors that occur when executing effects.
--
-- These preserve rich context for debugging: the effect that failed,
-- what was expected, what was received, and why it failed.
data WasmEffectError
  = EffectFailed
      { -- | The effect that failed
        weeEffect :: SerializableEffect,
        -- | Error message from TypeScript (ResError)
        weeMessage :: Text
      }
  | ParseFailed
      { -- | The effect that returned the value
        weeEffect :: SerializableEffect,
        -- | Type we were trying to parse (e.g., "LlmCallResult")
        weeExpectedType :: Text,
        -- | The value we got (for debugging type mismatches)
        weeActualValue :: Value,
        -- | Aeson parse error message
        weeParseError :: Text
      }
  | EmptyResult
      { -- | The effect that returned empty
        weeEffect :: SerializableEffect,
        -- | What we were expecting (e.g., "LLM response")
        weeContext :: Text
      }
  deriving (Show, Eq)

-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Effect failed with an error message from TypeScript.
effectFailed :: SerializableEffect -> Text -> WasmError
effectFailed eff msg = WasmEffectError (EffectFailed eff msg)

-- | Failed to parse effect result into expected type.
parseFailed :: SerializableEffect -> Text -> Value -> Text -> WasmError
parseFailed eff ty val err = WasmEffectError (ParseFailed eff ty val err)

-- | Effect returned empty when we expected a value.
emptyResult :: SerializableEffect -> Text -> WasmError
emptyResult eff ctx = WasmEffectError (EmptyResult eff ctx)

-- ════════════════════════════════════════════════════════════════════════════
-- ERROR FORMATTING
-- ════════════════════════════════════════════════════════════════════════════

-- | Format error for display in StepFailed output.
formatWasmError :: WasmError -> Text
formatWasmError (WasmEffectError err) = formatEffectError err
formatWasmError (WasmUnexpectedError msg) = "Unexpected error: " <> msg

-- | Format effect error with full context.
formatEffectError :: WasmEffectError -> Text
formatEffectError (EffectFailed eff msg) =
  "Effect failed: " <> effectName eff <> "\n  Error: " <> msg
formatEffectError (ParseFailed eff expectedTy val parseErr) =
  "Failed to parse "
    <> expectedTy
    <> " from "
    <> effectName eff
    <> "\n"
    <> "  Parse error: "
    <> parseErr
    <> "\n"
    <> "  Received value: "
    <> (TL.toStrict $ TLE.decodeUtf8 $ encode val)
formatEffectError (EmptyResult eff ctx) =
  "Effect returned empty result: "
    <> effectName eff
    <> "\n"
    <> "  Expected: "
    <> ctx

-- | Get effect name for error messages.
effectName :: SerializableEffect -> Text
effectName (EffLlmComplete {}) = "LlmComplete"
effectName (EffLogInfo {}) = "LogInfo"
effectName (EffLogError {}) = "LogError"
effectName (EffHabitica {}) = "Habitica"
effectName (EffTelegramSend {}) = "TelegramSend"
effectName (EffTelegramAsk {}) = "TelegramAsk"
effectName (EffLlmCall {}) = "LlmCall"
effectName (EffGetState {}) = "GetState"
effectName (EffSetState {}) = "SetState"
effectName (EffEmitEvent {}) = "EmitEvent"
effectName (EffRandomInt {}) = "RandomInt"
effectName (EffGetTime {}) = "GetTime"
