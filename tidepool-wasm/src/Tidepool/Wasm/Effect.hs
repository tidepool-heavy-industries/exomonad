{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | WASM effect system using freer-simple.
--
-- This module provides a yield/resume capable effect system for WASM execution.
-- Unlike effectful (which is ReaderT IO under the hood), freer-simple reifies
-- continuations as data, enabling step-by-step execution across FFI boundaries.
--
-- = Design
--
-- Effects are modeled using freer-simple's built-in 'Yield' effect from
-- "Control.Monad.Freer.Coroutine". Each effect operation:
--
-- 1. Converts to a 'SerializableEffect' (wire format for TypeScript)
-- 2. Yields via 'Yield', suspending execution
-- 3. Receives an 'EffectResult' when resumed
-- 4. Parses the result back to the expected Haskell type
--
-- The 'Status' type captures whether execution is 'Done' or 'Continue'
-- (yielded with a continuation).
module Tidepool.Wasm.Effect
  ( -- * Effect Types
    WasmEffects
  , WasmM

    -- * Smart Constructors
  , logInfo
  , logError
  , llmComplete
  , telegramConfirm

    -- * Habitica (typed API)
  , habitica

    -- * Running Effects
  , runWasmM
  , WasmStatus
  ) where

import Control.Monad.Freer (Eff, Member, run)
import Control.Monad.Freer.Coroutine (Yield, yield, runC, Status(..))
import Data.Aeson (Value, toJSON)
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Wasm.WireTypes (SerializableEffect(..), EffectResult(..))
import Tidepool.Wasm.Habitica (habitica)


-- | The effect stack for WASM computations.
--
-- Uses 'Yield' to suspend at each effect, yielding 'SerializableEffect'
-- and expecting 'EffectResult' on resume.
type WasmEffects = '[Yield SerializableEffect EffectResult]

-- | The WASM effect monad.
--
-- Computations in 'WasmM' can be stepped through one effect at a time,
-- yielding to TypeScript for execution and resuming with results.
type WasmM a = Eff WasmEffects a

-- | Status after running a WASM computation.
--
-- Either 'Done' with a result, or 'Continue' with a yielded effect
-- and a continuation to resume.
type WasmStatus a = Status '[] SerializableEffect EffectResult a


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Log an info message.
--
-- Yields 'EffLogInfo', expects acknowledgment (result ignored).
logInfo :: Member (Yield SerializableEffect EffectResult) effs
        => Text -> Eff effs ()
logInfo msg = do
  _ <- yield (EffLogInfo msg) (id @EffectResult)
  pure ()

-- | Log an error message.
--
-- Yields 'EffLogError', expects acknowledgment (result ignored).
logError :: Member (Yield SerializableEffect EffectResult) effs
         => Text -> Eff effs ()
logError msg = do
  _ <- yield (EffLogError msg) (id @EffectResult)
  pure ()

-- | Make an LLM completion call.
--
-- Yields 'EffLlmComplete', expects JSON response on success.
llmComplete :: Member (Yield SerializableEffect EffectResult) effs
            => Text           -- ^ Node name (for observability)
            -> Text           -- ^ System prompt
            -> Text           -- ^ User content
            -> Maybe Value    -- ^ Output schema (optional)
            -> Eff effs Value
llmComplete node systemPrompt userContent schema = do
  result <- yield (EffLlmComplete node systemPrompt userContent schema) (id @EffectResult)
  case result of
    ResSuccess (Just v) -> pure v
    ResSuccess Nothing  -> pure (toJSON ())
    ResError msg        -> error $ "LLM call failed: " <> T.unpack msg


-- | Request confirmation from user via Telegram buttons.
--
-- Yields 'EffTelegramConfirm', expects JSON response with user's selection.
-- Default buttons are: Yes, No (with feedback), Skip
telegramConfirm :: Member (Yield SerializableEffect EffectResult) effs
                => Text   -- ^ Message to display
                -> Eff effs Value
telegramConfirm message = do
  let buttons =
        [ ("✓ Yes", "approved")
        , ("✗ No", "denied")
        , ("Skip", "skipped")
        ]
  result <- yield (EffTelegramConfirm message buttons) (id @EffectResult)
  case result of
    ResSuccess (Just v) -> pure v
    ResSuccess Nothing  -> pure (toJSON ())
    ResError msg        -> error $ "Telegram confirm failed: " <> T.unpack msg


-- ════════════════════════════════════════════════════════════════════════════
-- RUNNING EFFECTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a 'WasmM' computation until the first yield or completion.
--
-- Returns 'Status':
--
-- * @Done result@ - Computation completed with result
-- * @Continue eff k@ - Yielded effect @eff@, call @k result@ to resume
--
-- Example usage:
--
-- @
-- case runWasmM computation of
--   Done result -> handleResult result
--   Continue eff k -> do
--     -- Execute effect externally (e.g., in TypeScript)
--     effectResult <- executeEffect eff
--     -- Resume with result
--     let nextStatus = run (k effectResult)
--     -- Continue stepping...
-- @
runWasmM :: WasmM a -> WasmStatus a
runWasmM = run . runC
