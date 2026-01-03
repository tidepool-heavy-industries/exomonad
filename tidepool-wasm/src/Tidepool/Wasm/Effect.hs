{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | WASM effect system using freer-simple.
--
-- This module provides a yield/resume capable effect system for WASM execution.
-- Unlike effect systems based on ReaderT IO, freer-simple reifies
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
  , logInfoWith
  , logError
  , logErrorWith
  , llmComplete
  , llmCompleteWith
  , llmCall
  , llmCallWith
    -- ** LLM Call Types (for tool-aware calls)
  , LlmCallResult(..)
  , WireMessage(..)
  , WireContentBlock(..)
  , WireToolCall(..)
  , askUserToolSchema
    -- ** Telegram
  , telegramSend
  , telegramMarkdown
  , telegramHtml
  , telegramAsk
  , TelegramAskResult(..)
    -- ** State
  , getState
  , setState
  , modifyState
    -- ** Events
  , emitEvent
    -- ** Random
  , randomInt
  , rollDice
    -- ** Time
  , getTime

    -- * Habitica (typed API)
  , habitica

    -- * Running Effects
  , runWasmM
  , WasmStatus
  ) where

import Control.Monad.Freer (Eff, Member, run)
import Control.Monad.Freer.Coroutine (Yield, yield, runC, Status(..))
import Data.Aeson (Value(..), toJSON, fromJSON, Result(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Tidepool.Wasm.CfTool (CfTool(..), CfObjectSchema(..), CfProperty(..), cfToolToValue)
import Tidepool.Wasm.WireTypes
  ( SerializableEffect(..)
  , EffectResult(..)
  , TelegramAskResult(..)
  , LlmCallResult(..)
  , WireMessage(..)
  , WireContentBlock(..)
  , WireToolCall(..)
  )
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

-- | Log an info message (no structured fields).
--
-- Yields 'EffLogInfo', expects acknowledgment (result ignored).
logInfo :: Member (Yield SerializableEffect EffectResult) effs
        => Text -> Eff effs ()
logInfo msg = do
  _ <- yield (EffLogInfo msg Nothing) (id @EffectResult)
  pure ()

-- | Log an info message with structured fields for queryable log data.
--
-- Example:
-- @
-- logInfoWith "Scoring daily"
--   [ ("taskId", toJSON taskId)
--   , ("direction", toJSON ("up" :: Text))
--   ]
-- @
--
-- Yields 'EffLogInfo' with fields, expects acknowledgment (result ignored).
logInfoWith :: Member (Yield SerializableEffect EffectResult) effs
            => Text -> [(Text, Value)] -> Eff effs ()
logInfoWith msg fields = do
  _ <- yield (EffLogInfo msg (Just (Map.fromList fields))) (id @EffectResult)
  pure ()

-- | Log an error message (no structured fields).
--
-- Yields 'EffLogError', expects acknowledgment (result ignored).
logError :: Member (Yield SerializableEffect EffectResult) effs
         => Text -> Eff effs ()
logError msg = do
  _ <- yield (EffLogError msg Nothing) (id @EffectResult)
  pure ()

-- | Log an error message with structured fields for queryable log data.
--
-- Yields 'EffLogError' with fields, expects acknowledgment (result ignored).
logErrorWith :: Member (Yield SerializableEffect EffectResult) effs
             => Text -> [(Text, Value)] -> Eff effs ()
logErrorWith msg fields = do
  _ <- yield (EffLogError msg (Just (Map.fromList fields))) (id @EffectResult)
  pure ()

-- | Make an LLM completion call with default model.
--
-- Yields 'EffLlmComplete', expects JSON response on success.
-- Uses the default model configured in the TypeScript handler.
llmComplete :: Member (Yield SerializableEffect EffectResult) effs
            => Text           -- ^ Node name (for observability)
            -> Text           -- ^ System prompt
            -> Text           -- ^ User content
            -> Maybe Value    -- ^ Output schema (optional)
            -> Eff effs Value
llmComplete = llmCompleteWith Nothing

-- | Make an LLM completion call with explicit model.
--
-- Yields 'EffLlmComplete', expects JSON response on success.
-- Pass @Just "@cf/meta/llama-3.2-1b-instruct"@ for a specific model,
-- or @Nothing@ to use the TypeScript handler's default.
llmCompleteWith :: Member (Yield SerializableEffect EffectResult) effs
                => Maybe Text     -- ^ Model to use (Nothing for default)
                -> Text           -- ^ Node name (for observability)
                -> Text           -- ^ System prompt
                -> Text           -- ^ User content
                -> Maybe Value    -- ^ Output schema (optional)
                -> Eff effs Value
llmCompleteWith model node systemPrompt userContent schema = do
  result <- yield (EffLlmComplete node systemPrompt userContent schema model) (id @EffectResult)
  case result of
    ResSuccess (Just v) -> pure v
    ResSuccess Nothing  -> pure (toJSON ())
    ResError msg        -> error $ "LLM call failed: " <> T.unpack msg

-- | Make a raw LLM call with full message history and optional tools.
--
-- Yields 'EffLlmCall', returns the raw LLM response including any tool calls.
-- Use this when you need tool calling or multi-turn conversation support.
-- Uses the default model configured in the TypeScript handler.
llmCall :: Member (Yield SerializableEffect EffectResult) effs
        => Text           -- ^ Node name (for observability)
        -> [WireMessage]  -- ^ Full message history
        -> Maybe Value    -- ^ Output schema (optional)
        -> [Value]        -- ^ Tool definitions (CF AI flat format)
        -> Eff effs LlmCallResult
llmCall = llmCallWith Nothing

-- | Make a raw LLM call with explicit model.
--
-- Yields 'EffLlmCall', returns the raw LLM response including any tool calls.
-- Use this when you need tool calling or multi-turn conversation support.
-- Pass @Just "@cf/meta/llama-3.2-1b-instruct"@ for a specific model,
-- or @Nothing@ to use the TypeScript handler's default.
llmCallWith :: Member (Yield SerializableEffect EffectResult) effs
            => Maybe Text     -- ^ Model to use (Nothing for default)
            -> Text           -- ^ Node name (for observability)
            -> [WireMessage]  -- ^ Full message history
            -> Maybe Value    -- ^ Output schema (optional)
            -> [Value]        -- ^ Tool definitions (CF AI flat format)
            -> Eff effs LlmCallResult
llmCallWith model node messages schema tools = do
  result <- yield (EffLlmCall node messages schema tools model) (id @EffectResult)
  case result of
    ResSuccess (Just v) -> case fromJSON v of
      Success r -> pure r
      Error err -> error $ "LlmCall: failed to parse result: " <> err
    ResSuccess Nothing  -> error "LlmCall: no response"
    ResError msg        -> error $ "[v2-tools-only] LlmCall failed: " <> T.unpack msg

-- | Tool schema for asking user a clarifying question (CF AI flat format).
askUserToolSchema :: Value
askUserToolSchema = cfToolToValue askUserTool

-- | Tool definition for ask_user.
askUserTool :: CfTool
askUserTool = CfTool
  { ctName = "ask_user"
  , ctDescription = "Ask the user a clarifying question. Use when the input is ambiguous and you need more information to proceed accurately."
  , ctParameters = CfObjectSchema
      { cosProperties = Map.fromList
          [ ("question", CfString "The question to ask the user")
          , ("options", CfArray "Optional button choices. If provided, user clicks one. If omitted, user types freeform response." CfStringType)
          ]
      , cosRequired = ["question"]
      }
  }


-- | Send a plain text message via Telegram.
--
-- Yields 'EffTelegramSend', returns message ID (as Int wrapped in Value).
-- Fire-and-forget semantics - doesn't block for response.
telegramSend :: Member (Yield SerializableEffect EffectResult) effs
             => Text  -- ^ Message text
             -> Eff effs Int
telegramSend txt = do
  result <- yield (EffTelegramSend txt "PlainText") (id @EffectResult)
  case result of
    ResSuccess (Just v) -> case v of
      Number n -> pure $ round n
      _        -> pure 0  -- Fire-and-forget OK
    ResSuccess Nothing  -> pure 0
    ResError msg        -> error $ "Telegram send failed: " <> T.unpack msg

-- | Send a Markdown-formatted message via Telegram.
--
-- Yields 'EffTelegramSend' with Markdown parse mode.
telegramMarkdown :: Member (Yield SerializableEffect EffectResult) effs
                 => Text  -- ^ Markdown text
                 -> Eff effs Int
telegramMarkdown txt = do
  result <- yield (EffTelegramSend txt "Markdown") (id @EffectResult)
  case result of
    ResSuccess (Just v) -> case v of
      Number n -> pure $ round n
      _        -> pure 0
    ResSuccess Nothing  -> pure 0
    ResError msg        -> error $ "Telegram send failed: " <> T.unpack msg

-- | Send an HTML-formatted message via Telegram.
--
-- Yields 'EffTelegramSend' with HTML parse mode.
telegramHtml :: Member (Yield SerializableEffect EffectResult) effs
             => Text  -- ^ HTML text
             -> Eff effs Int
telegramHtml txt = do
  result <- yield (EffTelegramSend txt "HTML") (id @EffectResult)
  case result of
    ResSuccess (Just v) -> case v of
      Number n -> pure $ round n
      _        -> pure 0
    ResSuccess Nothing  -> pure 0
    ResError msg        -> error $ "Telegram send failed: " <> T.unpack msg

-- | Ask user with custom buttons, returns the result.
--
-- Yields 'EffTelegramAsk', blocks until user responds.
-- The user can:
-- 1. Click a button → 'TelegramButton' with the action
-- 2. Send text instead → 'TelegramText' with the message
-- 3. Click a stale button → 'TelegramStaleButton'
telegramAsk :: Member (Yield SerializableEffect EffectResult) effs
            => Text           -- ^ Message to display
            -> [(Text, Text)] -- ^ [(button label, callback data)]
            -> Eff effs TelegramAskResult
telegramAsk message buttons = do
  result <- yield (EffTelegramAsk message "Markdown" buttons) (id @EffectResult)
  case result of
    ResSuccess (Just v) -> case fromJSON v of
      Success askResult -> pure askResult
      Error err         -> error $ "Telegram ask: failed to parse result: " <> err
    ResSuccess Nothing  -> error "Telegram ask: no response"
    ResError msg        -> error $ "Telegram ask failed: " <> T.unpack msg


-- ════════════════════════════════════════════════════════════════════════════
-- STATE EFFECTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Get state by key.
--
-- Yields 'EffGetState', returns the state value (or Null if not found).
getState :: Member (Yield SerializableEffect EffectResult) effs
         => Text  -- ^ State key (e.g., "worldState")
         -> Eff effs Value
getState key = do
  result <- yield (EffGetState key) (id @EffectResult)
  case result of
    ResSuccess (Just v) -> pure v
    ResSuccess Nothing  -> pure Null
    ResError msg        -> error $ "getState failed: " <> T.unpack msg

-- | Set state by key.
--
-- Yields 'EffSetState', fire-and-forget semantics.
setState :: Member (Yield SerializableEffect EffectResult) effs
         => Text   -- ^ State key
         -> Value  -- ^ New state value
         -> Eff effs ()
setState key value = do
  _ <- yield (EffSetState key value) (id @EffectResult)
  pure ()

-- | Modify state with a function.
--
-- Convenience wrapper that performs get → modify → set.
-- The modifier function receives the current state (or Null if not found).
modifyState :: Member (Yield SerializableEffect EffectResult) effs
            => Text            -- ^ State key
            -> (Value -> Value)  -- ^ Modifier function
            -> Eff effs ()
modifyState key f = do
  current <- getState key
  setState key (f current)


-- ════════════════════════════════════════════════════════════════════════════
-- EVENT EFFECTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Emit an event for observability/GUI updates.
--
-- Yields 'EffEmitEvent', fire-and-forget semantics.
-- Events are forwarded to connected clients (WebSocket) for real-time updates.
emitEvent :: Member (Yield SerializableEffect EffectResult) effs
          => Text   -- ^ Event name (e.g., "StressChanged", "ClockAdvanced")
          -> Value  -- ^ Event payload
          -> Eff effs ()
emitEvent name payload = do
  _ <- yield (EffEmitEvent name payload) (id @EffectResult)
  pure ()


-- ════════════════════════════════════════════════════════════════════════════
-- RANDOM EFFECTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Get a random integer in range [min, max] (inclusive).
--
-- Yields 'EffRandomInt', returns a random integer.
randomInt :: Member (Yield SerializableEffect EffectResult) effs
          => Int  -- ^ Minimum value (inclusive)
          -> Int  -- ^ Maximum value (inclusive)
          -> Eff effs Int
randomInt minVal maxVal = do
  result <- yield (EffRandomInt minVal maxVal) (id @EffectResult)
  case result of
    ResSuccess (Just v) -> case v of
      Number n -> pure $ round n
      _        -> error "randomInt: expected number"
    ResSuccess Nothing  -> error "randomInt: no response"
    ResError msg        -> error $ "randomInt failed: " <> T.unpack msg

-- | Roll multiple dice (e.g., for FitD mechanics).
--
-- Convenience function that rolls n dice with sides [1..sides].
rollDice :: Member (Yield SerializableEffect EffectResult) effs
         => Int  -- ^ Number of dice
         -> Int  -- ^ Number of sides per die
         -> Eff effs [Int]
rollDice n sides = mapM (\_ -> randomInt 1 sides) [1..n]


-- ════════════════════════════════════════════════════════════════════════════
-- TIME EFFECTS
-- ════════════════════════════════════════════════════════════════════════════

-- | Get current UTC time as ISO8601 string.
--
-- Yields 'EffGetTime', returns time like "2024-01-15T10:30:00Z".
getTime :: Member (Yield SerializableEffect EffectResult) effs
        => Eff effs Text
getTime = do
  result <- yield EffGetTime (id @EffectResult)
  case result of
    ResSuccess (Just v) -> case v of
      String s -> pure s
      _        -> error "getTime: expected string"
    ResSuccess Nothing  -> error "getTime: no response"
    ResError msg        -> error $ "getTime failed: " <> T.unpack msg


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
