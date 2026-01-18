{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

-- | LLM effect interpreter for WASM.
--
-- This module provides @runLLMAsYield@, which interprets the @LLM@ effect
-- by yielding @SerializableEffect@ to TypeScript. This enables handlers
-- using @Member LLM effs@ to work in WASM contexts.
--
-- = Design
--
-- The interpreter converts:
--
-- 1. @RunTurnOp systemPrompt content schema tools@ →
--    @EffLlmCall node messages schema tools@
-- 2. TypeScript handles the tool loop and returns @TurnResult@
-- 3. @EffectResult@ is parsed back to @TurnOutcome (TurnResult Value)@
--
-- = Usage
--
-- @
-- -- Handler using Member LLM (works in both WASM and native)
-- myHandler :: Member LLM effs => Input -> Eff effs Output
-- myHandler input = do
--   result <- runTurn systemPrompt content schema tools
--   -- Tool loop handled by runtime
--   pure $ processResult result
--
-- -- In WASM context: compose with runLLMAsYield
-- runWasmHandler :: Input -> Eff '[Yield SerializableEffect EffectResult] Output
-- runWasmHandler = runLLMAsYield . myHandler
-- @
module Tidepool.Wasm.Interpreter.LLM
  ( runLLMAsYield
  ) where

import Control.Monad.Freer (Eff, Member, interpret)
import Control.Monad.Freer.Coroutine (Yield, yield)
import Data.Aeson (Value)
import Data.Text (Text)

import Tidepool.Effect.Types
  ( LLM(..)
  , NodeMetadata(..)
  , TurnOutcome(..)
  , TurnResult(..)
  )
import Tidepool.Wasm.WireTypes
  ( SerializableEffect(..)
  , EffectResult(..)
  )
import Tidepool.Wasm.Conversion
  ( contentBlocksToWireMessages
  , parseWireTurnResult
  )


-- | Default node name used when no context is provided.
--
-- In production, this should be passed from the graph interpreter context.
defaultNodeName :: Text
defaultNodeName = "graph-node"


-- | Interpret @LLM@ effect by yielding to TypeScript.
--
-- This interpreter converts @LLM@ operations to @Yield SerializableEffect@,
-- enabling handlers that use @Member LLM effs@ to work in WASM contexts.
--
-- TypeScript receives @EffLlmCall@ and handles:
--
-- * The tool loop (calling tools until done)
-- * API calls to the LLM provider
-- * Parsing structured output
--
-- The final result is returned as @EffectResult@ and parsed back to
-- @TurnOutcome (TurnResult Value)@.
--
-- = Node Name
--
-- Currently uses 'defaultNodeName' as a placeholder. In production, this should
-- be passed from the graph interpreter context.
runLLMAsYield
  :: Member (Yield SerializableEffect EffectResult) effs
  => Eff (LLM ': effs) a
  -> Eff effs a
runLLMAsYield = interpret handleLLM
  where
    handleLLM :: Member (Yield SerializableEffect EffectResult) effs
              => LLM x -> Eff effs x
    handleLLM (RunTurnOp meta systemPrompt userContent outputSchema toolDefs) = do
      -- Convert native ContentBlocks to wire messages
      let wireMessages = contentBlocksToWireMessages systemPrompt userContent

      -- Build the effect to yield, using node name from metadata
      let effect = EffLlmCall
            { effLlmNode = meta.nmNodeName
            , effLlmMessages = wireMessages
            , effLlmSchema = Just outputSchema
            , effLlmTools = toolDefs
            , effLlmModel = Nothing  -- Use TypeScript default
            }

      -- Yield to TypeScript and get result
      result <- yield effect (id @EffectResult)

      -- Parse result back to TurnOutcome
      pure $ parseEffectResult effect result


-- | Parse EffectResult back to TurnOutcome.
--
-- Handles success/error cases and parses the wire TurnResult.
parseEffectResult :: SerializableEffect -> EffectResult -> TurnOutcome (TurnResult Value)
parseEffectResult _eff result = case result of
  ResSuccess (Just val) ->
    case parseWireTurnResult val of
      Right turnResult -> TurnCompleted turnResult
      Left err -> TurnBroken $ "Failed to parse TurnResult: " <> err

  ResSuccess Nothing ->
    TurnBroken "Empty result from LLM call"

  ResError msg ->
    TurnBroken $ "LLM call failed: " <> msg
