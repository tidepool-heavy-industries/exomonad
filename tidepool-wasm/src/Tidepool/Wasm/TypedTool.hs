{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Typed tool execution helpers for LLM tool calling.
--
-- This module provides type-safe helpers for parsing and executing tool calls
-- that have already been requested by the LLM. Instead of manual JSON parsing:
--
-- @
-- case toolCall of
--   WireToolCall { wtcName = "ask_user", wtcInput = input } -> do
--     let question = fromMaybe "" $ input ^? key "question" . _String
--     -- manual JSON parsing...
-- @
--
-- You can write:
--
-- @
-- input <- parseToolInputM @AskUserInput toolCall
-- selected <- askUser input
-- -- selected is the Text of the option the user clicked
-- @
--
-- = Design
--
-- Each tool helper works with three types:
--
-- 1. **Input type** - Parsed from the 'WireToolCall' JSON (e.g., 'AskUserInput')
-- 2. **Output type** - Raw result from the executor (e.g., 'TelegramAskResult')
-- 3. **Result type** - Simplified return value (e.g., 'Text')
--
-- For example, @ask_user@ has:
--
-- * Input: @AskUserInput { question, options }@ - Parsed from tool call
-- * Output: @TelegramAskResult@ - Raw result from Telegram
-- * Result: @Text@ - Just the selected option text
--
-- = Adding New Tools
--
-- To add a typed helper for a new tool:
--
-- 1. Define the input type with 'FromJSON' to parse from 'WireToolCall'
-- 2. Implement an executor that yields the appropriate effect
-- 3. Use 'parseToolInput' or 'parseToolInputM' in your dispatcher:
--
-- @
-- myToolHandler :: WireToolCall -> Eff effs (Either Text Result)
-- myToolHandler tc = do
--   input <- parseToolInputM tc
--   executeMyTool input
-- @
module Tidepool.Wasm.TypedTool
  ( -- * ask_user Tool
    AskUserInput(..)
  , askUser
  , askUserWithFallback
  , executeAskUser

    -- * Tool Input Parsing
  , parseToolInput
  , parseToolInputM

    -- * Tool Execution Helpers
  , ToolCallHandler
  , handleToolCalls
  , requireSingleTool

    -- * Re-exports for convenience
  , WireToolCall(..)
  ) where

import Control.Monad (forM)
import Control.Monad.Freer (Member, Eff)
import Control.Monad.Freer.Coroutine (Yield)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Result(..)
  , fromJSON
  , object
  , (.=)
  , (.:)
  , (.:?)
  , withObject
  )
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Wasm.WireTypes
  ( SerializableEffect
  , EffectResult
  , WireToolCall(..)
  , TelegramAskResult(..)
  )
import Tidepool.Wasm.Effect
  ( telegramAsk
  , logInfo
  , logError
  )


-- ════════════════════════════════════════════════════════════════════════════
-- ASK_USER TOOL
-- ════════════════════════════════════════════════════════════════════════════

-- | Input for the ask_user tool.
--
-- This is what the LLM specifies when calling the tool.
-- The question is required, options are optional (freeform text if omitted).
data AskUserInput = AskUserInput
  { auiQuestion :: !Text
    -- ^ The question to ask the user
  , auiOptions :: !(Maybe [Text])
    -- ^ Optional button choices. If provided, user clicks one.
    --   If omitted, user types freeform response.
  }
  deriving (Eq, Show)

instance FromJSON AskUserInput where
  parseJSON = withObject "AskUserInput" $ \o -> AskUserInput
    <$> o .: "question"
    <*> o .:? "options"

instance ToJSON AskUserInput where
  toJSON AskUserInput{..} = object $
    [ "question" .= auiQuestion ]
    ++ maybe [] (\opts -> ["options" .= opts]) auiOptions


-- | Execute the ask_user tool: ask the user a question via Telegram.
--
-- Handles the effectful flow:
-- 1. Takes already-parsed 'AskUserInput'
-- 2. Yields TelegramAsk effect
-- 3. Returns the selected option or typed text
--
-- Errors loudly on edge cases (stale buttons, unexpected button clicks).
--
-- Example:
--
-- @
-- let input = AskUserInput "What next?" (Just ["Continue", "Stop"])
-- selectedText <- askUser input
-- useSelection selectedText
-- @
askUser
  :: Member (Yield SerializableEffect EffectResult) effs
  => AskUserInput
  -> Eff effs Text
askUser AskUserInput{..} = do
  logInfo $ "Asking user: " <> auiQuestion
  case auiOptions of
    Nothing -> do
      -- No options = freeform text response
      -- Show a prompt button; user must type a response (clicking button errors)
      eitherResult <- telegramAsk auiQuestion [("Type your response", "freeform")]
      case eitherResult of
        Left err ->
          error $ "askUser: telegram effect failed: " <> show err
        Right result -> case result of
          TelegramButton _ ->
            error $ "askUser: user clicked button instead of typing for freeform question: "
                 <> T.unpack auiQuestion
          TelegramText txt -> pure txt
          TelegramStaleButton ->
            error $ "askUser: stale button clicked for freeform question: "
                 <> T.unpack auiQuestion
    Just opts -> do
      -- With options = inline keyboard buttons
      let buttons = [(opt, opt) | opt <- opts]  -- label = callback data
      eitherResult <- telegramAsk auiQuestion buttons
      case eitherResult of
        Left err ->
          error $ "askUser: telegram effect failed: " <> show err
        Right result -> case result of
          TelegramButton response -> pure response  -- The callback_data = option text
          TelegramText txt -> pure txt  -- User typed instead of clicking
          TelegramStaleButton ->
            error $ "askUser: stale button clicked for question: "
                 <> T.unpack auiQuestion


-- | Ask user with a fallback for when no options are provided.
--
-- If the LLM provides non-empty options, shows them as buttons.
-- If no options or empty list, shows the fallback options instead.
askUserWithFallback
  :: Member (Yield SerializableEffect EffectResult) effs
  => [Text]  -- ^ Fallback options if LLM doesn't provide any
  -> AskUserInput
  -> Eff effs Text
askUserWithFallback fallbackOpts input =
  let effectiveOpts = case input.auiOptions of
        Nothing -> fallbackOpts
        Just [] -> fallbackOpts
        Just opts -> opts
  in askUser input { auiOptions = Just effectiveOpts }


-- ════════════════════════════════════════════════════════════════════════════
-- TOOL EXECUTION HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Handler type for dispatching tool calls.
--
-- Given a tool call, executes it and returns the result as JSON string
-- (suitable for sending back to the LLM as tool_result content).
type ToolCallHandler effs = WireToolCall -> Eff effs (Either Text Text)


-- | Execute multiple tool calls with a dispatcher.
--
-- Returns list of (tool_use_id, result) pairs for building tool_result messages.
handleToolCalls
  :: Member (Yield SerializableEffect EffectResult) effs
  => ToolCallHandler effs
  -> [WireToolCall]
  -> Eff effs [(Text, Either Text Text)]
handleToolCalls handler calls = forM calls $ \tc -> do
  result <- handler tc
  pure (tc.wtcId, result)


-- | Extract a single expected tool call.
--
-- Use when you expect exactly one tool call of a specific type.
-- Returns Left with error message if expectations not met.
requireSingleTool
  :: Text  -- ^ Expected tool name
  -> [WireToolCall]
  -> Either Text WireToolCall
requireSingleTool expectedName calls = case calls of
  [] -> Left "No tool calls received"
  [tc]
    | tc.wtcName == expectedName -> Right tc
    | otherwise -> Left $ "Expected tool '" <> expectedName
                       <> "' but got '" <> tc.wtcName <> "'"
  _  -> Left $ "Expected single tool call but got " <> T.pack (show (length calls))


-- ════════════════════════════════════════════════════════════════════════════
-- TOOL INPUT PARSING
-- ════════════════════════════════════════════════════════════════════════════

-- | Parse tool input from a WireToolCall to a typed value.
--
-- Example:
--
-- @
-- case parseToolInput @AskUserInput toolCall of
--   Right input -> askUser input
--   Left err -> logError $ "Invalid ask_user input: " <> err
-- @
parseToolInput
  :: FromJSON a
  => WireToolCall
  -> Either Text a
parseToolInput tc = case fromJSON tc.wtcInput of
  Success a -> Right a
  Error err -> Left $ "Failed to parse tool input for '"
                   <> tc.wtcName <> "': " <> T.pack err


-- | Parse tool input, failing hard if parsing fails.
--
-- This is the "fail fast" variant of 'parseToolInput'. Use this when:
--
-- * Tool input schema is defined by you (not external)
-- * Parse failure indicates a bug in the LLM or tool definition
-- * You want to abort rather than handle malformed input gracefully
--
-- For graceful error handling, use 'parseToolInput' which returns 'Either'.
--
-- Logs the error before terminating.
parseToolInputM
  :: (FromJSON a, Member (Yield SerializableEffect EffectResult) effs)
  => WireToolCall
  -> Eff effs a
parseToolInputM tc = case parseToolInput tc of
  Right a -> pure a
  Left err -> do
    logError err
    error $ T.unpack err


-- ════════════════════════════════════════════════════════════════════════════
-- COMBINED TOOL EXECUTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Execute an ask_user tool call from a WireToolCall.
--
-- Returns 'Left' for parse errors only. Note that 'askUser' may still
-- throw errors for edge cases (stale buttons, unexpected button clicks
-- in freeform mode). If you need to catch those, wrap in exception handling.
--
-- @
-- case lcrToolCalls of
--   [tc] | tc.wtcName == "ask_user" -> do
--     result <- executeAskUser tc
--     -- result is Right Text (user's selection) or Left Text (parse error)
--   ...
-- @
executeAskUser
  :: Member (Yield SerializableEffect EffectResult) effs
  => WireToolCall
  -> Eff effs (Either Text Text)
executeAskUser tc = case parseToolInput tc of
  Left err -> pure $ Left err
  Right input -> Right <$> askUser input
