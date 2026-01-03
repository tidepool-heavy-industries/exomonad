{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Typed tool results for LLM tool calling.
--
-- This module provides type-safe wrappers around common tool patterns,
-- eliminating manual JSON parsing in tool handlers. Instead of:
--
-- @
-- case result of
--   NeedsTools toolCalls -> do
--     forM toolCalls $ \tc -> case tcName tc of
--       "ask_user" -> do
--         let question = fromMaybe "" $ tcInput tc ^? key "question" . _String
--         -- manual JSON parsing...
-- @
--
-- You can write:
--
-- @
-- let input = AskUserInput "Which meds?" (Just ["Morning meds", "HRT Shot"])
-- selected <- askUser input
-- -- selected is the Text of the option the user clicked
-- @
--
-- = Design
--
-- Tools are defined with three components:
--
-- 1. **Input type** - What the LLM passes to the tool (parsed from tool call)
-- 2. **Output type** - What the tool returns (from external execution)
-- 3. **Result type** - What the smart constructor returns (may be simpler)
--
-- For example, @ask_user@ has:
--
-- * Input: @AskUserInput { question, options }@ - What LLM specifies
-- * Output: @TelegramAskResult@ - Raw result from Telegram
-- * Result: @Text@ - Just the selected option text
--
-- The smart constructor handles:
--
-- 1. Building the tool schema for the LLM
-- 2. Yielding the tool call to TypeScript
-- 3. Parsing the result
-- 4. Extracting the useful information
--
-- = Adding New Tools
--
-- To add a typed wrapper for a new tool:
--
-- 1. Define the input type with FromJSON
-- 2. Create the CfTool schema
-- 3. Write a smart constructor that:
--    - Calls llmCall with the tool
--    - Handles NeedsTools by yielding appropriate effects
--    - Returns the typed result
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
import Data.Maybe (fromMaybe)
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
-- Handles the full flow:
-- 1. Parses the tool input
-- 2. Yields TelegramAsk effect
-- 3. Returns the selected option or typed text
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
      -- For now, show as single "Continue" button that user can ignore
      -- and type instead
      result <- telegramAsk auiQuestion [("Type your response", "freeform")]
      case result of
        TelegramButton _ -> pure ""  -- They clicked instead of typing
        TelegramText txt -> pure txt
        TelegramStaleButton -> do
          logInfo $ "Stale button clicked for freeform question: " <> auiQuestion
          pure ""
    Just opts -> do
      -- With options = inline keyboard buttons
      let buttons = [(opt, opt) | opt <- opts]  -- label = callback data
      result <- telegramAsk auiQuestion buttons
      case result of
        TelegramButton response -> pure response  -- The callback_data = option text
        TelegramText txt -> pure txt  -- User typed instead of clicking
        TelegramStaleButton -> do
          logInfo $ "Stale button clicked for question: " <> auiQuestion
          pure ""  -- Stale button, treat as empty


-- | Ask user with a fallback for when no options are provided.
--
-- If the LLM provides options, shows them as buttons.
-- If no options, shows the fallback options instead.
askUserWithFallback
  :: Member (Yield SerializableEffect EffectResult) effs
  => [Text]  -- ^ Fallback options if LLM doesn't provide any
  -> AskUserInput
  -> Eff effs Text
askUserWithFallback fallbackOpts input =
  askUser input { auiOptions = Just $ fromMaybe fallbackOpts input.auiOptions }


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
    logInfo $ "[ERROR] " <> err
    error $ T.unpack err


-- ════════════════════════════════════════════════════════════════════════════
-- COMBINED TOOL EXECUTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Execute an ask_user tool call from a WireToolCall.
--
-- This is the most convenient way to handle ask_user:
--
-- @
-- case lcrToolCalls of
--   [tc] | tc.wtcName == "ask_user" -> do
--     result <- executeAskUser tc
--     -- result is Right Text (user's selection) or Left Text (error)
--   ...
-- @
executeAskUser
  :: Member (Yield SerializableEffect EffectResult) effs
  => WireToolCall
  -> Eff effs (Either Text Text)
executeAskUser tc = case parseToolInput tc of
  Left err -> pure $ Left err
  Right input -> Right <$> askUser input
