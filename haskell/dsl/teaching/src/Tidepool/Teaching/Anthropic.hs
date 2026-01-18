{-# LANGUAGE OverloadedStrings #-}

-- | Anthropic Messages API client for teaching mode
--
-- This module re-exports the production Anthropic client types and provides
-- helper functions for teaching-specific workflows.
--
-- The existing @tidepool-llm-interpreter@ package already provides everything
-- we need for teaching mode:
-- - Full tool use support
-- - Content block parsing (text + tool_use)
-- - Haiku model support
--
-- = Usage Pattern
--
-- For teaching mode, use the LLM effect with the Anthropic interpreter:
--
-- @
-- import Tidepool.Teaching.Anthropic
-- import Tidepool.LLM.Interpreter (runLLMComplete, mkLLMEnv)
-- import Tidepool.LLM.Types (LLMConfig(..), AnthropicSecrets(..))
-- import Tidepool.Effects.LLMProvider (complete, SAnthropic)
--
-- -- Setup
-- let config = LLMConfig
--       { lcAnthropicSecrets = Just $ AnthropicSecrets
--           { asApiKey = teachingConfig.tcAnthropicKey
--           , asBaseUrl = Nothing
--           }
--       , lcOpenAISecrets = Nothing
--       }
-- env <- mkLLMEnv config
--
-- -- Build config with teacher guidance
-- let anthropicCfg = AnthropicConfig
--       { acModel = "claude-3-5-haiku-20241022"
--       , acMaxTokens = 1024
--       , acThinking = ThinkingDisabled  -- Direct tool calls
--       , acSystemPrompt = Just (baseSystemPrompt <> "\\n\\n" <> teacherGuidance \@TeachGemma)
--       }
--
-- -- Call with tools
-- response <- runM $ runLLMComplete env $ do
--   complete SAnthropic anthropicCfg userPrompt (Just tools)
--
-- -- Extract teaching turn
-- case extractTeachingTurn response of
--   Left err -> error err
--   Right (reasoning, toolName, args) -> ...
-- @
module Tidepool.Teaching.Anthropic
  ( -- * Re-exports from tidepool-llm-interpreter
    module Tidepool.LLM.Interpreter
  , module Tidepool.LLM.Types

    -- * Re-exports from tidepool-core
  , AnthropicConfig(..)
  , AnthropicResponse(..)
  , ContentBlock(..)
  , Usage(..)
  , ThinkingBudget(..)

    -- * Teaching-Specific Helpers
  , extractTeachingTurn
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import Tidepool.Effects.LLMProvider
  ( AnthropicConfig(..)
  , AnthropicResponse(..)
  , ContentBlock(..)
  , Usage(..)
  , ThinkingBudget(..)
  )
import Tidepool.LLM.Interpreter
import Tidepool.LLM.Types

-- | Extract teaching turn from Anthropic response
--
-- Parses the response to extract:
-- - Reasoning text (from text blocks)
-- - Tool name and arguments (from tool_use block)
--
-- Returns:
-- - Left error if no tool use found or multiple tool calls
-- - Right (reasoning, toolName, toolArgs)
--
-- Example:
-- @
-- case extractTeachingTurn response of
--   Left err -> error $ "No tool use: " <> T.unpack err
--   Right (reasoning, toolName, args) ->
--     -- reasoning: "Let me analyze the code structure..."
--     -- toolName: "select_symbols"
--     -- args: {"selected": ["Foo", "Bar"]}
-- @
extractTeachingTurn :: AnthropicResponse -> Either Text (Text, Text, Value)
extractTeachingTurn (AnthropicResponse contentBlocks _ _) =
  let textBlocks = [t | TextContent t <- contentBlocks]
      toolUseBlocks = [(name, input_) | ToolUseContent name input_ <- contentBlocks]
  in case toolUseBlocks of
    [] -> Left "No tool use in response (expected exactly one tool call)"
    [_,_] -> Left "Multiple tool calls in response (expected exactly one)"
    (_:_:_:_) -> Left "Multiple tool calls in response (expected exactly one)"
    [(toolName, toolArgs)] ->
      let reasoning = T.intercalate "\n" textBlocks
      in Right (reasoning, toolName, toolArgs)
