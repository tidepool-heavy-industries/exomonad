-- | Core types for typed LLM invocation.
--
-- This module provides the building blocks for type-safe LLM calls
-- within tool handlers. Unlike the graph machinery, these types are
-- standalone and can be used directly.
--
-- = Design
--
-- Two separate concerns:
--
-- 1. Template rendering - @render \@Tpl ctx -> Text@ (reusable for GH issues, PRs, etc.)
-- 2. LLM invocation - @call cfg (System text) (User text)@
--
-- = Usage
--
-- @
-- analyzeDoc doc = do
--   sysText <- render @AnalyzeSysTpl sysCtx
--   userText <- render @AnalyzeUserTpl (AnalyzeCtx doc)
--
--   let cfg = defaultLLM @Report
--         & model Opus
--         & tools AnalysisTools { search = \args -> searchIn doc args }
--
--   call cfg (System sysText) (User userText)
-- @
module ExoMonad.LLM.Types
  ( -- * Prompt Wrappers
    System(..)
  , User(..)

    -- * Model Selection
  , Model(..)
  , modelToText

    -- * LLM Configuration
  , CallConfig(..)
  , NoTools

    -- * Error Types
  , CallError(..)
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- PROMPT WRAPPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | System prompt wrapper.
--
-- Provides explicit typing for system prompts, preventing positional confusion.
--
-- @
-- call cfg (System "You are a helpful assistant.") (User "Hello!")
-- @
newtype System = System { getSystem :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | User prompt wrapper.
--
-- Provides explicit typing for user prompts, preventing positional confusion.
--
-- @
-- call cfg (System sysPrompt) (User userPrompt)
-- @
newtype User = User { getUser :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- MODEL SELECTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Available LLM models.
--
-- These correspond to Anthropic model tiers, but the design allows
-- for future provider expansion.
data Model
  = Haiku     -- ^ Fast, cost-effective for simple tasks
  | Sonnet    -- ^ Balanced performance and cost (default)
  | Opus      -- ^ Most capable, for complex reasoning
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Convert model to API string.
--
-- Maps our model enum to the actual Anthropic model identifier.
modelToText :: Model -> Text
modelToText = \case
  Haiku  -> "claude-3-5-haiku-latest"
  Sonnet -> "claude-sonnet-4-20250514"
  Opus   -> "claude-opus-4-20250514"


-- ════════════════════════════════════════════════════════════════════════════
-- LLM CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Marker type for configs with no tools.
--
-- This is an uninhabited type used only at the type level to indicate
-- that a 'CallConfig' has no tools attached.
data NoTools

-- | Configuration for an LLM call.
--
-- This config carries the output type as a phantom parameter and
-- optionally holds tools. The builder pattern is used for ergonomic
-- construction:
--
-- @
-- let cfg = defaultLLM \@Report
--       & model Opus
--       & temp 0.7
--       & maxTokens 4096
-- @
--
-- Type parameters:
--
-- * @out@ - The expected structured output type (phantom)
-- * @tools@ - Tool record type, or 'NoTools' if none
data CallConfig (out :: *) (tools :: *)  = CallConfig
  { ccModel       :: Model
    -- ^ Which model to use (default: Sonnet)
  , ccTemperature :: Maybe Double
    -- ^ Temperature setting (default: provider default)
  , ccMaxTokens   :: Maybe Int
    -- ^ Maximum tokens to generate (default: provider default)
  , ccTools       :: Maybe tools
    -- ^ Tool record if any
  }
  deriving stock (Generic)


-- ════════════════════════════════════════════════════════════════════════════
-- ERROR TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Errors that can occur during LLM invocation.
data CallError
  = CallRateLimited
    -- ^ Rate limit hit, retry later
  | CallTimeout
    -- ^ Request timed out
  | CallContextTooLong
    -- ^ Input too long for context window
  | CallParseFailed Text
    -- ^ Schema parsing failed (includes error message)
  | CallNetworkError Text
    -- ^ Network/connection failure
  | CallUnauthorized
    -- ^ Invalid API key
  | CallApiError Text Text
    -- ^ API error (type, message)
  | CallToolError Text Text
    -- ^ Tool execution error (tool name, error message)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
