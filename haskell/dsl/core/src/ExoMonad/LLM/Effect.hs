{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | LLM call effect for typed LLM invocations.
--
-- This module defines the 'LLMCall' effect for making structured LLM calls
-- from within tool handlers or other effectful code.
--
-- = Design
--
-- The effect provides a single function 'call' that:
--
-- 1. Takes a config (with output type and optional tools)
-- 2. Takes system and user prompts
-- 3. Returns structured output (or error)
--
-- The interpreter (see "ExoMonad.LLM.Interpret") handles:
--
-- * Nudge loops (schema conformance retries)
-- * Tool use loops (multi-turn when tools are invoked)
-- * Error handling and parsing
--
-- = Usage
--
-- @
-- analyzeDoc
--   :: (Member LLMCall es, Member Log es)
--   => Document
--   -> Eff es (Either CallError Report)
-- analyzeDoc doc = do
--   logInfo "Analyzing document..."
--
--   let cfg = defaultLLM \@Report & model Opus
--
--   callNoTools cfg (System systemPrompt) (User $ render doc)
-- @
module ExoMonad.LLM.Effect
  ( -- * Effect Type
    LLMCall(..)

    -- * Main Function
  , call
  , callNoTools

    -- * Re-exports
  , System(..)
  , User(..)
  , CallConfig
  , CallError(..)
  , NoTools
  , Model(..)
  ) where

import Control.Monad.Freer (Eff, Member, send)
import Data.Aeson (Value)
import Data.Proxy (Proxy(..))

import ExoMonad.LLM.Types
import ExoMonad.LLM.Tools (ToolRecord(..), toolSchemaToAnthropicTool)
import ExoMonad.Schema (schemaToValue)
import ExoMonad.StructuredOutput (StructuredOutput(..))
import ExoMonad.Tool.Wire (anthropicToolToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT TYPE
-- ════════════════════════════════════════════════════════════════════════════

-- | The LLM call effect.
--
-- This is a GADT that captures all the information needed to make an LLM call.
-- The interpreter receives this and handles the actual API interaction,
-- including tool loops and nudge retries.
--
-- Note: Tool dispatch is handled internally by the interpreter using the
-- schemas. The tools record is passed separately to allow the interpreter
-- to call tool handlers during the tool use loop.
data LLMCall a where
  -- | Make an LLM call (no tools).
  --
  -- The interpreter handles:
  --
  -- * Building the API request
  -- * Parsing structured output
  -- * Nudge retries (if output doesn't match schema)
  CallLLMNoTools
    :: StructuredOutput out
    => Model                           -- ^ Model to use
    -> Maybe Int                       -- ^ Max tokens (Nothing = default)
    -> System                          -- ^ System prompt
    -> User                            -- ^ User prompt
    -> Value                           -- ^ Output schema (JSON)
    -> LLMCall (Either CallError out)

  -- | Make an LLM call with tools.
  --
  -- The interpreter handles:
  --
  -- * Building the API request with tool schemas
  -- * Running tool loops (if LLM invokes tools)
  -- * Parsing structured output
  -- * Nudge retries (if output doesn't match schema)
  CallLLMWithTools
    :: StructuredOutput out
    => Model                           -- ^ Model to use
    -> Maybe Int                       -- ^ Max tokens (Nothing = default)
    -> System                          -- ^ System prompt
    -> User                            -- ^ User prompt
    -> Value                           -- ^ Output schema (JSON)
    -> [Value]                         -- ^ Tool schemas (Anthropic format)
    -> LLMCall (Either CallError out)


-- ════════════════════════════════════════════════════════════════════════════
-- MAIN FUNCTIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Make an LLM call with tools.
--
-- This is the primary function for invoking an LLM from effectful code
-- when you need tool support. The config carries the output type and
-- tool record.
--
-- Note: The tool record itself is not passed to the effect; only the
-- schemas are extracted. Tool dispatch happens at the interpreter level
-- where you must provide the actual tool record for execution.
--
-- @
-- let cfg = defaultLLM \@Report
--       & model Opus
--       & tools MyTools { search = searchHandler }
--
-- -- In practice, use the interpreter that accepts the tool record:
-- -- runLLMCallWithTools cfg.ccTools $ call cfg sys usr
-- result <- call cfg (System sysPrompt) (User userPrompt)
-- @
call
  :: forall out tools es.
     ( Member LLMCall es
     , StructuredOutput out
     , ToolRecord tools
     )
  => CallConfig out (tools es)
  -> System
  -> User
  -> Eff es (Either CallError out)
call cfg sys usr =
  let toolSchemaValues = map (anthropicToolToJSON . toolSchemaToAnthropicTool)
                            $ toolSchemas (Proxy @tools)
      schema = schemaToValue (structuredSchema @out)
  in send $ CallLLMWithTools
       cfg.ccModel
       cfg.ccMaxTokens
       sys
       usr
       schema
       toolSchemaValues

-- | Make an LLM call without tools.
--
-- Simplified version for calls that don't need tool support.
--
-- @
-- let cfg = defaultLLM \@Classification & model Haiku
-- result <- callNoTools cfg (System sysPrompt) (User userPrompt)
-- @
callNoTools
  :: forall out es.
     ( Member LLMCall es
     , StructuredOutput out
     )
  => CallConfig out NoTools
  -> System
  -> User
  -> Eff es (Either CallError out)
callNoTools cfg sys usr =
  let schema = schemaToValue (structuredSchema @out)
  in send $ CallLLMNoTools
       cfg.ccModel
       cfg.ccMaxTokens
       sys
       usr
       schema
