{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
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
--   :: (Member LLMCall r, Member Log r)
--   => Document
--   -> Sem r (Either CallError Report)
-- analyzeDoc doc = do
--   logInfo "Analyzing document..."
--
--   let cfg = defaultLLM \@Report & model Opus
--
--   callNoTools cfg (System systemPrompt) (User $ render doc)
-- @
module ExoMonad.LLM.Effect
  ( -- * Effect Type
    LLMCall (..),

    -- * Main Functions
    call,
    callNoTools,

    -- * Re-exports
    System (..),
    User (..),
    CallConfig,
    CallError (..),
    NoTools,
    Model (..),
  )
where

import Polysemy (Sem, Member, makeSem)
import Polysemy.Error (Error, throw)
import Data.Aeson (Value)
import Data.Kind (Type)
import ExoMonad.LLM.Types
import ExoMonad.StructuredOutput (StructuredOutput (..))

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | High-level LLM call effect with structured output and tool support.
--
-- This effect represents a single "call" to an LLM, abstracting away
-- the underlying provider, history, and retry logic.
--
-- Interpreters (like runLLMCall) handle the complexities of converting
-- to provider-specific formats and parsing responses.
--
-- Operations throw 'CallError' via Polysemy Error effect.
data LLMCall m a where
  -- | Make an LLM call (no tools).
  PerformLLMCall ::
    (StructuredOutput out) =>
    -- | Model to use
    Model ->
    -- | Max tokens (Nothing = default)
    Maybe Int ->
    -- | System prompt
    System ->
    -- | User prompt
    User ->
    -- | Output schema (JSON)
    Value ->
    LLMCall m out
  -- | Make an LLM call with tools.
  PerformLLMCallWithTools ::
    (StructuredOutput out) =>
    -- | Model to use
    Model ->
    -- | Max tokens (Nothing = default)
    Maybe Int ->
    -- | System prompt
    System ->
    -- | User prompt
    User ->
    -- | Output schema (JSON)
    Value ->
    -- | Tool schemas (Anthropic format)
    [Value] ->
    LLMCall m out

makeSem ''LLMCall

-- ════════════════════════════════════════════════════════════════════════════
-- MAIN FUNCTIONS
-- ════════════════════════════════════════════════════════════════════════════

-- | Make an LLM call with tools.
--
-- This is the primary function for invoking an LLM from effectful code
-- when you need tool support. The config carries the output type and
-- tool record.
--
-- Throws 'CallError' on failure.
call ::
  forall out tools r.
  ( Member LLMCall r,
    Member (Error CallError) r,
    StructuredOutput out,
    ToolRecord tools
  ) =>
  CallConfig out (tools r) ->
  System ->
  User ->
  Sem r out
call cfg sys usr =
  let toolSchemaValues =
        map (anthropicToolToJSON . toolSchemaToAnthropicTool) $
          toolSchemas (Proxy @tools)
      schema = schemaToValue (structuredSchema @out)
   in performLLMCallWithTools
        cfg.ccModel
        cfg.ccMaxTokens
        sys
        usr
        schema
        toolSchemaValues

-- | Make an LLM call without tools.
--
-- Throws 'CallError' on failure.
callNoTools ::
  forall out r.
  ( Member LLMCall r,
    Member (Error CallError) r,
    StructuredOutput out
  ) =>
  CallConfig out NoTools ->
  System ->
  User ->
  Sem r out
callNoTools cfg sys usr =
  let schema = schemaToValue (structuredSchema @out)
   in performLLMCall
        cfg.ccModel
        cfg.ccMaxTokens
        sys
        usr
        schema

