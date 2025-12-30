{-# LANGUAGE OverloadedStrings #-}

-- | Tidying Agent
--
-- A prosthetic executive function for tackling overwhelming spaces.
-- Third use case for the Tidepool effect library.
--
-- = Design
--
-- Mode-based turn loop with LLM-driven navigation:
--
-- 1. Mode determines template, tools, and output schema
-- 2. LLM receives system prompt + photos + text + history
-- 3. LLM produces structured output + optional tool calls
-- 4. Output updates mode data; transition tools change mode
--
-- = Key insight
--
-- Mode is a SUM TYPE with data, not an enum. Each mode carries
-- its own context fields. The LLM navigates between modes via
-- transition tools.
--
-- = Modes
--
-- * 'Surveying' - curious, orienting - discover function + anchors
-- * 'Sorting' - terse, directive - process items quickly
-- * 'Clarifying' - patient, descriptive - help identify items
-- * 'DecisionSupport' - gentle, reframing - help with stuck items
-- * 'WindingDown' - warm, factual - wrap up session
--
-- = Integration with Tidepool
--
-- Uses the Tidepool effect system:
--
-- * 'State SessionState' - session state
-- * 'LLM' - for multimodal calls
-- * 'Emit TidyingEvent' - for logging/debugging
-- * 'Log' - structured logging

module Tidying
  ( -- * Main loop (effect-based)
    tidyingTurn
  , Response(..)
  , TidyingEvent(..)

    -- * Agent
  , tidying
  , tidyingRun

    -- * State
  , SessionState(..)
  , Mode(..)
  , SurveyingData(..)
  , SortingData(..)
  , ClarifyingData(..)
  , DecisionSupportData(..)
  , WindingDownData(..)
  , Piles(..)
  , newSession
  , modeName

    -- * Input
  , UserInput(..)
  , Photo(..)

    -- * Context (for templates)
  , TidyingContext(..)
  , PhotoAnalysis(..)
  , buildTidyingContext
  , stubPhotoAnalysis

    -- * Output types (LLM structured output)
  , ActOutput(..)
  , actOutputSchema

    -- * Templates
  , renderActPrompt

    -- * Question DSL (Tidying.Question)
    -- For ItemDisposition type, import Tidying.Question directly
  , Question(..)
  , Answer(..)

    -- * Tools (Tidying.Tools)
  , tidyingTools
  , makeTidyingDispatcher
  ) where

import Tidying.State
import Tidying.Loop
import Tidying.Agent (tidying, tidyingRun)
import Tidying.Context
import Tidying.Output
import Tidying.Templates
import Tidying.Question (Question(..), Answer(..))
import Tidying.Tools (tidyingTools, makeTidyingDispatcher)
