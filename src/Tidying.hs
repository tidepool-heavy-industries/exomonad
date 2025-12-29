{-# LANGUAGE OverloadedStrings #-}

-- | Tidying Agent
--
-- A prosthetic executive function for tackling overwhelming spaces.
-- Third use case for the Tidepool effect library.
--
-- = Design
--
-- OODA-inspired loop:
--
-- * OBSERVE: User sends photos/text
-- * ORIENT: LLM classifies situation (template)
-- * DECIDE: Pure routing (state machine)
-- * ACT: Generate response (canned or template)
--
-- = Key insight
--
-- No upfront schema. Structure emerges from doing:
--
-- 1. Function + anchors (what's the space FOR, what STAYS)
-- 2. belongs / out / unsure (initial sort)
-- 3. Dynamic splits (when unsure grows)
-- 4. Refinement (place keepers)
--
-- = Phases
--
-- * 'Surveying' - gather photos, establish function + anchors
-- * 'Sorting' - main loop: classify items, atomic instructions
-- * 'Splitting' - break unsure pile into emergent categories
-- * 'Refining' - work through sub-piles, place keepers
-- * 'DecisionSupport' - help user decide on stuck items
--
-- = Integration with Tidepool
--
-- Uses the Tidepool effect system:
--
-- * 'State SessionState' - session state
-- * 'LLM' - for ORIENT and ACT calls
-- * 'Emit TidyingEvent' - for logging/debugging
-- * 'Log' - structured logging
--
-- Photo analysis is stubbed for now (requires vision support).

module Tidying
  ( -- * Main loop (effect-based)
    tidyingTurn
  , Response(..)
  , TidyingEvent(..)

    -- * Running sessions
  , runTidyingSession

    -- * State
  , SessionState(..)
  , Phase(..)
  , Piles(..)
  , newSession

    -- * Input
  , UserInput(..)
  , Photo(..)

    -- * Situation (ORIENT output)
  , Situation(..)
  , ItemClass(..)

    -- * Action (DECIDE output)
  , Action(..)

    -- * Pure routing
  , decide

    -- * Context (for templates)
  , TidyingContext(..)
  , PhotoAnalysis(..)
  , buildTidyingContext
  , stubPhotoAnalysis

    -- * Output types (LLM structured output)
  , OrientOutput(..)
  , ActOutput(..)
  , orientOutputSchema
  , actOutputSchema

    -- * Templates
  , renderOrientPrompt
  , renderActPrompt
  ) where

import Tidying.State
import Tidying.Situation
import Tidying.Action
import Tidying.Decide
import Tidying.Loop
import Tidying.Agent (runTidyingSession)
import Tidying.Context
import Tidying.Output
import Tidying.Templates
