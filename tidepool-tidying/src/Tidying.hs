-- | Tidying Agent
--
-- A prosthetic executive function for tackling overwhelming spaces.
-- Third use case for the Tidepool effect library.
--
-- = Design
--
-- OODA-inspired loop:
--
-- * OBSERVE: User sends photos/text (analyzePhotos)
-- * ORIENT: LLM extracts structured facts (extractFromInput)
-- * DECIDE: Pure routing (decideFromExtract)
-- * ACT: Generate response (actResponse)
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

    -- * Agent
  , tidying
  , tidyingRun

    -- * State
  , SessionState(..)
  , Phase(..)
  , Piles(..)
  , newSession

    -- * Input
  , UserInput(..)
  , Photo(..)

    -- * Action (DECIDE output)
  , Action(..)

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

    -- * Question DSL (from Tidepool.Question)
    -- For ItemDisposition type, import Tidepool.Question directly
  , Question(..)
  , Answer(..)

    -- * Tools (Tidying.Tools)
  , tidyingTools
  , makeTidyingDispatcher
  ) where

import Tidying.State
import Tidying.Action
import Tidying.Loop
import Tidying.Agent (tidying, tidyingRun)
import Tidying.Context
import Tidying.Output
import Tidying.Templates
import Tidepool.Question (Question(..), Answer(..))
import Tidying.Tools (tidyingTools, makeTidyingDispatcher)
