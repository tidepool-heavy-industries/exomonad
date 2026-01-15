{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- | Gemma effect for semantic code scoring.
--
-- This effect abstracts over the FunctionGemma 270M model which scores
-- code locations for relevance, risk, and complexity.
--
-- Architecture:
--
-- @
-- ┌─────────────────────────────────────────┐
-- │  Exploration Loop                        │
-- │    │                                     │
-- │    ▼                                     │
-- │  rateNode :: NodeContext -> Gemma Rubric │
-- │    │                                     │
-- └────┼─────────────────────────────────────┘
--      │
--      ▼
-- ┌─────────────────────────────────────────┐
-- │  Interpreter (pluggable)                 │
-- │                                         │
-- │  • runGemmaMock  - hardcoded rubric     │
-- │  • runGemmaHTTP  - mistral.rs server    │
-- └─────────────────────────────────────────┘
-- @
--
-- The mock interpreter is useful for:
--   * Testing exploration logic
--   * Development without local model
--   * Generating training data with heuristic labels
module Tidepool.Agents.Scout.Gemma
  ( -- * Effect
    Gemma(..)

    -- * Smart Constructors
  , rateNode

    -- * Interpreters
  , runGemmaMock
  , runGemmaHeuristic
  ) where

import Control.Monad.Freer (Eff, Member, send, interpret)

import Tidepool.Agents.Scout.Types (QueryContext, NodeContext, Rubric(..))
import Tidepool.Agents.Scout.Heuristics (scoreNode)
import Tidepool.Training.Types (Tag(..))


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Gemma effect for semantic code scoring.
--
-- FunctionGemma is a fine-tuned 270M model that outputs structured Rubrics.
-- This effect allows swapping implementations:
--   * Mock (hardcoded) for testing
--   * Heuristic for baseline
--   * HTTP to mistral.rs server for real inference
data Gemma a where
  -- | Rate a code location given query context.
  --
  -- Input: NodeContext (hover, snippet, location) + QueryContext (query, tags)
  -- Output: Rubric (relevance, risk, complexity, confidence, tags)
  RateNode :: QueryContext -> NodeContext -> Gemma Rubric


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Rate a node using the Gemma model.
rateNode :: Member Gemma effs => QueryContext -> NodeContext -> Eff effs Rubric
rateNode query node = send (RateNode query node)


-- ════════════════════════════════════════════════════════════════════════════
-- MOCK INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Mock interpreter that returns a hardcoded rubric.
--
-- Useful for testing the exploration loop without a real model.
-- Returns a "medium relevance, medium risk" rubric.
runGemmaMock :: Eff (Gemma ': effs) a -> Eff effs a
runGemmaMock = interpret $ \case
  RateNode _query _node -> pure mockRubric

-- | A reasonable default rubric for testing.
mockRubric :: Rubric
mockRubric = Rubric
  { rRelevance  = 3  -- Medium relevance
  , rRisk       = 3  -- Medium risk
  , rComplexity = 2  -- Low-medium complexity
  , rConfidence = 4  -- Reasonably confident
  , rTags       = [PatternMatch, Implementation]  -- Common tags
  }


-- ════════════════════════════════════════════════════════════════════════════
-- HEURISTIC INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Heuristic interpreter using deterministic rules.
--
-- This is the baseline that FunctionGemma should improve upon.
-- Uses pattern matching on code snippets and hover info.
runGemmaHeuristic :: Eff (Gemma ': effs) a -> Eff effs a
runGemmaHeuristic = interpret $ \case
  RateNode query node -> pure $ scoreNode query node
