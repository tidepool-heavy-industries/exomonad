{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- | Gemma effect for semantic code scoring.
--
-- FunctionGemma is the coalgebra in the exploration anamorphism:
--   coalgebra :: (Query, Edge) → Rubric
--
-- The model handles one edge at a time; the exploration loop handles recursion.
--
-- Architecture:
--
-- @
-- ┌─────────────────────────────────────────┐
-- │  Exploration Loop                        │
-- │    │                                     │
-- │    ▼                                     │
-- │  rateEdge :: EdgeContext -> Gemma Rubric │
-- │    │                                     │
-- └────┼─────────────────────────────────────┘
--      │
--      ▼
-- ┌─────────────────────────────────────────┐
-- │  Interpreter (pluggable)                 │
-- │                                         │
-- │  • runGemmaStub      - formats template, returns heuristic
-- │  • runGemmaHeuristic - pattern-based rules
-- │  • runGemmaMock      - hardcoded rubric (testing)
-- │  • runGemmaHTTP      - mistral.rs server (future)
-- └─────────────────────────────────────────┘
-- @
module Tidepool.Control.Scout.Gemma
  ( -- * Effect
    Gemma(..)

    -- * Smart Constructors
  , rateEdge
  , rateNode  -- Legacy, for compatibility

    -- * Interpreters
  , runGemmaStub
  , runGemmaHeuristic
  , runGemmaMock
  ) where

import Control.Monad.Freer (Eff, Member, send, interpret)
import Data.Text (Text)

import Tidepool.Control.Scout.Types (QueryContext(..), Rubric(..))
import Tidepool.Control.Scout.EdgeTypes (EdgeContext(..))
import Tidepool.Control.Scout.Templates (ScoringContext, mkScoringContext, renderScoringPrompt)
import Tidepool.Control.Scout.Heuristics (scoreNode, scoreEdge)
import Tidepool.Training.Types (Tag(..), NodeContext(..))


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Gemma effect for semantic code scoring.
--
-- FunctionGemma is a fine-tuned 270M model that outputs structured Rubrics.
-- This effect allows swapping implementations:
--   * Stub: formats template, returns heuristic (development)
--   * Heuristic: pattern-based rules (baseline)
--   * Mock: hardcoded rubric (testing)
--   * HTTP: mistral.rs server (production, future)
data Gemma a where
  -- | Rate an edge given query context.
  --
  -- Input: EdgeContext (typed edge with hover, snippet, location)
  --        + query text
  -- Output: Rubric (relevance, risk, complexity, confidence, tags)
  RateEdge :: Text -> EdgeContext -> Gemma Rubric

  -- | Legacy: Rate a node (for backward compatibility).
  RateNode :: QueryContext -> NodeContext -> Gemma Rubric


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Rate an edge using the Gemma model.
--
-- This is the coalgebra: (Query, Edge) → Rubric
rateEdge :: Member Gemma effs => Text -> EdgeContext -> Eff effs Rubric
rateEdge query edge = send (RateEdge query edge)

-- | Legacy: Rate a node using the Gemma model.
rateNode :: Member Gemma effs => QueryContext -> NodeContext -> Eff effs Rubric
rateNode query node = send (RateNode query node)


-- ════════════════════════════════════════════════════════════════════════════
-- STUB INTERPRETER (Development)
-- ════════════════════════════════════════════════════════════════════════════

-- | Stub interpreter that formats the template but returns heuristic results.
--
-- This is the main development interpreter. It:
--   1. Renders the full FunctionGemma prompt (for debugging/logging)
--   2. Returns heuristic-based rubric (no model call)
--
-- Useful for:
--   * Developing the exploration loop
--   * Debugging prompt rendering
--   * Testing without model dependency
runGemmaStub :: Eff (Gemma ': effs) a -> Eff effs a
runGemmaStub = interpret $ \case
  RateEdge query edge -> do
    -- Format the prompt (could log this for debugging)
    let ctx = mkScoringContext query edge
    let _prompt = renderScoringPrompt ctx  -- TODO: Log this

    -- Return heuristic-based rubric
    pure $ scoreEdge query edge

  RateNode query node -> pure $ scoreNode query node


-- ════════════════════════════════════════════════════════════════════════════
-- HEURISTIC INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Heuristic interpreter using deterministic rules.
--
-- This is the baseline that FunctionGemma should improve upon.
-- Uses pattern matching on code snippets and hover info.
runGemmaHeuristic :: Eff (Gemma ': effs) a -> Eff effs a
runGemmaHeuristic = interpret $ \case
  RateEdge query edge -> pure $ scoreEdge query edge
  RateNode query node -> pure $ scoreNode query node


-- ════════════════════════════════════════════════════════════════════════════
-- MOCK INTERPRETER (Testing)
-- ════════════════════════════════════════════════════════════════════════════

-- | Mock interpreter that returns a hardcoded rubric.
--
-- Useful for testing the exploration loop without any scoring logic.
-- Returns a "medium relevance, medium risk" rubric.
runGemmaMock :: Eff (Gemma ': effs) a -> Eff effs a
runGemmaMock = interpret $ \case
  RateEdge _query _edge -> pure mockRubric
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
-- HTTP INTERPRETER (Future)
-- ════════════════════════════════════════════════════════════════════════════

-- | HTTP interpreter that calls FunctionGemma inference server.
--
-- NOT IMPLEMENTED YET.
--
-- When implemented, this will:
--   1. Render the Turn 1-5 prompt
--   2. POST to inference server
--   3. Parse the rubric from response
--
-- runGemmaHTTP :: Text -> Eff (Gemma ': effs) a -> Eff effs a
-- runGemmaHTTP endpoint = interpret $ \case
--   RateEdge query edge -> do
--     let ctx = mkScoringContext query edge
--     let prompt = renderScoringPrompt ctx
--     -- POST to endpoint, parse response
--     error "Not implemented: HTTP model call"
