{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GADTs #-}

-- | Graph definition for semantic code exploration.
--
-- This implements the exploration as a proper Tidepool graph node with:
--   * Self-looping exploration via @Goto Self@
--   * @Memory@ for frontier/visited state
--   * FunctionGemma as coalgebra: @(Query, Edge) → Rubric@
--   * Priority queue ordering by composite score
--
-- Architecture:
--
-- @
-- ┌─────────────────────────────────────────────────────────────────┐
-- │  ScoutGraph (self-looping)                                      │
-- │                                                                 │
-- │  entry ──▶ explore ◀──┐                                         │
-- │              │        │ Goto Self (more edges to score)         │
-- │              │        │                                         │
-- │              ├────────┘                                         │
-- │              │                                                  │
-- │              ▼                                                  │
-- │            exit ──▶ ScoutResponse                               │
-- └─────────────────────────────────────────────────────────────────┘
-- @
module Tidepool.Control.Scout.Graph
  ( -- * Graph Definition
    ScoutGraph(..)

    -- * Input/Output Types
  , ExploreInput(..)
  , ExploreMemory(..)
  , initialMemory

    -- * Handler Definition
  , scoutHandlers
  , exploreHandler
  ) where

import Control.Monad.Freer (Eff)
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy(..))
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Tidepool.Graph.Types (type (:@), Input, UsesEffects, Exit, Self)
import Tidepool.Graph.Generic (GraphMode(..), AsHandler)
import qualified Tidepool.Graph.Generic as G
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoExit, gotoSelf)

import Tidepool.Control.Scout.Types
  ( ScoutQuery(..), ScoutResponse(..), Pointer(..)
  , QueryContext(..), Rubric(..), Tag(..)
  , depthToBudget
  )
import Tidepool.Control.Scout.EdgeTypes (EdgeContext(..))
import Tidepool.Control.Scout.Scoring (Frontier, emptyFrontier, pushEdge, popEdge, ScoredEdge(..))
import Tidepool.Training.Types (ScoreEdgeOutput(..))


-- ════════════════════════════════════════════════════════════════════════════
-- GRAPH DEFINITION
-- ════════════════════════════════════════════════════════════════════════════

-- | Semantic scout as a Tidepool graph.
--
-- The exploration is a self-looping LogicNode that:
--   1. Pops the highest-scoring edge from the priority queue
--   2. Fetches context via LSP (handled externally)
--   3. Scores with FunctionGemma (handled externally)
--   4. Pushes new edges to frontier
--   5. Either loops (more edges) or exits (budget exhausted)
data ScoutGraph mode = ScoutGraph
  { sgEntry   :: mode :- G.EntryNode ScoutQuery
  , sgExplore :: mode :- G.LogicNode
      :@ Input ExploreInput
      :@ UsesEffects '[Goto Self ExploreInput, Goto Exit ScoutResponse]
  , sgExit    :: mode :- G.ExitNode ScoutResponse
  }
  deriving Generic


-- ════════════════════════════════════════════════════════════════════════════
-- INPUT/STATE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Input to the explore node.
--
-- Contains the query context and optionally new edges to add to frontier.
data ExploreInput = ExploreInput
  { eiQuery     :: QueryContext
    -- ^ The original query for relevance scoring
  , eiNewEdges  :: [ScoredEdge]
    -- ^ New scored edges to add to frontier (from external LSP/Gemma calls)
  , eiMemory    :: ExploreMemory
    -- ^ Current exploration state (passed explicitly for stateless handler)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Memory state for exploration.
--
-- Persists across self-loop iterations.
data ExploreMemory = ExploreMemory
  { emFrontier :: Frontier
    -- ^ Priority queue of edges to explore
  , emVisited  :: Set Text
    -- ^ Visited locations (to avoid cycles)
  , emResults  :: [ScoredEdge]
    -- ^ Accumulated results (scored edges we've processed)
  , emBudget   :: Int
    -- ^ Remaining exploration budget
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Initial memory state for a given query.
initialMemory :: ScoutQuery -> [ScoredEdge] -> ExploreMemory
initialMemory query initialEdges = ExploreMemory
  { emFrontier = foldr pushEdge emptyFrontier initialEdges
  , emVisited = Set.empty
  , emResults = []
  , emBudget = depthToBudget (sqDepth query)
  }


-- ════════════════════════════════════════════════════════════════════════════
-- HANDLERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Handlers for the scout graph.
--
-- Note: This uses a stateless design where memory is passed via input.
-- The explore handler is pure - it doesn't use the Memory effect.
-- Instead, memory is threaded through the input/output.
scoutHandlers :: ScoutGraph (AsHandler '[])
scoutHandlers = ScoutGraph
  { sgEntry   = Proxy @ScoutQuery
  , sgExplore = exploreHandler
  , sgExit    = Proxy @ScoutResponse
  }


-- | The exploration handler.
--
-- Logic:
--   1. Add any new edges from input to frontier
--   2. Check termination conditions (budget <= 0, frontier empty)
--   3. Pop highest-scoring edge
--   4. Skip if already visited, else record it
--   5. Return: either self-loop (continue) or exit (done)
--
-- Note: LSP calls and Gemma scoring happen OUTSIDE this handler.
-- The caller is responsible for:
--   1. Calling LSP to get references
--   2. Calling Gemma to score edges
--   3. Passing scored edges via eiNewEdges
--
-- This separation keeps the graph handler pure (no LSP/Gemma effects).
exploreHandler
  :: ExploreInput
  -> Eff '[] (GotoChoice '[To Self ExploreInput, To Exit ScoutResponse])
exploreHandler input = do
  let mem = input.eiMemory

  -- Add new edges from input to frontier
  let frontierWithNew = foldr pushEdge mem.emFrontier input.eiNewEdges
  let mem' = mem { emFrontier = frontierWithNew }

  -- Check termination: budget exhausted
  if mem'.emBudget <= 0
    then exitWithResults input.eiQuery mem'
    else case popEdge frontierWithNew of
      -- Frontier empty
      Nothing -> exitWithResults input.eiQuery mem'

      -- Pop next edge
      Just (edge, newFrontier) -> do
        let loc = ecLocation edge.seEdge

        -- Skip if already visited
        if loc `Set.member` mem'.emVisited
          then do
            let mem'' = mem' { emFrontier = newFrontier }
            pure $ gotoSelf input { eiNewEdges = [], eiMemory = mem'' }
          else do
            -- Record visit
            let mem'' = mem'
                  { emFrontier = newFrontier
                  , emVisited = Set.insert loc mem'.emVisited
                  , emResults = edge : mem'.emResults
                  , emBudget = mem'.emBudget - 1
                  }

            -- Continue exploration (caller will provide new edges)
            pure $ gotoSelf input { eiNewEdges = [], eiMemory = mem'' }


-- | Build response and exit.
exitWithResults
  :: QueryContext
  -> ExploreMemory
  -> Eff '[] (GotoChoice '[To Self ExploreInput, To Exit ScoutResponse])
exitWithResults query mem = do
  let pointers = map toPointer (reverse mem.emResults)
  let response = ScoutResponse
        { srSummary = buildSummary query pointers
        , srPointers = pointers
        , srNodesVisited = length mem.emResults
        , srTrainingExamples = []  -- TODO: Generate training examples
        }
  pure $ gotoExit response


-- | Convert scored edge to pointer.
--
-- Uses the boolean outputs to derive risk/relevance values:
--   - relevance: 5 if query_relevant, else 2
--   - risk: 5 if breaking_boundary OR public_contract, else 2
toPointer :: ScoredEdge -> Pointer
toPointer se = Pointer
  { pLocation = ecLocation se.seEdge
  , pWhat = T.take 60 (ecHover se.seEdge)
  , pRisk = if seoIsBreakingBoundary bools || seoIsPublicContract bools then 5 else 2
  , pRelevance = if seoIsQueryRelevant bools then 5 else 2
  , pTags = boolsToTags bools
  , pAction = suggestActionFromBools bools
  }
  where
    bools = se.seBools

-- | Convert boolean outputs to Tag list.
boolsToTags :: ScoreEdgeOutput -> [Tag]
boolsToTags bools = catMaybes
  [ if seoIsBreakingBoundary bools then Just Exhaustive else Nothing
  , if seoIsPublicContract bools then Just Implementation else Nothing
  ]

-- | Suggest action based on boolean outputs.
suggestActionFromBools :: ScoreEdgeOutput -> Maybe Text
suggestActionFromBools bools
  | seoIsBreakingBoundary bools = Just "Review carefully - breaking boundary"
  | seoIsPublicContract bools = Just "Part of public API - changes have external impact"
  | otherwise = Nothing


-- | Build summary text.
buildSummary :: QueryContext -> [Pointer] -> Text
buildSummary query pointers = T.unlines
  [ "## Exploration Summary"
  , ""
  , "Query: " <> qcQuery query
  , ""
  , "Found " <> T.pack (show (length pointers)) <> " relevant locations."
  ]
