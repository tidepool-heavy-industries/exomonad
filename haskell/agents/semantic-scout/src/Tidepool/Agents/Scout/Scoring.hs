{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

-- | Scoring functions for semantic code exploration.
--
-- Combines FunctionGemma rubrics with heuristics to produce
-- composite scores for priority queue ordering.
module Tidepool.Agents.Scout.Scoring
  ( -- * Composite Scoring
    compositeScore
  , ScoreConfig(..)
  , defaultScoreConfig

    -- * Scored Edge
  , ScoredEdge(..)
  , mkScoredEdge

    -- * Priority Queue Operations
  , Frontier
  , emptyFrontier
  , pushEdge
  , pushEdges
  , popEdge
  , frontierSize
  , frontierNull
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Types (Parser)
import qualified Data.PQueue.Prio.Min as PQ
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Agents.Scout.EdgeTypes (EdgeContext(..), EdgeType(..))
import Tidepool.Training.Types (Rubric(..))


-- | Configuration for composite score calculation.
data ScoreConfig = ScoreConfig
  { scRelevanceWeight :: Double
    -- ^ Weight for relevance score (default: 2.0)
  , scRiskWeight      :: Double
    -- ^ Weight for risk score (default: 1.5)
  , scConfidenceBonus :: Double
    -- ^ Bonus per confidence point (default: 0.5)
  , scDepthPenalty    :: Double
    -- ^ Penalty per depth level (default: 0.3)
  , scEdgeTypeBonus   :: EdgeType -> Double
    -- ^ Bonus by edge type (pattern matches get priority)
  } deriving (Generic)

-- | Default scoring configuration.
defaultScoreConfig :: ScoreConfig
defaultScoreConfig = ScoreConfig
  { scRelevanceWeight = 2.0
  , scRiskWeight      = 1.5
  , scConfidenceBonus = 0.5
  , scDepthPenalty    = 0.3
  , scEdgeTypeBonus   = defaultEdgeTypeBonus
  }

-- | Default edge type bonus.
--
-- Pattern matches and type references get priority since they're
-- most likely to break when adding variants.
defaultEdgeTypeBonus :: EdgeType -> Double
defaultEdgeTypeBonus = \case
  PatternMatchSite -> 2.0   -- High priority: exhaustive matches
  TypeReference    -> 1.5   -- Type definitions
  InstanceSite     -> 1.0   -- Typeclass instances
  ConstructorRef   -> 0.5   -- Constructor usage
  DefinitionSite   -> 0.5   -- Where things are defined
  ValueReference   -> 0.0   -- Normal value refs
  UsageSite        -> 0.0   -- Generic usage
  ImportEdge       -> -1.0  -- Low priority: imports
  ExportEdge       -> -1.0  -- Low priority: exports
  UnknownEdge      -> 0.0   -- No bonus


-- | Calculate composite score from rubric and edge context.
--
-- Higher scores = more relevant, explored first.
--
-- Formula:
--   score = relevance * 2.0
--         + risk * 1.5
--         + confidence * 0.5
--         + edgeTypeBonus
--         - depth * 0.3
compositeScore :: ScoreConfig -> Rubric -> EdgeContext -> Double
compositeScore config rubric edge =
  let relevance  = fromIntegral (rRelevance rubric)
      risk       = fromIntegral (rRisk rubric)
      confidence = fromIntegral (rConfidence rubric)
      depth      = fromIntegral (ecDepth edge)
      typeBonus  = scEdgeTypeBonus config (ecEdgeType edge)
  in relevance * scRelevanceWeight config
   + risk * scRiskWeight config
   + confidence * scConfidenceBonus config
   + typeBonus
   - depth * scDepthPenalty config

-- | Simplified composite score with default config.
compositeScoreDefault :: Rubric -> Int -> Double
compositeScoreDefault rubric depth =
  let relevance  = fromIntegral (rRelevance rubric)
      risk       = fromIntegral (rRisk rubric)
      confidence = fromIntegral (rConfidence rubric)
  in relevance * 2.0
   + risk * 1.5
   + confidence * 0.5
   - fromIntegral depth * 0.3


-- | An edge with its computed score, ready for the priority queue.
data ScoredEdge = ScoredEdge
  { seEdge     :: EdgeContext
    -- ^ The edge context
  , seRubric   :: Rubric
    -- ^ The rubric from FunctionGemma
  , seScore    :: Double
    -- ^ Composite score (higher = more relevant)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Create a scored edge from context and rubric.
mkScoredEdge :: ScoreConfig -> EdgeContext -> Rubric -> ScoredEdge
mkScoredEdge config edge rubric = ScoredEdge
  { seEdge   = edge
  , seRubric = rubric
  , seScore  = compositeScore config rubric edge
  }


-- ════════════════════════════════════════════════════════════════════════════
-- PRIORITY QUEUE (Frontier)
-- ════════════════════════════════════════════════════════════════════════════

-- | Priority queue of edges to explore.
--
-- Uses MinPQueue with negated scores so highest scores come first.
-- (MinPQueue pops smallest priority first; we want largest score first)
newtype Frontier = Frontier (PQ.MinPQueue Double ScoredEdge)
  deriving stock (Show)

-- | Equality for Frontier (compare as sorted lists).
instance Eq Frontier where
  Frontier pq1 == Frontier pq2 = PQ.toAscList pq1 == PQ.toAscList pq2

-- | JSON serialization for Frontier (as list of scored edges).
instance ToJSON Frontier where
  toJSON (Frontier pq) = toJSON (map snd $ PQ.toAscList pq)

instance FromJSON Frontier where
  parseJSON v = do
    edges <- parseJSON v :: Parser [ScoredEdge]
    pure $ foldr pushEdge emptyFrontier edges

-- | Empty frontier.
emptyFrontier :: Frontier
emptyFrontier = Frontier PQ.empty

-- | Push an edge onto the frontier.
--
-- Uses negated score so highest-scoring edges are popped first.
pushEdge :: ScoredEdge -> Frontier -> Frontier
pushEdge edge (Frontier pq) =
  Frontier $ PQ.insert (negate $ seScore edge) edge pq

-- | Push multiple edges onto the frontier.
pushEdges :: [ScoredEdge] -> Frontier -> Frontier
pushEdges edges frontier = foldr pushEdge frontier edges

-- | Pop the highest-scoring edge from the frontier.
--
-- Returns Nothing if frontier is empty.
popEdge :: Frontier -> Maybe (ScoredEdge, Frontier)
popEdge (Frontier pq) = case PQ.minViewWithKey pq of
  Nothing -> Nothing
  Just ((_, edge), pq') -> Just (edge, Frontier pq')

-- | Get the number of edges in the frontier.
frontierSize :: Frontier -> Int
frontierSize (Frontier pq) = PQ.size pq

-- | Check if the frontier is empty.
frontierNull :: Frontier -> Bool
frontierNull (Frontier pq) = PQ.null pq
