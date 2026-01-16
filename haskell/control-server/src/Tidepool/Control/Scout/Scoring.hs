{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

-- | Scoring functions for semantic code exploration.
--
-- Combines FunctionGemma boolean outputs with LSP EdgeType to produce
-- composite scores for priority queue ordering.
--
-- Key insight: FunctionGemma answers semantic questions (4 booleans),
-- while EdgeType comes from LSP (structural signal). Haskell combines
-- them into a tunable composite score.
module Tidepool.Control.Scout.Scoring
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

import Tidepool.Control.Scout.EdgeTypes (EdgeContext(..), EdgeType(..))
import Tidepool.Training.Types (ScoreEdgeOutput(..))


-- | Configuration for composite score calculation.
--
-- Weights for combining FunctionGemma booleans with LSP EdgeType.
-- The formula is:
--   score = queryRelevantBonus (if true)
--         + breakingBoundaryBonus (if true)
--         + publicContractBonus (if true)
--         + stableAnchorBonus (if true, negative if false)
--         + edgeTypeBonus
--         - depth * depthPenalty
data ScoreConfig = ScoreConfig
  { scQueryRelevantBonus    :: Double
    -- ^ Bonus if edge is query-relevant (default: 3.0)
  , scBreakingBoundaryBonus :: Double
    -- ^ Bonus if edge crosses breaking boundary (default: 2.0)
  , scPublicContractBonus   :: Double
    -- ^ Bonus if edge is part of public API (default: 1.0)
  , scStableAnchorBonus     :: Double
    -- ^ Bonus if stable anchor, penalty if unstable (default: 0.5, -0.5)
  , scDepthPenalty          :: Double
    -- ^ Penalty per depth level (default: 0.3)
  , scEdgeTypeBonus         :: EdgeType -> Double
    -- ^ Bonus by edge type (pattern matches get priority)
  } deriving (Generic)

-- | Default scoring configuration.
defaultScoreConfig :: ScoreConfig
defaultScoreConfig = ScoreConfig
  { scQueryRelevantBonus    = 3.0
  , scBreakingBoundaryBonus = 2.0
  , scPublicContractBonus   = 1.0
  , scStableAnchorBonus     = 0.5
  , scDepthPenalty          = 0.3
  , scEdgeTypeBonus         = defaultEdgeTypeBonus
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


-- | Calculate composite score from boolean outputs and edge context.
--
-- Higher scores = more relevant, explored first.
--
-- Formula:
--   score = queryRelevantBonus (if true)
--         + breakingBoundaryBonus (if true)
--         + publicContractBonus (if true)
--         + stableAnchorBonus (if true) or -stableAnchorBonus (if false)
--         + edgeTypeBonus
--         - depth * depthPenalty
--
-- FunctionGemma provides semantic signals (4 booleans).
-- LSP provides structural signals (EdgeType).
-- Haskell combines them with tunable weights.
compositeScore :: ScoreConfig -> ScoreEdgeOutput -> EdgeContext -> Double
compositeScore config bools edge =
  let depth = fromIntegral (ecDepth edge)
      typeBonus = scEdgeTypeBonus config (ecEdgeType edge)
  in -- FunctionGemma semantic signals
     (if seoIsQueryRelevant bools then scQueryRelevantBonus config else 0.0)
   + (if seoIsBreakingBoundary bools then scBreakingBoundaryBonus config else 0.0)
   + (if seoIsPublicContract bools then scPublicContractBonus config else 0.0)
   + (if seoIsStableAnchor bools then scStableAnchorBonus config else negate (scStableAnchorBonus config))
   -- LSP structural signals
   + typeBonus
   -- Depth penalty
   - depth * scDepthPenalty config

-- | Simplified composite score with default config.
compositeScoreDefault :: ScoreEdgeOutput -> Int -> Double
compositeScoreDefault bools depth =
  compositeScore defaultScoreConfig bools (dummyEdgeContext depth)
  where
    -- Create a minimal EdgeContext just for scoring (with UnknownEdge for no bonus)
    dummyEdgeContext d = EdgeContext
      { ecEdgeType = UnknownEdge
      , ecLocation = ""
      , ecHover = ""
      , ecSnippet = ""
      , ecDepth = d
      , ecParent = Nothing
      , ecTypeName = Nothing
      , ecFunctionSig = Nothing
      , ecPatterns = Nothing
      , ecConstraints = Nothing
      }


-- | An edge with its computed score, ready for the priority queue.
data ScoredEdge = ScoredEdge
  { seEdge   :: EdgeContext
    -- ^ The edge context
  , seBools  :: ScoreEdgeOutput
    -- ^ The boolean outputs from FunctionGemma
  , seScore  :: Double
    -- ^ Composite score (higher = more relevant)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Create a scored edge from context and boolean outputs.
mkScoredEdge :: ScoreConfig -> EdgeContext -> ScoreEdgeOutput -> ScoredEdge
mkScoredEdge config edge bools = ScoredEdge
  { seEdge  = edge
  , seBools = bools
  , seScore = compositeScore config bools edge
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
