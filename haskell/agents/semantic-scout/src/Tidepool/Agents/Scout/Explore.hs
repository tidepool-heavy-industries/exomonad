{-# LANGUAGE OverloadedStrings #-}

-- | Exploration loop for semantic code navigation.
--
-- Architecture:
--
-- @
-- ScoutQuery
--     │
--     ▼
-- ┌─────────────────────────────────────────┐
-- │           Exploration Loop              │
-- │                                         │
-- │  1. Find entry points (workspace symbol)│
-- │  2. For each location:                  │
-- │     a. Fetch context (hover, snippet)   │
-- │     b. Score with Scorer (Gemma/heur)   │
-- │     c. Decide expand via heuristics     │
-- │     d. If expand: queue children (refs) │
-- │  3. Accumulate visited nodes            │
-- │  4. Generate training examples          │
-- │                                         │
-- └─────────────────────────────────────────┘
--     │
--     ▼
-- ScoutResponse
-- @
--
-- Key design: FunctionGemma only scores (outputs Rubric).
-- All exploration decisions are made by Haskell heuristics
-- operating on the Rubric. This separation allows:
--
--   * FunctionGemma to have a narrow, well-defined task
--   * Heuristics to be tuned without retraining
--   * Training data to be straightforward (context → rubric)
module Tidepool.Agents.Scout.Explore
  ( -- * Exploration
    explore
  , ExploreConfig(..)
  , defaultExploreConfig

    -- * Scorer Abstraction
  , Scorer
  , heuristicScorer

    -- * Exploration State
  , ExploreEnv(..)
  ) where

import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, ViewL(..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Tidepool.Agents.Scout.Types
import Tidepool.Agents.Scout.Heuristics (scoreNode, shouldExpand)


-- ════════════════════════════════════════════════════════════════════════════
-- SCORER ABSTRACTION
-- ════════════════════════════════════════════════════════════════════════════

-- | A scorer takes query context and node context, returns a Rubric.
--
-- This abstraction allows swapping implementations:
--   * 'heuristicScorer' - Deterministic rules (fast, no model)
--   * Future: gemmaScorer - FunctionGemma 270M (fast, local, fine-tuned)
--
-- The scorer is called once per visited node. For Gemma, this means
-- one inference call per node (~5ms on M1 with Metal).
type Scorer m = QueryContext -> NodeContext -> m Rubric

-- | Heuristic scorer using deterministic rules.
--
-- This is the baseline implementation. It's fast and predictable,
-- but lacks semantic understanding. Good for:
--   * Generating training data (ground truth)
--   * Fallback when Gemma unavailable
--   * Testing exploration logic
heuristicScorer :: Applicative m => Scorer m
heuristicScorer query node = pure $ scoreNode query node


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for exploration.
data ExploreConfig = ExploreConfig
  { ecMaxDepth :: Int
    -- ^ Maximum exploration depth (default: 5)
  , ecDefaultBudget :: Int
    -- ^ Default budget if not specified in query (default: 20)
  , ecExpandThreshold :: Double
    -- ^ Score threshold for expansion (default: 8.0)
  } deriving (Show, Eq)

defaultExploreConfig :: ExploreConfig
defaultExploreConfig = ExploreConfig
  { ecMaxDepth = 5
  , ecDefaultBudget = 20
  , ecExpandThreshold = 8.0
  }


-- ════════════════════════════════════════════════════════════════════════════
-- EXPLORATION STATE
-- ════════════════════════════════════════════════════════════════════════════

-- | Environment for exploration (mutable state).
data ExploreEnv = ExploreEnv
  { eeVisited :: Set Text
    -- ^ Locations already visited (to avoid cycles)
  , eeQueue :: Seq NodeToExplore
    -- ^ Priority queue of nodes to explore
  , eeResults :: [VisitedNode]
    -- ^ Accumulated results (reverse order)
  , eeBudget :: Int
    -- ^ Remaining budget
  , eeDepth :: Int
    -- ^ Current depth (for logging)
  }

-- | A node queued for exploration.
data NodeToExplore = NodeToExplore
  { nteLocation :: Text
    -- ^ Location to explore
  , nteDepth :: Int
    -- ^ Depth at which this was queued
  , nteParent :: Maybe Text
    -- ^ Parent location (for breadth calculation)
  } deriving (Show, Eq)


-- ════════════════════════════════════════════════════════════════════════════
-- EXPLORATION LOOP
-- ════════════════════════════════════════════════════════════════════════════

-- | Run exploration and return response.
--
-- This is the main entry point. It:
--   1. Initializes state from query
--   2. Finds entry points (mock for now, LSP later)
--   3. Runs BFS exploration loop
--   4. Builds response with pointers and training examples
--
-- The exploration is parameterized by:
--   * Scorer: How to score each node (heuristic or Gemma)
--   * Config: Depth/budget limits
--
-- For now, this uses mock data. V1 will wire in actual LSP.
explore
  :: Monad m
  => Scorer m           -- ^ Scoring function
  -> ExploreConfig      -- ^ Configuration
  -> ScoutQuery         -- ^ Input query
  -> m ScoutResponse
explore scorer config query = do
  let queryCtx = QueryContext
        { qcQuery = sqQuery query
        , qcTags = sqTags query
        }
  let budget = fromMaybe (ecDefaultBudget config) (sqBudget query)

  -- Initialize with mock entry points
  -- TODO: Replace with actual LSP workspace symbol search
  let entryPoints = mockEntryPoints (sqTags query)
  let initialQueue = Seq.fromList
        [ NodeToExplore loc 0 Nothing | loc <- entryPoints ]

  let initialEnv = ExploreEnv
        { eeVisited = Set.empty
        , eeQueue = initialQueue
        , eeResults = []
        , eeBudget = budget
        , eeDepth = 0
        }

  -- Run exploration loop
  finalEnv <- exploreLoop scorer config queryCtx initialEnv

  -- Build response
  let visited = reverse (eeResults finalEnv)
  let pointers = map toPointer visited
  let trainingExamples = map (toTrainingExample queryCtx) visited

  pure ScoutResponse
    { srSummary = buildSummary query pointers
    , srPointers = pointers
    , srNodesVisited = length visited
    , srTrainingExamples = trainingExamples
    }


-- | The core exploration loop.
--
-- Processes nodes from queue until budget exhausted or queue empty.
-- Each iteration:
--   1. Dequeue next node
--   2. Skip if already visited
--   3. Fetch node context (mock for now)
--   4. Score with scorer
--   5. Record as visited
--   6. If shouldExpand: enqueue children
exploreLoop
  :: Monad m
  => Scorer m
  -> ExploreConfig
  -> QueryContext
  -> ExploreEnv
  -> m ExploreEnv
exploreLoop scorer config queryCtx env
  -- Budget exhausted
  | eeBudget env <= 0 = pure env
  -- Process next node
  | otherwise = case Seq.viewl (eeQueue env) of
      -- Queue empty
      EmptyL -> pure env
      -- Pop and process
      node :< rest ->
        -- Skip if already visited
        if nteLocation node `Set.member` eeVisited env
          then exploreLoop scorer config queryCtx env { eeQueue = rest }
          else do
            -- Fetch context (mock for now)
            let nodeCtx = mockNodeContext node

            -- Score with scorer
            rubric <- scorer queryCtx nodeCtx

            -- Record visit
            let visitedNode = VisitedNode
                  { vnContext = nodeCtx
                  , vnRubric = rubric
                  }

            -- Decide whether to expand
            let doExpand = shouldExpand
                  rubric
                  (qcTags queryCtx)
                  (nteDepth node)
                  (countSiblings node env)
                  (eeBudget env - 1)
                  && nteDepth node < ecMaxDepth config

            -- Queue children if expanding
            let children = if doExpand
                  then mockChildren (nteLocation node) (nteDepth node + 1)
                  else []

            let newEnv = env
                  { eeVisited = Set.insert (nteLocation node) (eeVisited env)
                  , eeQueue = rest Seq.>< Seq.fromList children
                  , eeResults = visitedNode : eeResults env
                  , eeBudget = eeBudget env - 1
                  }

            exploreLoop scorer config queryCtx newEnv


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Count siblings at current level (for breadth calculation).
countSiblings :: NodeToExplore -> ExploreEnv -> Int
countSiblings node env =
  length $ filter (\n -> nteDepth n == nteDepth node) (toList $ eeQueue env)
  where
    toList = foldr (:) []

-- | Convert visited node to pointer for response.
toPointer :: VisitedNode -> Pointer
toPointer vn = Pointer
  { pLocation = ncLocation (vnContext vn)
  , pWhat = T.take 60 (ncHover (vnContext vn))
  , pRisk = rRisk (vnRubric vn)
  , pRelevance = rRelevance (vnRubric vn)
  , pTags = rTags (vnRubric vn)
  , pAction = suggestAction (vnRubric vn)
  }

-- | Suggest action based on rubric.
suggestAction :: Rubric -> Maybe Text
suggestAction rubric
  | rRisk rubric >= 4 = Just "Review carefully before modifying"
  | Exhaustive `elem` rTags rubric = Just "Add case for new variant"
  | TypeFamily `elem` rTags rubric = Just "Add instance for new type"
  | otherwise = Nothing

-- | Convert to training example.
toTrainingExample :: QueryContext -> VisitedNode -> TrainingExample
toTrainingExample query vn = TrainingExample
  { teQuery = query
  , teNode = vnContext vn
  , teRubric = vnRubric vn
  }

-- | Build summary text.
buildSummary :: ScoutQuery -> [Pointer] -> Text
buildSummary query pointers = T.unlines
  [ "## Exploration Summary"
  , ""
  , "Query: " <> sqQuery query
  , ""
  , "Interest tags: " <> T.intercalate ", " (map tagToText (sqTags query))
  , ""
  , "Found " <> T.pack (show (length pointers)) <> " relevant locations."
  , ""
  , "**Note**: This is using mock data. Real LSP exploration coming in V1."
  ]


-- ════════════════════════════════════════════════════════════════════════════
-- MOCK DATA (Replace with LSP in V1)
-- ════════════════════════════════════════════════════════════════════════════

-- | Mock entry points based on tags.
mockEntryPoints :: [Tag] -> [Text]
mockEntryPoints tags
  | PatternMatch `elem` tags = ["Edges.hs:80", "Interpret.hs:150"]
  | TypeFamily `elem` tags = ["Types.hs:45", "Generic.hs:200"]
  | otherwise = ["Types.hs:100", "Edges.hs:50"]

-- | Mock node context for a location.
mockNodeContext :: NodeToExplore -> NodeContext
mockNodeContext node = NodeContext
  { ncLocation = nteLocation node
  , ncHover = mockHoverFor (nteLocation node)
  , ncCodeSnippet = mockSnippetFor (nteLocation node)
  , ncDepth = nteDepth node
  , ncBreadth = 5  -- Mock breadth
  }

-- | Mock hover info based on location.
mockHoverFor :: Text -> Text
mockHoverFor loc
  | "Edges" `T.isInfixOf` loc = "dispatchOneOf :: OneOf targets -> GraphM ()"
  | "Types" `T.isInfixOf` loc = "type family NodeHandler mode :: Type"
  | "Interpret" `T.isInfixOf` loc = "runGraph :: ValidGraphRecord g => g (AsHandler es) -> Eff es a"
  | "Generic" `T.isInfixOf` loc = "class GReifyMCPEntries (rep :: Type -> Type)"
  | otherwise = "unknown :: ()"

-- | Mock code snippet based on location.
mockSnippetFor :: Text -> Text
mockSnippetFor loc
  | "Edges" `T.isInfixOf` loc = T.unlines
      [ "dispatchOneOf choice = case choice of"
      , "  Here payload -> handleHere payload"
      , "  There rest   -> dispatchOneOf rest"
      ]
  | "Types" `T.isInfixOf` loc = T.unlines
      [ "type family NodeHandler mode where"
      , "  NodeHandler (AsHandler es) = HandlerType es"
      , "  NodeHandler AsGraph = Proxy"
      ]
  | otherwise = "-- mock snippet"

-- | Mock children (references) for a location.
mockChildren :: Text -> Int -> [NodeToExplore]
mockChildren parent depth
  | depth >= 3 = []  -- Stop expanding at depth 3
  | "Edges" `T.isInfixOf` parent =
      [ NodeToExplore "Interpret.hs:200" depth (Just parent)
      , NodeToExplore "Generic.hs:100" depth (Just parent)
      ]
  | "Types" `T.isInfixOf` parent =
      [ NodeToExplore "Edges.hs:90" depth (Just parent)
      , NodeToExplore "Validate.hs:50" depth (Just parent)
      ]
  | otherwise = []
