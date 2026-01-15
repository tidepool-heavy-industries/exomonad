{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

-- | Types for the semantic scout tool.
--
-- Uses the shared Tag enum from training-generator for consistency
-- between exploration and training data generation.
module Tidepool.Agents.Scout.Types
  ( -- * Query Input
    ScoutQuery(..)
  , defaultBudget

    -- * Response Output
  , ScoutResponse(..)
  , Pointer(..)

    -- * Exploration State
  , ExploreState(..)
  , initialExploreState
  , VisitedNode(..)

    -- * Re-exports from Training Types
  , Tag(..)
  , Rubric(..)
  , NodeContext(..)
  , QueryContext(..)
  , TrainingExample(..)
  , allTags
  , tagToText
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), Value(..)
  , withObject, (.:), (.:?), (.!=), (.=), object, parseJSON
  )
import Data.Aeson.Types (Parser)
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Text.Read (readMaybe)

import Tidepool.Schema (HasJSONSchema(..), objectSchema, arraySchema, emptySchema, SchemaType(..))

-- Re-export training types for consistency
import Tidepool.Training.Types
  ( Tag(..)
  , Rubric(..)
  , NodeContext(..)
  , QueryContext(..)
  , TrainingExample(..)
  , allTags
  , tagToText
  )


-- | Input query for semantic code exploration.
--
-- Note: Tags are stored as [Tag] internally but parsed from comma-separated
-- Text for MCP compatibility (mcp-server library limitation).
data ScoutQuery = ScoutQuery
  { sqQuery :: Text
    -- ^ Natural language question about the codebase
  , sqTags :: [Tag]
    -- ^ Interest signals using the fixed Tag enum
  , sqBudget :: Maybe Int
    -- ^ Maximum number of LSP calls to make (default: 20)
  } deriving stock (Show, Eq, Generic)

-- | Custom FromJSON that handles tags as comma-separated string
-- (for MCP compatibility) or as a proper JSON array.
instance FromJSON ScoutQuery where
  parseJSON = withObject "ScoutQuery" $ \v -> ScoutQuery
    <$> v .: "query"
    <*> (parseTags =<< v .:? "tags" .!= "")
    <*> (parseOptionalInt =<< v .:? "budget")
    where
      -- Parse tags from either comma-separated string or JSON array
      parseTags :: Value -> Parser [Tag]
      parseTags (String t)
        | T.null t = pure []
        | otherwise = pure $ mapMaybe parseTag (T.splitOn "," t)
      parseTags (Array arr) = mapM parseJSON (toList arr)
      parseTags _ = pure []

      -- Parse optional int from either string or number
      parseOptionalInt :: Maybe Value -> Parser (Maybe Int)
      parseOptionalInt Nothing = pure Nothing
      parseOptionalInt (Just (Number n)) = pure $ Just (round n)
      parseOptionalInt (Just (String s)) = pure $ readMaybe (T.unpack s)
      parseOptionalInt (Just _) = pure Nothing

      -- Parse a single tag from text
      parseTag :: Text -> Maybe Tag
      parseTag t = case T.toLower (T.strip t) of
        "exhaustive" -> Just Exhaustive
        "patternmatch" -> Just PatternMatch
        "typefamily" -> Just TypeFamily
        "breaksonadd" -> Just BreaksOnAdd
        "import" -> Just Import
        "reexport" -> Just ReExport
        "signature" -> Just Signature
        "implementation" -> Just Implementation
        "constructor" -> Just Constructor
        "recursive" -> Just Recursive
        _ -> Nothing

instance ToJSON ScoutQuery where
  toJSON sq = object
    [ "query" .= sqQuery sq
    , "tags" .= map tagToText (sqTags sq)
    , "budget" .= sqBudget sq
    ]

-- | Default exploration budget.
defaultBudget :: Int
defaultBudget = 20

-- | Response from semantic scout.
data ScoutResponse = ScoutResponse
  { srSummary :: Text
    -- ^ Markdown summary answering the query
  , srPointers :: [Pointer]
    -- ^ Actionable locations found
  , srNodesVisited :: Int
    -- ^ Number of LSP operations performed
  , srTrainingExamples :: [TrainingExample]
    -- ^ Training examples generated during exploration (for fine-tuning)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | A pointer to a relevant code location.
data Pointer = Pointer
  { pLocation :: Text
    -- ^ File and line (e.g., "Edges.hs:89")
  , pWhat :: Text
    -- ^ What was found at this location
  , pRisk :: Int
    -- ^ Risk level: 1-5 (from rubric)
  , pRelevance :: Int
    -- ^ Relevance level: 1-5 (from rubric)
  , pTags :: [Tag]
    -- ^ Tags that apply to this location
  , pAction :: Maybe Text
    -- ^ Suggested action (optional)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | A visited node during exploration.
data VisitedNode = VisitedNode
  { vnContext :: NodeContext
    -- ^ The node's context (location, hover, snippet)
  , vnRubric :: Rubric
    -- ^ The computed rubric for this node
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | State maintained during exploration.
data ExploreState = ExploreState
  { esQuery :: QueryContext
    -- ^ The original query context
  , esVisited :: [VisitedNode]
    -- ^ Nodes visited so far
  , esBudget :: Int
    -- ^ Remaining exploration budget
  , esDepth :: Int
    -- ^ Current exploration depth
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Create initial exploration state from a query.
initialExploreState :: ScoutQuery -> ExploreState
initialExploreState sq = ExploreState
  { esQuery = QueryContext
      { qcQuery = sqQuery sq
      , qcTags = sqTags sq
      }
  , esVisited = []
  , esBudget = maybe defaultBudget id (sqBudget sq)
  , esDepth = 0
  }

-- ════════════════════════════════════════════════════════════════════════════
-- HasJSONSchema Instance
-- ════════════════════════════════════════════════════════════════════════════

-- Manual instance for ScoutQuery (required by MCP server).
-- The schema defines the JSON input format for the scout tool.
-- Field names match the Aeson encoding: query, tags, budget (no "sq" prefix)
instance HasJSONSchema ScoutQuery where
  jsonSchema = objectSchema
    [ ("query", emptySchema TString)
    , ("tags", arraySchema (emptySchema TString))  -- Tags as strings
    , ("budget", emptySchema TInteger)
    ]
    ["query"]  -- Only query is required; tags and budget have defaults
