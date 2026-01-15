{-# LANGUAGE StrictData #-}

-- | Core types for FunctionGemma training data generation.
--
-- These types define the LOCKED SCHEMA for the rating model.
-- Do NOT change field names or enum variants without retraining.
-- The 270M model is syntax-sensitive.
module Tidepool.Training.Types
  ( -- * Tag Enum (Fixed)
    Tag(..)
  , allTags
  , tagToText

    -- * Rubric (Output Schema)
  , Rubric(..)

    -- * Input Context
  , NodeContext(..)
  , QueryContext(..)

    -- * Training Example
  , TrainingExample(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)


-- | Fixed tag enum for semantic classification.
--
-- IMPORTANT: Do not add/remove/rename variants without retraining.
-- The model learns to output exactly these strings.
data Tag
  = Exhaustive        -- ^ Exhaustive pattern match (must handle all cases)
  | PatternMatch      -- ^ Case/pattern matching construct
  | TypeFamily        -- ^ Type family definition or instance
  | BreaksOnAdd       -- ^ Adding a variant/field would break this
  | Import            -- ^ Import statement (typically low relevance)
  | ReExport          -- ^ Re-export (typically low relevance)
  | Signature         -- ^ Type signature only (no implementation)
  | Implementation    -- ^ Actual implementation code
  | Constructor       -- ^ Data constructor usage
  | Recursive         -- ^ Recursive function call
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | All tags in enum order.
allTags :: [Tag]
allTags = [minBound .. maxBound]

-- | Convert tag to lowercase text for output.
tagToText :: Tag -> Text
tagToText = T.toLower . T.pack . show


-- | Rubric output schema (locked).
--
-- All fields are 1-5 integers. Tags must be from the fixed enum.
data Rubric = Rubric
  { rRelevance  :: Int   -- ^ 1-5: How on-topic for the query
  , rRisk       :: Int   -- ^ 1-5: How likely to break if changed
  , rComplexity :: Int   -- ^ 1-5: How much context needed to understand
  , rConfidence :: Int   -- ^ 1-5: How confident in this rating
  , rTags       :: [Tag] -- ^ Which tags apply (from fixed enum)
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Input context for a code location to be rated.
data NodeContext = NodeContext
  { ncLocation    :: Text  -- ^ File location, e.g. "Edges.hs:89"
  , ncHover       :: Text  -- ^ Hover info from LSP (markdown)
  , ncCodeSnippet :: Text  -- ^ Surrounding code lines
  , ncDepth       :: Int   -- ^ Exploration depth (0 = root)
  , ncBreadth     :: Int   -- ^ Number of siblings at this level
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Query context from the user.
data QueryContext = QueryContext
  { qcQuery :: Text   -- ^ Natural language query
  , qcTags  :: [Tag]  -- ^ Interest tags from caller
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)


-- | Complete training example.
data TrainingExample = TrainingExample
  { teQuery  :: QueryContext  -- ^ The query being answered
  , teNode   :: NodeContext   -- ^ The code location being rated
  , teRubric :: Rubric        -- ^ Ground truth rating
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
