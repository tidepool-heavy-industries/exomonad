{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FieldSelectors #-}

-- | Input/Output types for the Summarizer CLI example.
--
-- This module must be separate from Graph.hs for TH staging.
-- The @deriveCLIParser@ and @deriveJSONSchema@ macros require
-- types to be defined in an already-compiled module.
module Tidepool.Graph.CLI.Example.Types
  ( SummaryInput(..)
  , SummaryOutput(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

import Tidepool.Schema (HasJSONSchema(..), objectSchema, emptySchema, SchemaType(..))

-- | Input for the summarizer CLI.
--
-- Parsed from CLI arguments via @deriveCLIParser@.
data SummaryInput = SummaryInput
  { textToSummarize :: Text
    -- ^ The text content to summarize
  , maxWords :: Int
    -- ^ Maximum number of words in the summary (approximate)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Output from the summarizer graph.
--
-- Returned as structured output from the LLM and formatted
-- as JSON or text based on @--format@ flag.
data SummaryOutput = SummaryOutput
  { summary :: Text
    -- ^ The summarized text
  , wordCount :: Int
    -- ^ Actual word count of the summary
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Manual HasJSONSchema instance (TH can't process types in the same module)
instance HasJSONSchema SummaryOutput where
  jsonSchema = objectSchema
    [ ("summary", emptySchema TString)
    , ("wordCount", emptySchema TInteger)
    ]
    ["summary", "wordCount"]
