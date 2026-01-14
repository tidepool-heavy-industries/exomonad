{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Core types for graph entry and exit
module HumanDrivenDev.Types.Core
  ( -- * Entry
    Spec(..)
  , TopLevelInput(..)
    -- * Exit
  , FinalResult(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Specification for what to build/analyze/process
data Spec = Spec
  { sDescription :: Text
    -- ^ Human-readable description of the task
  , sAcceptanceCriteria :: [Text]
    -- ^ List of criteria that define success
  , sTargetPath :: FilePath
    -- ^ Where to write output files
  , sContext :: Maybe Text
    -- ^ Optional additional context
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Top-level input to the graph
data TopLevelInput = TopLevelInput
  { tliSpec :: Spec
    -- ^ The specification to work from
  , tliWorkDir :: FilePath
    -- ^ Working directory for operations
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Final result from the graph
data FinalResult = FinalResult
  { frSummary :: Text
    -- ^ Summary of what was accomplished
  , frArtifacts :: [FilePath]
    -- ^ Paths to generated artifacts
  , frMetrics :: Maybe Text
    -- ^ Optional metrics/stats
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
