{-# LANGUAGE FieldSelectors #-}
-- | Input/Output types for impact analysis
--
-- This module is separate for TH staging (deriveCLIParser needs types
-- to be compiled before the TH splice runs).
--
module ImpactAnalysis.Types
  ( -- * Input
    ImpactInput(..)
    -- * Output
  , ImpactOutput(..)
  , SymbolInfo(..)
  , LocationInfo(..)
  , ImpactStats(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Input for impact analysis
data ImpactInput = ImpactInput
  { file :: FilePath
    -- ^ Haskell source file to analyze
  , line :: Int
    -- ^ Line number (1-indexed, as displayed in editors)
  , col :: Int
    -- ^ Column number (1-indexed, as displayed in editors)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Output from impact analysis
data ImpactOutput = ImpactOutput
  { symbol :: Maybe SymbolInfo
    -- ^ Information about the symbol at the position
  , definedAt :: [LocationInfo]
    -- ^ Where the symbol is defined
  , references :: [LocationInfo]
    -- ^ All references to the symbol
  , stats :: ImpactStats
    -- ^ Summary statistics
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Information about a symbol
data SymbolInfo = SymbolInfo
  { symbolName :: Text
    -- ^ Name of the symbol
  , typeSignature :: Text
    -- ^ Type signature (from hover info)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A location in a file
data LocationInfo = LocationInfo
  { locFile :: Text
    -- ^ File path
  , locLine :: Int
    -- ^ Line number (1-indexed)
  , locCol :: Int
    -- ^ Column number (1-indexed)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Summary statistics about impact
data ImpactStats = ImpactStats
  { totalReferences :: Int
    -- ^ Total number of references
  , filesAffected :: Int
    -- ^ Number of unique files with references
  , fileBreakdown :: [(Text, Int)]
    -- ^ Per-file reference counts
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
