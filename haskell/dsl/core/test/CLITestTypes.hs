{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FieldSelectors #-}

-- | Test types for CLI derivation tests.
--
-- This module is separate from CLISpec.hs due to TH staging requirements.
-- The FieldSelectors pragma is required for getDoc to work.
module CLITestTypes
  ( SimpleInput(..)
  , CommandInput(..)
  , OutputResult(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Simple flat record for basic CLI test.
data SimpleInput = SimpleInput
  { inputFile :: FilePath
    -- ^ Path to the input file to process
  , outputFile :: Maybe FilePath
    -- ^ Optional output file path (default: stdout)
  , count :: Int
    -- ^ Number of items to process
  , verbose :: Bool
    -- ^ Enable verbose output
  }
  deriving (Show, Eq, Generic)

-- | Sum type for subcommand test.
data CommandInput
  = Process
      { processFile :: FilePath
        -- ^ File to process
      , processStrict :: Bool
        -- ^ Use strict mode
      }
      -- ^ Process a file
  | Validate
      { validateFile :: FilePath
        -- ^ File to validate
      }
      -- ^ Validate a file
  | List
      -- ^ List available items
  deriving (Show, Eq, Generic)

-- | Output type for testing formatOutput.
data OutputResult = OutputResult
  { resultStatus :: Text
  , resultCount :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
