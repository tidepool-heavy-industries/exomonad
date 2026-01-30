{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FieldSelectors #-}

-- | Test types for CLI derivation tests.
--
-- This module is separate from CLISpec.hs due to TH staging requirements.
-- The FieldSelectors pragma is required for getDoc to work.
module CLITestTypes
  ( SimpleInput (..),
    CommandInput (..),
    OutputResult (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Simple flat record for basic CLI test.
data SimpleInput = SimpleInput
  { -- | Path to the input file to process
    inputFile :: FilePath,
    -- | Optional output file path (default: stdout)
    outputFile :: Maybe FilePath,
    -- | Number of items to process
    count :: Int,
    -- | Enable verbose output
    verbose :: Bool
  }
  deriving (Show, Eq, Generic)

-- | Sum type for subcommand test.
data CommandInput
  = Process
      { -- | File to process
        processFile :: FilePath,
        -- | Use strict mode
        processStrict :: Bool
      }
  | -- \^ Process a file
    Validate
      { -- | File to validate
        validateFile :: FilePath
      }
  | -- \^ Validate a file

    -- | List available items
    List
  deriving (Show, Eq, Generic)

-- | Output type for testing formatOutput.
data OutputResult = OutputResult
  { resultStatus :: Text,
    resultCount :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
