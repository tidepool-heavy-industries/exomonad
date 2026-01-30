{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FieldSelectors #-}

-- | E2E test types for CLI graph integration.
--
-- This module is separate from CLIGraphSpec.hs due to TH staging requirements.
-- The FieldSelectors pragma is required for getDoc to work with deriveCLIParser.
module CLIGraphTypes
  ( CounterInput (..),
    CounterOutput (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- | Input for the counter graph CLI.
--
-- This type demonstrates CLI derivation with multiple numeric fields.
data CounterInput = CounterInput
  { -- | Starting value for the counter
    startValue :: Int,
    -- | Amount to add each iteration
    increment :: Int,
    -- | How many times to increment
    times :: Int
  }
  deriving (Show, Eq, Generic)

-- | Output from the counter graph.
--
-- Contains the final computed value and operation count for verification.
data CounterOutput = CounterOutput
  { finalValue :: Int,
    operationsPerformed :: Int
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
