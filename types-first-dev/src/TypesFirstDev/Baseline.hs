{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Baseline runner for types-first-dev workflow.
--
-- DEPRECATED: This module contained the old baseline implementation.
-- Use the hybrid handlers instead.
module TypesFirstDev.Baseline
  ( -- * Running
    runBaseline
  , runBaselineWithSpec
    -- * Result Construction
  , resultsToMetadata
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import TypesFirstDev.Stats (RunMetadata)
import TypesFirstDev.Types (StackSpec)

-- Stub implementations
runBaseline :: Text -> FilePath -> IO RunMetadata
runBaseline _ _ = error "runBaseline: Deprecated. Use hybrid handlers instead."

runBaselineWithSpec :: Text -> StackSpec -> IO RunMetadata
runBaselineWithSpec _ _ = error "runBaselineWithSpec: Deprecated. Use hybrid handlers instead."

resultsToMetadata :: Text -> UTCTime -> IO RunMetadata
resultsToMetadata _ _ = error "resultsToMetadata: Deprecated. Use hybrid handlers instead."
