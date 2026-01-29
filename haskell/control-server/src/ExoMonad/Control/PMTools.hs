{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Control.PMTools
  ( -- * Epic Logic
    pmEpicCreateLogic
  , pmEpicListLogic
  , pmEpicUpdateLogic

    -- * Strategy Logic
  , pmPitchLogic
  , pmInterviewLogic
  ) where

import Control.Monad.Freer (Eff, Member)
import qualified Data.Text as T

import ExoMonad.Control.PMTools.Types
import ExoMonad.Effect.Types (Log, logInfo, logWarn)

-- ════════════════════════════════════════════════════════════════════════════
-- EPIC LOGIC (SCAFFOLD)
-- ════════════════════════════════════════════════════════════════════════════

pmEpicCreateLogic :: (Member Log es) => PMEpicCreateArgs -> Eff es PMEpicCreateResult
pmEpicCreateLogic args = do
  logInfo $ "Scaffold: pmEpicCreateLogic called for " <> args.title
  pure $ PMEpicCreateResult "epic-1" "https://github.com/owner/repo/issues/1"

pmEpicListLogic :: (Member Log es) => PMEpicListArgs -> Eff es PMEpicListResult
pmEpicListLogic _ = do
  logInfo "Scaffold: pmEpicListLogic called"
  pure $ PMEpicListResult [EpicSummary "epic-1" "Scaffold Epic" "open" 0]

pmEpicUpdateLogic :: (Member Log es) => PMEpicUpdateArgs -> Eff es PMEpicUpdateResult
pmEpicUpdateLogic args = do
  logInfo $ "Scaffold: pmEpicUpdateLogic called for " <> args.epicId
  pure $ PMEpicUpdateResult True (EpicSummary args.epicId "Scaffold Epic" "open" 10)

-- ════════════════════════════════════════════════════════════════════════════
-- STRATEGY LOGIC (SCAFFOLD)
-- ════════════════════════════════════════════════════════════════════════════

pmPitchLogic :: (Member Log es) => PMPitchArgs -> Eff es PMPitchResult
pmPitchLogic args = do
  logInfo $ "Scaffold: pmPitchLogic called for " <> args.title
  -- In real implementation, this would invoke a TUI popup
  pure $ PMPitchResult True (Just "Looks good, proceed.")

pmInterviewLogic :: (Member Log es) => PMInterviewArgs -> Eff es PMInterviewResult
pmInterviewLogic args = do
  logInfo $ "Scaffold: pmInterviewLogic called for " <> args.topic
  -- In real implementation, this would invoke a multi-step TUI dialog
  pure $ PMInterviewResult ["Answer 1", "Answer 2"] "Summary of interview"