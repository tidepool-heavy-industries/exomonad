{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ExoMonad.Control.ExoTools.FilePR
  ( filePRLogic
  , FilePRArgs(..)
  , FilePRResult(..)
  , PRInfo(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import qualified Data.Text as T
import Data.Maybe (listToMaybe)

import ExoMonad.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo)
import ExoMonad.Effects.GitHub (GitHub, PRCreateSpec(..), PRUrl(..), PullRequest(..), PRFilter(..), Issue(..), getIssue, createPR, listPullRequests, defaultPRFilter, defaultRepo)

import ExoMonad.Control.ExoTools.Internal (parseIssueNumber, slugify, formatPRBody)
import ExoMonad.Control.ExoTools.FilePR.Types
import ExoMonad.Control.Combinators (withEffect)

-- | Core logic for file_pr.
-- Issue number and title inferred from branch. Agent provides testing/compromises.
-- Idempotent: checks for existing PR first, returns it if found.
filePRLogic
  :: (Member Git es, Member GitHub es)
  => FilePRArgs
  -> Eff es FilePRResult
filePRLogic args = do
  -- 1. Get Worktree/Git info
  mWt <- getWorktreeInfo

  -- 2. Determine Issue Number from branch
  let mIssueNum = case mWt of
        Just wt -> parseIssueNumber wt.wiBranch
        Nothing -> Nothing

  case mIssueNum of
    Nothing ->
      pure $ FilePRResult Nothing False (Just "Not on an issue branch. file_pr requires gh-{num}/* branch naming.")
    Just num -> do
      -- 3. Get Issue Info
      let repo = defaultRepo
      withEffect (getIssue repo num False)
        (\case
          Nothing ->
            pure $ FilePRResult Nothing False (Just $ "Issue #" <> T.pack (show num) <> " not found.")
          Just issue -> do
            -- 4. Check if PR already exists (idempotent)
            let searchStr = "[gh-" <> T.pack (show num) <> "]"
                filt = defaultPRFilter { pfSearch = Just searchStr, pfLimit = Just 1 }

            withEffect (listPullRequests repo filt)
              (\prs ->
                case listToMaybe prs of
                  Just pr -> do
                    -- PR already exists - return it (idempotent behavior)
                    let info = PRInfo
                          { number = pr.prNumber
                          , url = pr.prUrl
                          , status = T.pack (show pr.prState)
                          , title = pr.prTitle
                          }
                    pure $ FilePRResult (Just info) False Nothing

                  Nothing -> do
                    -- 5. Prepare PR Spec
                    let headBranch = case mWt of
                          Just wt -> wt.wiBranch
                          Nothing -> "gh-" <> T.pack (show num) <> "/" <> slugify issue.issueTitle

                    let title = "[" <> "gh-" <> T.pack (show num) <> "] " <> issue.issueTitle
                        body = formatPRBody issue args.testing args.compromises
                        spec = PRCreateSpec
                          { prcsRepo = repo
                          , prcsHead = headBranch
                          , prcsBase = "main"
                          , prcsTitle = title
                          , prcsBody = body
                          }

                    -- 6. Create PR
                    withEffect (createPR spec)
                      (\(PRUrl url) -> do
                        -- Note: We don't have the PR number from createPR, so we use 0
                        -- In practice, the URL is what matters
                        let info = PRInfo
                              { number = 0
                              , url = url
                              , status = "OPEN"
                              , title = title
                              }
                        pure $ FilePRResult (Just info) True Nothing)
                      (\_err -> pure $ FilePRResult Nothing False (Just "GitHub error creating PR"))
              )
              (\_err -> pure $ FilePRResult Nothing False (Just "GitHub error listing PRs"))
        )
        (\_err -> pure $ FilePRResult Nothing False (Just $ "GitHub error fetching issue #" <> T.pack (show num)))