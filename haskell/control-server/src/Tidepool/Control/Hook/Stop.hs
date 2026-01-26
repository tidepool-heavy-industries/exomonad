{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Stop hook logic with rich state detection and templated prompting.
--
-- Gathers git state (dirty files, commits ahead, PR status), runs pre-commit
-- checks, and optionally closes beads. Renders a Jinja template that provides
-- actionable guidance to the agent.
--
-- This module integrates the logic from exo_complete and pre_commit_check tools
-- directly into the Stop hook flow.
module Tidepool.Control.Hook.Stop
  ( stopHookLogic
  , StopHookResult(..)
  ) where

import Control.Monad.Freer (Eff, Member)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec.Pos (SourcePos)

import Tidepool.Control.ExoTools (parseIssueNumber)
import Tidepool.Control.Hook.Stop.Context (StopContext(..), StopPRContext(..), StopPreCommitContext(..))
import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo, getDirtyFiles, getCommitsAhead)
import Tidepool.Effects.GitHub (GitHub, PullRequest(..), listPullRequests, Repo(..), PRFilter(..), defaultPRFilter, Issue(..), getIssue, closeIssue, IssueState(..))
import Tidepool.Effects.Justfile (Justfile, runRecipe, JustResult(..))
import Tidepool.Graph.Template (TypedTemplate, typedTemplateFile, runTypedTemplate)

-- | Compiled Stop hook template.
stopTemplate :: TypedTemplate StopContext SourcePos
stopTemplate = $(typedTemplateFile ''StopContext "templates/hook/stop.jinja")

-- | Result of Stop hook logic.
data StopHookResult = StopHookResult
  { shrShouldBlock :: Bool
    -- ^ True if stop should be blocked (uncommitted changes on issue branch)
  , shrMessage :: Text
    -- ^ Rendered template with guidance
  , shrStage :: Maybe Text
    -- ^ Optional stage name for circuit breaker tracking
  } deriving (Show, Eq)

-- | Core Stop hook logic.
--
-- Detects git state, runs pre-commit checks, closes issues, and renders
-- template with actionable guidance.
--
-- Behavior:
-- 1. Not on issue branch: Allow stop, no checks
-- 2. On issue branch without PR: Block with "file PR" guidance
-- 3. On issue branch with dirty files and no PR: Block with commit guidance
-- 4. On issue branch with PR: Run pre-commit checks, close issue if passes, allow stop
stopHookLogic
  :: (Member Git es, Member GitHub es, Member Justfile es)
  => Text  -- ^ GitHub repo name (e.g., "owner/repo")
  -> Bool  -- ^ Whether to run pre-commit checks (from TIDEPOOL_STOP_PRECOMMIT)
  -> Eff es StopHookResult
stopHookLogic repoName runPreCommit = do
  -- 1. Gather git state
  mWorktree <- getWorktreeInfo
  dirtyFiles <- getDirtyFiles

  case mWorktree of
    Nothing -> do
      -- Not in a git repo
      let ctx = StopContext
            { issue_number = Nothing
            , branch = "(no git)"
            , dirty_files = []
            , commits_ahead = 0
            , pr = Nothing
            , clean = True
            , pre_commit = Nothing
            , issue_closed = False
            , issue_already_closed = False
            }
      pure $ StopHookResult False (runTypedTemplate ctx stopTemplate) Nothing

    Just wt -> do
      let branchName = wt.wiBranch
          mIssueNum = parseIssueNumber branchName

      -- 2. Get commits ahead of main
      commitsAhead <- getCommitsAhead "origin/main"

      -- 3. Check for existing PR
      let repo = Repo repoName
          filt = defaultPRFilter { pfBase = Just "main", pfLimit = Just 100 }
      prs <- listPullRequests repo filt
      let mPR = find (\p -> p.prHeadRefName == branchName) prs

      -- 4. Build base context
      let hasDirty = not (null dirtyFiles)
          hasPR = isJust mPR
          isIssueBranch = isJust mIssueNum

          prCtx = case mPR of
            Nothing -> Nothing
            Just p -> Just StopPRContext
              { number = p.prNumber
              , url = p.prUrl
              , pending_comments = 0  -- TODO: fetch from pr_review_status
              }

      -- 5. Check issue status and run completion logic if appropriate
      (preCommitCtx, issueWasClosed, issueWasAlreadyClosed) <- case mIssueNum of
        Nothing -> pure (Nothing, False, False)
        Just num -> do
          -- Check issue status
          mIssue <- getIssue repo num False
          case mIssue of
            Nothing -> pure (Nothing, False, False)
            Just issue | issue.issueState == IssueClosed ->
              -- Issue already closed
              pure (Nothing, False, True)
            Just _issue -> do
              -- Issue is open - run pre-commit and potentially close
              if hasPR && not hasDirty && runPreCommit
                then do
                  -- Run pre-commit checks
                  res <- runRecipe "pre-commit-fast" []
                  let checkSuccess = res.exitCode == 0
                      checkOutput = if T.null res.stdout
                                    then res.stderr
                                    else res.stdout <> "\n" <> res.stderr
                      pcCtx = Just StopPreCommitContext
                        { success = checkSuccess
                        , output = checkOutput
                        }

                  -- Close issue if checks pass
                  if checkSuccess
                    then do
                      closeIssue repo num
                      pure (pcCtx, True, False)
                    else pure (pcCtx, False, False)
                else pure (Nothing, False, False)

      -- 6. Determine blocking and clean status
      let -- Clean if: (not issue branch) OR (has PR) OR (issue was closed or already closed)
          isClean = not isIssueBranch || hasPR || issueWasClosed || issueWasAlreadyClosed

          -- Block if: on issue branch AND has uncommitted changes AND no PR
          -- If PR exists, we don't block even with dirty files
          -- Also block if pre-commit check failed
          preCommitFailed = case preCommitCtx of
            Just pc -> not (success pc)
            Nothing -> False
          shouldBlock = (isIssueBranch && hasDirty && not hasPR) || preCommitFailed
          
          stage = if not shouldBlock then Nothing
                  else if preCommitFailed then Just "pre-commit-failed"
                  else Just "uncommitted-changes"

          ctx = StopContext
            { issue_number = mIssueNum
            , branch = branchName
            , dirty_files = map T.pack dirtyFiles
            , commits_ahead = commitsAhead
            , pr = prCtx
            , clean = isClean
            , pre_commit = preCommitCtx
            , issue_closed = issueWasClosed
            , issue_already_closed = issueWasAlreadyClosed
            }

      pure $ StopHookResult shouldBlock (runTypedTemplate ctx stopTemplate) stage
