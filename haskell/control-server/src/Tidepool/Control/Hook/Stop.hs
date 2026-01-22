{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Stop hook logic with rich state detection and templated prompting.
--
-- Gathers git state (dirty files, commits ahead, PR status) and renders
-- a Jinja template that provides actionable guidance to the agent.
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

import Tidepool.Control.ExoTools (parseBeadId)
import Tidepool.Control.Hook.Stop.Context (StopContext(..), StopPRContext(..))
import Tidepool.Effects.Git (Git, WorktreeInfo(..), getWorktreeInfo, getDirtyFiles, getCommitsAhead)
import Tidepool.Effects.GitHub (GitHub, PullRequest(..), listPullRequests, Repo(..), PRFilter(..), defaultPRFilter)
import Tidepool.Graph.Template (TypedTemplate, typedTemplateFile, runTypedTemplate)

-- | Compiled Stop hook template.
stopTemplate :: TypedTemplate StopContext SourcePos
stopTemplate = $(typedTemplateFile ''StopContext "templates/hook/stop.jinja")

-- | Result of Stop hook logic.
data StopHookResult = StopHookResult
  { shrShouldBlock :: Bool
    -- ^ True if stop should be blocked (uncommitted changes on bead branch)
  , shrMessage :: Text
    -- ^ Rendered template with guidance
  } deriving (Show, Eq)

-- | Core Stop hook logic.
--
-- Detects git state and renders template with actionable guidance.
stopHookLogic
  :: (Member Git es, Member GitHub es)
  => Text  -- ^ GitHub repo name (e.g., "owner/repo")
  -> Eff es StopHookResult
stopHookLogic repoName = do
  -- 1. Gather git state
  mWorktree <- getWorktreeInfo
  dirtyFiles <- getDirtyFiles

  case mWorktree of
    Nothing -> do
      -- Not in a git repo
      let ctx = StopContext
            { bead_id = Nothing
            , branch = "(no git)"
            , dirty_files = []
            , commits_ahead = 0
            , pr = Nothing
            , clean = True
            }
      pure $ StopHookResult False (runTypedTemplate ctx stopTemplate)

    Just wt -> do
      let branchName = wt.wiBranch
          mBeadId = parseBeadId branchName

      -- 2. Get commits ahead of main
      commitsAhead <- getCommitsAhead "origin/main"

      -- 3. Check for existing PR
      let repo = Repo repoName
          filt = defaultPRFilter { pfBase = Just "main", pfLimit = Just 100 }
      prs <- listPullRequests repo filt
      let mPR = find (\p -> p.prHeadRefName == branchName) prs

      -- 4. Build context
      let hasDirty = not (null dirtyFiles)
          hasPR = isJust mPR
          isBeadBranch = isJust mBeadId

          -- Clean if: (not bead branch) OR (has PR)
          -- Having a PR means work is "done enough" even with local dirty files
          isClean = not isBeadBranch || hasPR

          -- Block if: on bead branch AND has uncommitted changes AND no PR
          -- If PR exists, we don't block even with dirty files
          shouldBlock = isBeadBranch && hasDirty && not hasPR

          prCtx = case mPR of
            Nothing -> Nothing
            Just p -> Just StopPRContext
              { number = p.prNumber
              , url = p.prUrl
              , pending_comments = 0  -- TODO: fetch from pr_review_status
              }

          ctx = StopContext
            { bead_id = mBeadId
            , branch = branchName
            , dirty_files = map T.pack dirtyFiles
            , commits_ahead = commitsAhead
            , pr = prCtx
            , clean = isClean
            }

      pure $ StopHookResult shouldBlock (runTypedTemplate ctx stopTemplate)
