{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Git effects for repository operations.
--
-- All effects are dispatched via the @git@ namespace.
-- Request and response types are proto-generated from @proto/effects/git.proto@.
--
-- = Example
--
-- @
-- import ExoMonad.Effects.Git
--
-- main :: IO ()
-- main = do
--   result <- getBranch (GetBranchRequest ".")
--   case result of
--     Left err -> print err
--     Right resp -> putStrLn $ "Branch: " <> show (getBranchResponseBranch resp)
-- @
module ExoMonad.Effects.Git
  ( -- * Effect Types
    GitGetBranch,
    GitGetStatus,
    GitGetCommits,
    GitHasUnpushedCommits,
    GitGetRemoteUrl,
    GitGetRepoInfo,
    GitGetWorktree,

    -- * Re-exported proto types
    module Effects.Git,
  )
where

import Effects.EffectError (EffectError)
import Effects.Git
import ExoMonad.Effect.Class (Effect (..))

-- ============================================================================
-- Effect phantom types + instances
-- ============================================================================

data GitGetBranch

instance Effect GitGetBranch where
  type Input GitGetBranch = GetBranchRequest
  type Output GitGetBranch = GetBranchResponse
  effectId = "git.get_branch"

data GitGetStatus

instance Effect GitGetStatus where
  type Input GitGetStatus = GetStatusRequest
  type Output GitGetStatus = GetStatusResponse
  effectId = "git.get_status"

data GitGetCommits

instance Effect GitGetCommits where
  type Input GitGetCommits = GetCommitsRequest
  type Output GitGetCommits = GetCommitsResponse
  effectId = "git.get_commits"

data GitHasUnpushedCommits

instance Effect GitHasUnpushedCommits where
  type Input GitHasUnpushedCommits = HasUnpushedCommitsRequest
  type Output GitHasUnpushedCommits = HasUnpushedCommitsResponse
  effectId = "git.has_unpushed_commits"

data GitGetRemoteUrl

instance Effect GitGetRemoteUrl where
  type Input GitGetRemoteUrl = GetRemoteUrlRequest
  type Output GitGetRemoteUrl = GetRemoteUrlResponse
  effectId = "git.get_remote_url"

data GitGetRepoInfo

instance Effect GitGetRepoInfo where
  type Input GitGetRepoInfo = GetRepoInfoRequest
  type Output GitGetRepoInfo = GetRepoInfoResponse
  effectId = "git.get_repo_info"

data GitGetWorktree

instance Effect GitGetWorktree where
  type Input GitGetWorktree = GetWorktreeRequest
  type Output GitGetWorktree = GetWorktreeResponse
  effectId = "git.get_worktree"
