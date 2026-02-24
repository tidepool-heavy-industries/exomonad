{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | GitHub effects for repository, issue, and PR operations.
--
-- All effects are dispatched via the @github@ namespace.
-- Request and response types are proto-generated from @proto/effects/github.proto@.
--
-- = Example
--
-- @
-- import ExoMonad.Effects.GitHub
--
-- main :: IO ()
-- main = do
--   result <- listIssues (ListIssuesRequest "anthropics" "exomonad" ...)
--   case result of
--     Left err -> print err
--     Right resp -> mapM_ print (listIssuesResponseIssues resp)
-- @
module ExoMonad.Effects.GitHub
  ( -- * Effect Types
    GitHubListIssues,
    GitHubGetIssue,
    GitHubListPullRequests,
    GitHubGetPullRequest,
    GitHubGetPullRequestForBranch,
    GitHubGetPullRequestReviewComments,
    GitHubCreatePullRequest,

    -- * Re-exported proto types
    module Effects.Github,
  )
where

import Effects.EffectError (EffectError)
import Effects.Github
import ExoMonad.Effect.Class (Effect (..))

-- ============================================================================
-- Effect phantom types + instances
-- ============================================================================

data GitHubListIssues

instance Effect GitHubListIssues where
  type Input GitHubListIssues = ListIssuesRequest
  type Output GitHubListIssues = ListIssuesResponse
  effectId = "github.list_issues"

data GitHubGetIssue

instance Effect GitHubGetIssue where
  type Input GitHubGetIssue = GetIssueRequest
  type Output GitHubGetIssue = GetIssueResponse
  effectId = "github.get_issue"

data GitHubListPullRequests

instance Effect GitHubListPullRequests where
  type Input GitHubListPullRequests = ListPullRequestsRequest
  type Output GitHubListPullRequests = ListPullRequestsResponse
  effectId = "github.list_pull_requests"

data GitHubGetPullRequest

instance Effect GitHubGetPullRequest where
  type Input GitHubGetPullRequest = GetPullRequestRequest
  type Output GitHubGetPullRequest = GetPullRequestResponse
  effectId = "github.get_pull_request"

data GitHubGetPullRequestForBranch

instance Effect GitHubGetPullRequestForBranch where
  type Input GitHubGetPullRequestForBranch = GetPullRequestForBranchRequest
  type Output GitHubGetPullRequestForBranch = GetPullRequestForBranchResponse
  effectId = "github.get_pull_request_for_branch"

data GitHubGetPullRequestReviewComments

instance Effect GitHubGetPullRequestReviewComments where
  type Input GitHubGetPullRequestReviewComments = GetPullRequestReviewCommentsRequest
  type Output GitHubGetPullRequestReviewComments = GetPullRequestReviewCommentsResponse
  effectId = "github.get_pull_request_review_comments"

data GitHubCreatePullRequest

instance Effect GitHubCreatePullRequest where
  type Input GitHubCreatePullRequest = CreatePullRequestRequest
  type Output GitHubCreatePullRequest = CreatePullRequestResponse
  effectId = "github.create_pull_request"
