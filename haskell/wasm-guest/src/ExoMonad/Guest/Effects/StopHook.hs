{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Guest.Effects.StopHook
  ( checkUncommittedWork,
    checkPRNotFiled,
    getCurrentBranch,
    getAgentId,
  )
where

import Control.Monad.Freer (Eff)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Effects.Git qualified as Git
import Effects.Github qualified as GH
import ExoMonad.Effects.Git (GitGetBranch, GitGetRepoInfo, GitGetStatus, GitHasUnpushedCommits)
import ExoMonad.Effects.GitHub (GitHubGetPullRequestForBranch)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect)
import ExoMonad.Types (Effects)
import System.Environment (lookupEnv)

-- | Check for uncommitted/unpushed work and nudge if found.
checkUncommittedWork :: Text -> Eff Effects (Maybe Text)
checkUncommittedWork branch = do
  statusResult <- suspendEffect @GitGetStatus (Git.GetStatusRequest {Git.getStatusRequestWorkingDir = "."})
  let hasUncommitted = case statusResult of
        Right resp -> not (null (Git.getStatusResponseDirtyFiles resp))
                      || not (null (Git.getStatusResponseStagedFiles resp))
        _ -> False

  unpushedResult <- suspendEffect @GitHasUnpushedCommits (Git.HasUnpushedCommitsRequest {Git.hasUnpushedCommitsRequestWorkingDir = ".", Git.hasUnpushedCommitsRequestRemote = "origin"})
  let hasUnpushed = case unpushedResult of
        Right resp -> Git.hasUnpushedCommitsResponseHasUnpushed resp
        _ -> False

  if hasUncommitted
    then pure $ Just $ "You have uncommitted changes on " <> branch <> " but no PR filed. Commit and file a PR before stopping."
    else if hasUnpushed
      then pure $ Just $ "Commits on " <> branch <> " aren't in a PR yet. File a PR before stopping."
      else pure Nothing

-- | Check if the agent is on a non-main branch with no PR filed.
-- Returns a nudge message if so. This catches agents that exit without
-- filing a PR for their work.
checkPRNotFiled :: Text -> Eff Effects (Maybe Text)
checkPRNotFiled branch = do
  repoResult <- suspendEffect @GitGetRepoInfo (Git.GetRepoInfoRequest {Git.getRepoInfoRequestWorkingDir = "."})
  case repoResult of
    Left _ -> pure Nothing  -- can't determine repo, don't block
    Right repoInfo -> do
      let owner = Git.getRepoInfoResponseOwner repoInfo
          repo = Git.getRepoInfoResponseName repoInfo
      -- If we can't derive a meaningful owner/repo (e.g., non-GitHub remote),
      -- treat PR status as unknown and don't nudge.
      if T.null owner || T.null repo
        then pure Nothing
        else do
          prResult <- suspendEffect @GitHubGetPullRequestForBranch
            (GH.GetPullRequestForBranchRequest
              { GH.getPullRequestForBranchRequestOwner = owner
              , GH.getPullRequestForBranchRequestRepo = repo
              , GH.getPullRequestForBranchRequestBranch = TL.fromStrict branch
              })
          case prResult of
            Right resp
              | GH.getPullRequestForBranchResponseFound resp ->
                  pure Nothing
            Right _ ->
              pure $ Just $ "No PR filed for branch " <> branch <> ". File a PR with file_pr before stopping."
            Left _ ->
              -- On GitHub API errors/offline, don't emit a misleading nudge.
              pure Nothing

-- ============================================================================
-- Agent Identity Helpers
-- ============================================================================

-- | Read the agent's identity from EXOMONAD_AGENT_ID env var.
getAgentId :: IO (Maybe Text)
getAgentId = fmap (fmap T.pack) (lookupEnv "EXOMONAD_AGENT_ID")

-- | Get the current git branch name, defaulting to "unknown" on error.
getCurrentBranch :: Eff Effects Text
getCurrentBranch = do
  result <- suspendEffect @GitGetBranch (Git.GetBranchRequest {Git.getBranchRequestWorkingDir = "."})
  case result of
    Right resp -> pure $ TL.toStrict (Git.getBranchResponseBranch resp)
    Left _ -> pure "unknown"
