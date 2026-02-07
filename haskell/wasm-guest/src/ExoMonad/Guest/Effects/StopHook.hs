{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ExoMonad.Guest.Effects.StopHook
  ( runStopHookChecks,
    ReviewStatus (..),
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Effects.Copilot qualified as Copilot
import Effects.Git qualified as Git
import Effects.Github qualified as GH
import ExoMonad.Effects.Copilot (waitForCopilotReview)
import ExoMonad.Effects.Git (getRepoInfo, getStatus, hasUnpushedCommits)
import ExoMonad.Effects.GitHub (getPullRequestForBranch)
import ExoMonad.Guest.Types (StopHookOutput, allowStopResponse, blockStopResponse)

-- ============================================================================
-- Types
-- ============================================================================

-- | Copilot review status (strongly typed).
data ReviewStatus = Reviewed | Pending | Timeout
  deriving stock (Show, Eq)

-- ============================================================================
-- Main Check Logic
-- ============================================================================

runStopHookChecks :: IO StopHookOutput
runStopHookChecks = do
  -- Check 1: Uncommitted changes
  dirtyResult <- getStatus (Git.GetStatusRequest {Git.getStatusRequestWorkingDir = "."})
  case dirtyResult of
    Left err -> pure $ blockStopResponse $ "Failed to check dirty files: " <> T.pack (show err)
    Right resp ->
      let dirtyFiles = V.toList (Git.getStatusResponseDirtyFiles resp)
       in if not (null dirtyFiles)
            then
              let files = T.intercalate ", " (map TL.toStrict dirtyFiles)
                  msg =
                    "You have uncommitted changes in these files: "
                      <> files
                      <> "\n\nTo continue, please stage and commit them:\n"
                      <> "git add -A && git commit -m \"Describe your work\""
               in pure $ blockStopResponse msg
            else checkUnpushedCommits

checkUnpushedCommits :: IO StopHookOutput
checkUnpushedCommits = do
  -- Get repo info (branch + GitHub owner/name)
  repoResult <- getRepoInfo (Git.GetRepoInfoRequest {Git.getRepoInfoRequestWorkingDir = "."})
  case repoResult of
    Left err -> pure $ blockStopResponse $ "Failed to get repo info: " <> T.pack (show err)
    Right repoInfo -> do
      let branchName = TL.toStrict (Git.getRepoInfoResponseBranch repoInfo)
      -- Check for unpushed commits
      unpushedResult <- hasUnpushedCommits (Git.HasUnpushedCommitsRequest {Git.hasUnpushedCommitsRequestWorkingDir = ".", Git.hasUnpushedCommitsRequestRemote = "origin"})
      case unpushedResult of
        Left err -> pure $ blockStopResponse $ "Failed to check unpushed commits: " <> T.pack (show err)
        Right unpushedResp ->
          let unpushedCount = fromIntegral (Git.hasUnpushedCommitsResponseCount unpushedResp) :: Int
           in if unpushedCount > 0
                then
                  let commitWord =
                        if unpushedCount == 1
                          then "commit"
                          else "commits"
                      msg =
                        "You have "
                          <> T.pack (show unpushedCount)
                          <> " unpushed "
                          <> commitWord
                          <> " on branch '"
                          <> branchName
                          <> "'.\n"
                          <> "Push them to GitHub with:\n"
                          <> "git push -u origin "
                          <> branchName
                   in pure $ blockStopResponse msg
                else checkPRFiled repoInfo

checkPRFiled :: Git.GetRepoInfoResponse -> IO StopHookOutput
checkPRFiled repoInfo = do
  let repoOwner = TL.toStrict (Git.getRepoInfoResponseOwner repoInfo)
      repoName = TL.toStrict (Git.getRepoInfoResponseName repoInfo)
      branchName = TL.toStrict (Git.getRepoInfoResponseBranch repoInfo)

  if T.null repoOwner
    then pure $ blockStopResponse "Could not determine GitHub owner from remote URL"
    else
      if T.null repoName
        then pure $ blockStopResponse "Could not determine GitHub repo name from remote URL"
        else do
          let prInput =
                GH.GetPullRequestForBranchRequest
                  { GH.getPullRequestForBranchRequestOwner = TL.fromStrict repoOwner,
                    GH.getPullRequestForBranchRequestRepo = TL.fromStrict repoName,
                    GH.getPullRequestForBranchRequestBranch = TL.fromStrict branchName
                  }

          prResult <- getPullRequestForBranch prInput
          case prResult of
            Left err -> pure $ blockStopResponse $ "Failed to check for PR: " <> T.pack (show err)
            Right prResp ->
              if not (GH.getPullRequestForBranchResponseFound prResp)
                then
                  pure $
                    blockStopResponse $
                      "No PR filed for this branch. Use the file_pr tool to create a PR:\n"
                        <> "file_pr(title=\"Describe your work\", body=\"Detailed description of changes\")"
                else case GH.getPullRequestForBranchResponsePullRequest prResp of
                  Nothing ->
                    pure $
                      blockStopResponse "PR found but no details available"
                  Just pr ->
                    checkReviewComments pr

checkReviewComments :: GH.PullRequest -> IO StopHookOutput
checkReviewComments pr = do
  let prNum = fromIntegral (GH.pullRequestNumber pr) :: Int
  -- Wait for Copilot to review the PR (polls with 30s interval, 5 min timeout)
  let waitInput =
        Copilot.WaitForCopilotReviewRequest
          { Copilot.waitForCopilotReviewRequestPrNumber = fromIntegral prNum,
            Copilot.waitForCopilotReviewRequestTimeoutSecs = 300, -- 5 minutes
            Copilot.waitForCopilotReviewRequestPollIntervalSecs = 30
          }

  reviewResult <- waitForCopilotReview waitInput
  case reviewResult of
    Left err -> pure $ blockStopResponse $ "Failed to wait for Copilot review: " <> T.pack (show err)
    Right reviewOutput ->
      let status = TL.toStrict (Copilot.waitForCopilotReviewResponseStatus reviewOutput)
       in case status of
            "timeout" ->
              pure $
                blockStopResponse $
                  "Copilot hasn't reviewed PR #"
                    <> T.pack (show prNum)
                    <> " yet. Wait for Copilot review or check if Copilot is enabled for this repo."
            "reviewed" ->
              let comments = V.toList (Copilot.waitForCopilotReviewResponseComments reviewOutput)
               in if null comments
                    then pure allowStopResponse
                    else
                      let formattedComments = T.unlines $ map formatComment comments
                          msg =
                            "Copilot left "
                              <> T.pack (show (length comments))
                              <> " comment(s) on PR #"
                              <> T.pack (show prNum)
                              <> ":\n\n"
                              <> formattedComments
                              <> "\nAddress these comments, commit, and push your changes."
                       in pure $ blockStopResponse msg
            "pending" ->
              pure $
                blockStopResponse $
                  "Copilot review is still pending for PR #"
                    <> T.pack (show prNum)
                    <> ". Wait for review to complete."
            _ ->
              pure $
                blockStopResponse $
                  "Unknown review status: " <> status

formatComment :: Copilot.CopilotComment -> Text
formatComment c =
  "- "
    <> TL.toStrict (Copilot.copilotCommentPath c)
    <> (let l = Copilot.copilotCommentLine c in if l == 0 then "" else ":" <> T.pack (show l))
    <> ": "
    <> TL.toStrict (Copilot.copilotCommentBody c)
