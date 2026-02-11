{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ExoMonad.Guest.Effects.StopHook
  ( runStopHookChecks,
    ReviewStatus (..),

    -- * Agent identity helpers
    getAgentId,

    -- * Lifecycle messaging
    sendLifecycleNote,
  )
where

import Data.Aeson (ToJSON (..), encode, object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Vector qualified as V
import Effects.Copilot qualified as Copilot
import Effects.Git qualified as Git
import Effects.Github qualified as GH
import ExoMonad.Effects.Copilot (waitForCopilotReview)
import ExoMonad.Effects.Git (getBranch, getRepoInfo, getStatus, hasUnpushedCommits)
import ExoMonad.Effects.GitHub (getPullRequestForBranch)
import ExoMonad.Effects.Messaging (sendNote)
import ExoMonad.Guest.Types (StopDecision (..), StopHookOutput (..), allowStopResponse, blockStopResponse)
import GHC.Generics (Generic)
import System.Environment (lookupEnv)

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
  result <- runStopHookChecksInner
  -- Send lifecycle note based on outcome
  case result of
    out@(StopHookOutput Allow _) -> do
      sendLifecycleNote "agent_completed" [("summary", "All checks passed: clean tree, pushed, PR filed, Copilot approved")]
      pure out
    out@(StopHookOutput Block (Just reason)) -> do
      sendLifecycleNote "agent_blocked" [("reason", reason)]
      pure out
    out -> pure out

-- | Inner check logic (separated so we can wrap with lifecycle messaging).
runStopHookChecksInner :: IO StopHookOutput
runStopHookChecksInner = do
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
                    pure $ blockStopResponse "PR found but no details available"
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
              pure $ blockStopResponse $ "Unknown review status: " <> status

formatComment :: Copilot.CopilotComment -> Text
formatComment c =
  "- "
    <> TL.toStrict (Copilot.copilotCommentPath c)
    <> (let l = Copilot.copilotCommentLine c in if l == 0 then "" else ":" <> T.pack (show l))
    <> ": "
    <> TL.toStrict (Copilot.copilotCommentBody c)

-- ============================================================================
-- Agent Identity Helpers
-- ============================================================================

-- | Read the agent's identity from EXOMONAD_AGENT_ID env var.
getAgentId :: IO (Maybe Text)
getAgentId = fmap (fmap T.pack) (lookupEnv "EXOMONAD_AGENT_ID")

-- ============================================================================
-- Lifecycle Messaging
-- ============================================================================

-- | Send a structured lifecycle note to the TL.
--
-- The note content is JSON-structured so the TL can parse it programmatically.
-- If agent ID is not available, the note is silently skipped.
sendLifecycleNote :: Text -> [(Text, Text)] -> IO ()
sendLifecycleNote noteType fields = do
  mAgent <- getAgentId
  case mAgent of
    Just agent -> do
      -- Get current branch for context
      branchName <- getCurrentBranch
      let jsonFields =
              [ ("\"type\": \"" <> noteType <> "\""),
                ("\"agent_id\": \"" <> agent <> "\""),
                ("\"branch\": \"" <> branchName <> "\"")
              ]
              <> map (\(k, v) -> "\"" <> k <> "\": \"" <> escapeJson v <> "\"") fields
          jsonContent = "{" <> T.intercalate ", " jsonFields <> "}"
      _ <- sendNote jsonContent
      pure ()
    _ -> pure () -- No agent context, skip messaging

-- | Get the current git branch name, defaulting to "unknown" on error.
getCurrentBranch :: IO Text
getCurrentBranch = do
  result <- getBranch (Git.GetBranchRequest {Git.getBranchRequestWorkingDir = "."})
  case result of
    Right resp -> pure $ TL.toStrict (Git.getBranchResponseBranch resp)
    Left _ -> pure "unknown"

-- | Escape JSON string content (minimal: backslash and double-quote).
escapeJson :: Text -> Text
escapeJson = T.replace "\\" "\\\\" . T.replace "\"" "\\\"" . T.replace "\n" "\\n"