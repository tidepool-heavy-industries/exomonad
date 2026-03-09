{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Guest.Effects.StopHook
  ( runStopHookChecks,

    -- * Agent identity helpers
    getAgentId,
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Effects.Git qualified as Git
import Effects.Github qualified as GH
import Effects.Log qualified as Log
import ExoMonad.Effects.Git (GitGetBranch, GitGetRepoInfo, GitGetStatus, GitHasUnpushedCommits)
import ExoMonad.Effects.GitHub (GitHubGetPullRequest, GitHubGetPullRequestForBranch, GitHubGetPullRequestReviewComments)
import ExoMonad.Effects.Log (LogEmitEvent)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect, suspendEffect_)
import ExoMonad.Guest.Types (StopDecision (..), StopHookOutput (..), allowStopResponse, blockStopResponse)
import ExoMonad.Types (HookEffects)
import Proto3.Suite.Types qualified as Protobuf
import System.Environment (lookupEnv)

-- ============================================================================
-- Main Check Logic
-- ============================================================================

data StopCheckResult
  = MustBlock Text   -- ^ changes_requested: agent must address
  | ShouldNudge Text -- ^ informational: agent can still stop
  | Clean            -- ^ no issues

runStopHookChecks :: Eff HookEffects StopHookOutput
runStopHookChecks = do
  branch <- getCurrentBranch
  if branch `elem` ["main", "master"]
    then pure allowStopResponse
    else do
      result <- gatherStopCheck branch
      -- Log to event log (JSONL) for observability
      let eventPayload = BSL.toStrict $ Aeson.encode $
            object ["branch" .= branch, "result" .= describeResult result]
      void $ suspendEffect_ @LogEmitEvent (Log.EmitEventRequest
        { Log.emitEventRequestEventType = "agent.stop_check",
          Log.emitEventRequestPayload = eventPayload,
          Log.emitEventRequestTimestamp = 0
        })

      case result of
        MustBlock msg -> pure $ blockStopResponse msg
        ShouldNudge msg -> pure $ StopHookOutput Allow (Just msg)
        Clean -> pure allowStopResponse

describeResult :: StopCheckResult -> Text
describeResult (MustBlock msg) = "block: " <> msg
describeResult (ShouldNudge msg) = "nudge: " <> msg
describeResult Clean = "clean"

-- | Check git/PR state and return a structured check result.
gatherStopCheck :: Text -> Eff HookEffects StopCheckResult
gatherStopCheck branch = do
  repoInfoResult <- suspendEffect @GitGetRepoInfo (Git.GetRepoInfoRequest {Git.getRepoInfoRequestWorkingDir = "."})
  case repoInfoResult of
    Left _ -> pure Clean
    Right repoInfo -> do
      let repoOwner = TL.toStrict (Git.getRepoInfoResponseOwner repoInfo)
          repoName = TL.toStrict (Git.getRepoInfoResponseName repoInfo)

      prResult <- suspendEffect @GitHubGetPullRequestForBranch GH.GetPullRequestForBranchRequest
        { GH.getPullRequestForBranchRequestOwner = TL.fromStrict repoOwner
        , GH.getPullRequestForBranchRequestRepo = TL.fromStrict repoName
        , GH.getPullRequestForBranchRequestBranch = TL.fromStrict branch
        }

      case prResult of
        Right prResp | GH.getPullRequestForBranchResponseFound prResp ->
          case GH.getPullRequestForBranchResponsePullRequest prResp of
            Nothing -> pure Clean
            Just pr -> do
              let prNum = fromIntegral (GH.pullRequestNumber pr) :: Int
              checkPrStatusStructured repoOwner repoName prNum

        _ -> do
          nudge <- checkUncommittedWork branch
          case nudge of
            Just msg -> pure (ShouldNudge msg)
            Nothing -> pure Clean

-- | Check PR review status and return structured result if there are issues.
checkPrStatusStructured :: Text -> Text -> Int -> Eff HookEffects StopCheckResult
checkPrStatusStructured repoOwner repoName prNum = do
  fullPrResult <- suspendEffect @GitHubGetPullRequest GH.GetPullRequestRequest
    { GH.getPullRequestRequestOwner = TL.fromStrict repoOwner
    , GH.getPullRequestRequestRepo = TL.fromStrict repoName
    , GH.getPullRequestRequestNumber = fromIntegral prNum
    , GH.getPullRequestRequestIncludeReviews = True
    }

  let reviews = case fullPrResult of
        Right resp -> V.toList (GH.getPullRequestResponseReviews resp)
        _ -> []

  commentsResult <- suspendEffect @GitHubGetPullRequestReviewComments GH.GetPullRequestReviewCommentsRequest
    { GH.getPullRequestReviewCommentsRequestOwner = TL.fromStrict repoOwner
    , GH.getPullRequestReviewCommentsRequestRepo = TL.fromStrict repoName
    , GH.getPullRequestReviewCommentsRequestNumber = fromIntegral prNum
    }

  let comments = case commentsResult of
        Right resp -> V.toList (GH.getPullRequestReviewCommentsResponseComments resp)
        _ -> []

  let hasChangesRequested = any (\r -> GH.reviewState r == Protobuf.Enumerated (Right GH.ReviewStateREVIEW_STATE_CHANGES_REQUESTED)) reviews
  if hasChangesRequested
    then pure $ MustBlock $ "PR #" <> T.pack (show prNum) <> " has changes requested. Address review comments before stopping."
    else if not (null comments)
      then pure $ ShouldNudge $ "PR #" <> T.pack (show prNum) <> " has review comments. Address them before stopping."
      else if null reviews
        then pure $ ShouldNudge $ "PR #" <> T.pack (show prNum) <> " is awaiting review. The system will auto-notify your parent when review completes or times out. You can continue working or yield."
        else pure Clean

-- | Check for uncommitted/unpushed work and nudge if found.
checkUncommittedWork :: Text -> Eff HookEffects (Maybe Text)
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
    then pure $ Just $ "You have uncommitted changes on " <> branch <> " but no PR filed. Commit, file a PR, and call notify_parent before stopping."
    else if hasUnpushed
      then pure $ Just $ "Commits on " <> branch <> " aren't in a PR yet. File a PR and call notify_parent before stopping."
      else pure Nothing

-- ============================================================================
-- Agent Identity Helpers
-- ============================================================================

-- | Read the agent's identity from EXOMONAD_AGENT_ID env var.
getAgentId :: IO (Maybe Text)
getAgentId = fmap (fmap T.pack) (lookupEnv "EXOMONAD_AGENT_ID")

-- | Get the current git branch name, defaulting to "unknown" on error.
getCurrentBranch :: Eff HookEffects Text
getCurrentBranch = do
  result <- suspendEffect @GitGetBranch (Git.GetBranchRequest {Git.getBranchRequestWorkingDir = "."})
  case result of
    Right resp -> pure $ TL.toStrict (Git.getBranchResponseBranch resp)
    Left _ -> pure "unknown"
