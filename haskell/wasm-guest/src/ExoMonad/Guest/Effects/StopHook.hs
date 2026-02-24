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
import ExoMonad.Guest.Types (StopDecision (..), StopHookOutput (..), allowStopResponse)
import ExoMonad.Types (HookEffects)
import Proto3.Suite.Types qualified as Protobuf
import System.Environment (lookupEnv)

-- ============================================================================
-- Main Check Logic
-- ============================================================================

runStopHookChecks :: Eff HookEffects StopHookOutput
runStopHookChecks = do
  branch <- getCurrentBranch
  if branch `elem` ["main", "master"]
    then pure allowStopResponse
    else do
      nudge <- gatherNudge branch
      -- Log to event log (JSONL) for observability
      let eventPayload = BSL.toStrict $ Aeson.encode $
            object ["branch" .= branch, "nudge" .= nudge]
      void $ suspendEffect_ @LogEmitEvent (Log.EmitEventRequest
        { Log.emitEventRequestEventType = "agent.stop_check",
          Log.emitEventRequestPayload = eventPayload,
          Log.emitEventRequestTimestamp = 0
        })
      -- Return nudge as reason so the agent sees it
      pure $ StopHookOutput Allow nudge

-- | Check git/PR state and return a nudge message if the agent should
-- consider calling notify_parent before stopping.
gatherNudge :: Text -> Eff HookEffects (Maybe Text)
gatherNudge branch = do
  repoInfoResult <- suspendEffect @GitGetRepoInfo (Git.GetRepoInfoRequest {Git.getRepoInfoRequestWorkingDir = "."})
  case repoInfoResult of
    Left _ -> pure Nothing
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
            Nothing -> pure Nothing
            Just pr -> do
              let prNum = fromIntegral (GH.pullRequestNumber pr) :: Int
              checkPrStatus repoOwner repoName prNum

        _ -> checkUncommittedWork branch

-- | Check PR review status and nudge if there are unresolved comments.
checkPrStatus :: Text -> Text -> Int -> Eff HookEffects (Maybe Text)
checkPrStatus repoOwner repoName prNum = do
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
  if not (null comments) || hasChangesRequested
    then pure $ Just $ "PR #" <> T.pack (show prNum) <> " has unresolved review comments. Address them before stopping, or call notify_parent with status 'failure' if you're stuck."
    else if null reviews
      then pure $ Just $ "PR #" <> T.pack (show prNum) <> " hasn't been reviewed yet. If your work is done, call notify_parent to let your parent know."
      else pure Nothing

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
