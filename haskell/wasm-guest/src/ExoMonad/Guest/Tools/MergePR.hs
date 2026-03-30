{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Merge PR tool - merges a child's PR with readiness checks.
--
-- 'mergePRCore' contains the shared I/O logic.
-- Role-specific MCP wrappers apply their own state transitions.
module ExoMonad.Guest.Tools.MergePR
  ( MergePR,
    MergePRArgs (..),
    MergePROutput (..),
    mergePRCore,
    mergePRDescription,
    mergePRSchema,
    mergePRRender,
    extractAgentName,
  )
where

import Control.Monad (void, when)
import Control.Monad.Freer (Eff)
import Data.Aeson (FromJSON, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Effects.Agent qualified as Agent
import Effects.Git qualified as Git
import Effects.Github qualified as GH
import Effects.Log qualified as Log
import Effects.MergePr qualified as MP
import Effects.Process qualified as Proc
import ExoMonad.Effects.Agent (AgentCleanup)
import ExoMonad.Effects.Git (GitGetBranch, GitGetRepoInfo)
import ExoMonad.Effects.GitHub (GitHubGetPullRequest)
import ExoMonad.Effects.Log (LogEmitEvent, LogError, LogInfo)
import ExoMonad.Effects.MergePR (MergePRMergePr)
import ExoMonad.Effects.Process (ProcessRun)
import ExoMonad.Guest.Tool.Class (Effects, MCPCallOutput, MCPTool (..), errorResult, successResult)
import ExoMonad.Guest.Tool.Schema (genericToolSchemaWith)
import ExoMonad.Guest.Tool.SuspendEffect (suspendEffect, suspendEffect_)
import GHC.Generics (Generic)
import Proto3.Suite.Types qualified as Protobuf

data MergePR

data MergePRArgs = MergePRArgs
  { mprPrNumber :: Int,
    mprStrategy :: Maybe Text,
    mprWorkingDir :: Maybe Text,
    mprForce :: Maybe Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON MergePRArgs where
  parseJSON = withObject "MergePRArgs" $ \v ->
    MergePRArgs
      <$> v .: "pr_number"
      <*> v .:? "strategy"
      <*> v .:? "working_dir"
      <*> v .:? "force"

data MergePROutput = MergePROutput
  { mpoSuccess :: Bool,
    mpoMessage :: Text,
    mpoGitFetched :: Bool,
    mpoBranchName :: Text,
    mpoPullOk :: Bool
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON MergePROutput where
  toJSON (MergePROutput s m g b p) =
    object
      [ "success" .= s,
        "message" .= m,
        "git_fetched" .= g,
        "branch_name" .= b,
        "pull_ok" .= p
      ]

-- | Shared tool description for merge_pr.
mergePRDescription :: Text
mergePRDescription = "Merge a GitHub pull request and fetch changes. Checks Copilot review status before merging: requires either a clean review (no change requests) or commits pushed after the latest review. Use force=true to bypass (e.g., after [REVIEW TIMEOUT]). After merging, verify the build — especially when merging multiple PRs in parallel, as changes may interact."

-- | Shared tool schema for merge_pr.
mergePRSchema :: Aeson.Object
mergePRSchema =
  genericToolSchemaWith @MergePRArgs
    [ ("pr_number", "PR number to merge"),
      ("strategy", "Merge strategy: squash (default), merge, or rebase"),
      ("working_dir", "Working directory for git operations"),
      ("force", "Skip Copilot review check and merge immediately (use after [REVIEW TIMEOUT])")
    ]

-- | Core merge_pr I/O: self-merge guard + readiness check + merge + cleanup + git pull.
-- Returns Left on error, Right MergePROutput on success.
mergePRCore :: MergePRArgs -> Eff Effects (Either Text MergePROutput)
mergePRCore args = do
  let force = fromMaybe False (mprForce args)
  let prNum = mprPrNumber args
  void $ suspendEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = TL.fromStrict ("MergePR: Merging PR #" <> T.pack (show prNum) <> if force then " (force)" else ""), Log.infoRequestFields = ""})

  -- Get repo info and branch (shared across self-merge guard and readiness check)
  repoInfoResult <- suspendEffect @GitGetRepoInfo (Git.GetRepoInfoRequest {Git.getRepoInfoRequestWorkingDir = "."})
  branchResult <- suspendEffect @GitGetBranch (Git.GetBranchRequest {Git.getBranchRequestWorkingDir = "."})

  case (repoInfoResult, branchResult) of
    (Right repoInfo, Right branchResp) -> do
      let owner = TL.toStrict (Git.getRepoInfoResponseOwner repoInfo)
          repo = TL.toStrict (Git.getRepoInfoResponseName repoInfo)
          currentBranch = TL.toStrict (Git.getBranchResponseBranch branchResp)

      -- Self-merge guard: agents cannot merge their own PRs
      prResult <-
        suspendEffect @GitHubGetPullRequest
          GH.GetPullRequestRequest
            { GH.getPullRequestRequestOwner = TL.fromStrict owner,
              GH.getPullRequestRequestRepo = TL.fromStrict repo,
              GH.getPullRequestRequestNumber = fromIntegral prNum,
              GH.getPullRequestRequestIncludeReviews = not force
            }
      case prResult of
        Left err -> do
          void $ suspendEffect_ @LogError (Log.ErrorRequest {Log.errorRequestMessage = TL.fromStrict ("MergePR: failed to fetch PR: " <> T.pack (show err)), Log.errorRequestFields = ""})
          pure $ Left ("Failed to fetch PR #" <> T.pack (show prNum) <> " for self-merge check. Cannot proceed.")
        Right resp -> do
          let mPr = GH.getPullRequestResponsePullRequest resp
              isSelfMerge = case mPr of
                Just pr -> TL.toStrict (GH.pullRequestHeadRef pr) == currentBranch
                Nothing -> False
          if isSelfMerge
            then pure $ Left $ "Cannot merge your own PR #" <> T.pack (show prNum) <> ". Your parent agent will merge this PR after reviewing. Call notify_parent instead."
            else
              if force
                then doMerge args
                else do
                  let readiness = checkCopilotReadinessFromPR prNum resp
                  case readiness of
                    Ready -> doMerge args
                    NotReady reason -> do
                      void $ suspendEffect_ @LogError (Log.ErrorRequest {Log.errorRequestMessage = TL.fromStrict ("MergePR: blocked: " <> reason), Log.errorRequestFields = ""})
                      pure $ Left reason
    _ -> do
      void $ suspendEffect_ @LogError (Log.ErrorRequest {Log.errorRequestMessage = "MergePR: failed to get repo info or branch for self-merge check", Log.errorRequestFields = ""})
      pure $ Left ("Failed to determine repo/branch info. Cannot verify self-merge guard for PR #" <> T.pack (show prNum) <> ".")

-- | Render a MergePROutput to MCPCallOutput.
mergePRRender :: MergePROutput -> MCPCallOutput
mergePRRender output =
  successResult $
    object
      [ "success" .= mpoSuccess output,
        "message" .= mpoMessage output,
        "git_fetched" .= mpoGitFetched output,
        "next"
          .= ( if mpoPullOk output
                 then "Verify build: cargo check --workspace. Especially important after parallel merges — changes may interact." :: Text
                 else "git pull failed — run 'git pull --rebase' manually to sync your local branch. Then verify build: cargo check --workspace." :: Text
             )
      ]

-- | Copilot review readiness.
data Readiness = Ready | NotReady Text

-- | Check Copilot readiness from an already-fetched PR response.
checkCopilotReadinessFromPR :: Int -> GH.GetPullRequestResponse -> Readiness
checkCopilotReadinessFromPR prNum resp =
  let reviews = V.toList (GH.getPullRequestResponseReviews resp)
      pr = GH.getPullRequestResponsePullRequest resp
      headSha = case pr of
        Just p -> TL.toStrict (GH.pullRequestHeadSha p)
        Nothing -> ""
      copilotReviews = filter isCopilotReview reviews
   in case reverse copilotReviews of
        [] ->
          NotReady $
            "No Copilot review yet on PR #"
              <> T.pack (show prNum)
              <> ". Wait for [PR READY] or [REVIEW TIMEOUT] from the event system."
        (latest : _) ->
          let reviewSha = TL.toStrict (GH.reviewCommitId latest)
              state = GH.reviewState latest
           in case state of
                Protobuf.Enumerated (Right GH.ReviewStateREVIEW_STATE_APPROVED) ->
                  Ready
                Protobuf.Enumerated (Right GH.ReviewStateREVIEW_STATE_CHANGES_REQUESTED) ->
                  if headSha /= reviewSha && not (T.null headSha) && not (T.null reviewSha)
                    then Ready
                    else
                      NotReady $
                        "Copilot requested changes on PR #"
                          <> T.pack (show prNum)
                          <> ". Wait for the agent to push fixes ([FIXES PUSHED]) or use force=true."
                Protobuf.Enumerated (Right GH.ReviewStateREVIEW_STATE_COMMENTED) ->
                  if headSha /= reviewSha && not (T.null headSha) && not (T.null reviewSha)
                    then Ready
                    else
                      NotReady $
                        "Copilot commented on PR #"
                          <> T.pack (show prNum)
                          <> ". Wait for the agent to address comments ([FIXES PUSHED]) or use force=true."
                _ -> Ready

-- | Check if a review is from Copilot (author login contains "copilot").
isCopilotReview :: GH.Review -> Bool
isCopilotReview r =
  case GH.reviewAuthor r of
    Just user ->
      let login = T.toLower (TL.toStrict (GH.userLogin user))
       in T.isInfixOf "copilot" login
    Nothing -> False

-- | Extract the agent name (last dot-segment) from a branch name.
-- After the unified namespace change, the last segment IS the agent name (suffixed).
extractAgentName :: Text -> Maybe Text
extractAgentName branch
  | T.null branch = Nothing
  | otherwise = case reverse (T.splitOn "." branch) of
      [] -> Nothing
      (slug : _) -> Just slug

-- | Execute the actual merge after readiness check passes.
doMerge :: MergePRArgs -> Eff Effects (Either Text MergePROutput)
doMerge args = do
  let req =
        MP.MergePrRequest
          { MP.mergePrRequestPrNumber = fromIntegral (mprPrNumber args),
            MP.mergePrRequestStrategy = maybe "" TL.fromStrict (mprStrategy args),
            MP.mergePrRequestWorkingDir = maybe "" TL.fromStrict (mprWorkingDir args)
          }
  result <- suspendEffect @MergePRMergePr req
  case result of
    Left err -> do
      void $ suspendEffect_ @LogError (Log.ErrorRequest {Log.errorRequestMessage = TL.fromStrict ("MergePR: failed: " <> T.pack (show err)), Log.errorRequestFields = ""})
      pure $ Left (T.pack (show err))
    Right resp -> do
      let branchName = TL.toStrict (MP.mergePrResponseBranchName resp)
          mergeSuccess = MP.mergePrResponseSuccess resp
          mergeMsg = TL.toStrict (MP.mergePrResponseMessage resp)
          gitFetched = MP.mergePrResponseGitFetched resp

      void $ suspendEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = TL.fromStrict ("MergePR: " <> mergeMsg), Log.infoRequestFields = ""})

      pullOk <-
        if mergeSuccess
          then do
            let eventPayload =
                  BSL.toStrict $
                    Aeson.encode $
                      object
                        [ "pr_number" .= mprPrNumber args,
                          "success" .= True
                        ]
            void $
              suspendEffect_ @LogEmitEvent
                ( Log.EmitEventRequest
                    { Log.emitEventRequestEventType = "pr.merged",
                      Log.emitEventRequestPayload = eventPayload,
                      Log.emitEventRequestTimestamp = 0
                    }
                )

            -- Fast-forward local branch after merge
            pullOkStatus <- do
              let pullReq =
                    Proc.RunRequest
                      { Proc.runRequestCommand = "git",
                        Proc.runRequestArgs = V.fromList ["pull"],
                        Proc.runRequestWorkingDir = maybe "" TL.fromStrict (mprWorkingDir args),
                        Proc.runRequestEnv = Map.empty,
                        Proc.runRequestTimeoutMs = 30000
                      }
              pullResult <- suspendEffect @ProcessRun pullReq
              case pullResult of
                Left _ -> pure False
                Right pullResp -> pure (Proc.runResponseExitCode pullResp == 0)

            -- Auto-cleanup: close agent tab, remove worktree, unregister
            case extractAgentName branchName of
              Just slug -> do
                let cleanupReq =
                      Agent.CleanupRequest
                        { Agent.cleanupRequestIssue = TL.fromStrict slug,
                          Agent.cleanupRequestForce = True,
                          Agent.cleanupRequestSubrepo = ""
                        }
                cleanupResult <- suspendEffect @AgentCleanup cleanupReq
                case cleanupResult of
                  Left cleanupErr ->
                    void $
                      suspendEffect_ @LogInfo
                        ( Log.InfoRequest
                            { Log.infoRequestMessage = TL.fromStrict ("MergePR: cleanup failed (non-fatal): " <> T.pack (show cleanupErr)),
                              Log.infoRequestFields = ""
                            }
                        )
                  Right _ ->
                    void $
                      suspendEffect_ @LogInfo
                        ( Log.InfoRequest
                            { Log.infoRequestMessage = TL.fromStrict ("MergePR: cleaned up agent " <> slug),
                              Log.infoRequestFields = ""
                            }
                        )
              Nothing -> pure ()

            pure pullOkStatus
          else pure True

      pure $
        Right $
          MergePROutput
            { mpoSuccess = mergeSuccess,
              mpoMessage = mergeMsg,
              mpoGitFetched = gitFetched,
              mpoBranchName = branchName,
              mpoPullOk = pullOk
            }
