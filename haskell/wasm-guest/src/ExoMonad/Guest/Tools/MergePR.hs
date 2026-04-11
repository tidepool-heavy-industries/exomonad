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
    workingDirOrDefault,
  )
where

import Control.Monad (void, when)
import Control.Monad.Freer (Eff)
import Data.Aeson (FromJSON, object, withObject, (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, explicitParseField, explicitParseFieldMaybe)
import Data.ByteString.Lazy qualified as BSL
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
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
import ExoMonad.Guest.Tool.Class (Effects, MCPCallOutput (..))
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
  parseJSON = withObject "MergePRArgs" $ \v -> do
    prNumber <- explicitParseField parseFlexibleInt v "pr_number"
    strategy <- v .:? "strategy"
    workingDir <- v .:? "working_dir"
    force <- explicitParseFieldMaybe parseFlexibleBool v "force"
    pure (MergePRArgs prNumber strategy workingDir force)

-- Accept `pr_number` as either a JSON Number or a numeric JSON String.
-- Some MCP harnesses serialize scalar tool arguments as strings regardless
-- of their declared type; these helpers let both shapes through so tools
-- remain callable from any harness.
parseFlexibleInt :: Aeson.Value -> Parser Int
parseFlexibleInt (Aeson.Number n) =
  let i = round n :: Int
   in if fromIntegral i == n
        then pure i
        else fail ("pr_number not an integer: " <> show n)
parseFlexibleInt (Aeson.String s) =
  case reads (T.unpack s) :: [(Int, String)] of
    [(i, "")] -> pure i
    _ -> fail ("pr_number string not a valid integer: " <> T.unpack s)
parseFlexibleInt v =
  fail ("pr_number must be a Number or numeric String, got: " <> show v)

parseFlexibleBool :: Aeson.Value -> Parser Bool
parseFlexibleBool (Aeson.Bool b) = pure b
parseFlexibleBool (Aeson.String s) =
  case T.toLower s of
    "true" -> pure True
    "false" -> pure False
    _ -> fail ("force string not a valid bool: " <> T.unpack s)
parseFlexibleBool v =
  fail ("force must be a Bool or bool-shaped String, got: " <> show v)

data MergePROutput = MergePROutput
  { mpoMerged :: Bool,        -- ^ The GitHub PR was successfully merged.
    mpoLocalSynced :: Bool,   -- ^ Local working branch is in sync with remote after the merge.
    mpoCleanupOk :: Bool,     -- ^ The post-merge cleanup (close tab, remove worktree) succeeded.
    mpoMessage :: Text,
    mpoBranchName :: Text,
    mpoPullError :: Maybe Text  -- ^ When mpoLocalSynced is False, the git pull stderr/reason.
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON MergePROutput where
  toJSON out =
    object
      [ "merged" .= mpoMerged out,
        "local_synced" .= mpoLocalSynced out,
        "cleanup_ok" .= mpoCleanupOk out,
        "message" .= mpoMessage out,
        "branch_name" .= mpoBranchName out,
        "pull_error" .= mpoPullError out
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
  let fullySuccess = mpoMerged output && mpoLocalSynced output
      res =
        object
          [ "success" .= fullySuccess,
            "merged" .= mpoMerged output,
            "local_synced" .= mpoLocalSynced output,
            "cleanup_ok" .= mpoCleanupOk output,
            "message" .= mpoMessage output,
            "branch_name" .= mpoBranchName output,
            "pull_error" .= mpoPullError output,
            "next" .= nextGuidance output
          ]
   in MCPCallOutput fullySuccess (Just res) (mergePRError output)

mergePRError :: MergePROutput -> Maybe Text
mergePRError output
  | fullySuccess = Nothing
  | isJust pullErr = Just (guidance <> " Pull error: " <> fromMaybe "" pullErr)
  | not (T.null msg) = Just (guidance <> " Details: " <> msg)
  | otherwise = Just guidance
  where
    fullySuccess = mpoMerged output && mpoLocalSynced output
    guidance = nextGuidance output
    pullErr = mpoPullError output
    msg = mpoMessage output

nextGuidance :: MergePROutput -> Text
nextGuidance output
  | not (mpoMerged output) = "Merge did not complete. Check `message` for the reason."
  | not (mpoLocalSynced output) = "PR merged but local branch is out of sync. Run `git pull --rebase` manually, then verify build with `cargo check --workspace`."
  | not (mpoCleanupOk output) =
      "PR merged and synced, but post-merge cleanup failed. Run `git branch -D "
        <> mpoBranchName output
        <> "` and cleanup the worktree manually if needed."
  | otherwise = "Verify build: cargo check --workspace."

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

-- | Helper for working directory defaults (defaults to ".").
workingDirOrDefault :: Maybe Text -> TL.Text
workingDirOrDefault = maybe "." TL.fromStrict

-- | Execute the actual merge after readiness check passes.
doMerge :: MergePRArgs -> Eff Effects (Either Text MergePROutput)
doMerge args = do
  let req =
        MP.MergePrRequest
          { MP.mergePrRequestPrNumber = fromIntegral (mprPrNumber args),
            MP.mergePrRequestStrategy = maybe "" TL.fromStrict (mprStrategy args),
            MP.mergePrRequestWorkingDir = workingDirOrDefault (mprWorkingDir args)
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

      void $ suspendEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = TL.fromStrict ("MergePR: " <> mergeMsg), Log.infoRequestFields = ""})

      (localSynced, pullError, cleanupOk) <-
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
            (syncOk, syncErr) <- do
              let pullReq =
                    Proc.RunRequest
                      { Proc.runRequestCommand = "git",
                        Proc.runRequestArgs = V.fromList ["pull"],
                        Proc.runRequestWorkingDir = workingDirOrDefault (mprWorkingDir args),
                        Proc.runRequestEnv = Map.empty,
                        Proc.runRequestTimeoutMs = 30000
                      }
              pullResult <- suspendEffect @ProcessRun pullReq
              case pullResult of
                Left err -> pure (False, Just (T.pack (show err)))
                Right pullResp ->
                  if Proc.runResponseExitCode pullResp == 0
                    then pure (True, Nothing)
                    else pure (False, Just (TL.toStrict (Proc.runResponseStderr pullResp)))

            -- Auto-cleanup: close agent tab, remove worktree, unregister
            cleanOk <- case extractAgentName branchName of
              Just slug -> do
                let cleanupReq =
                      Agent.CleanupRequest
                        { Agent.cleanupRequestIssue = TL.fromStrict slug,
                          Agent.cleanupRequestForce = True,
                          Agent.cleanupRequestSubrepo = ""
                        }
                cleanupResult <- suspendEffect @AgentCleanup cleanupReq
                case cleanupResult of
                  Left cleanupErr -> do
                    void $
                      suspendEffect_ @LogInfo
                        ( Log.InfoRequest
                            { Log.infoRequestMessage = TL.fromStrict ("MergePR: cleanup failed (non-fatal): " <> T.pack (show cleanupErr)),
                              Log.infoRequestFields = ""
                            }
                        )
                    pure False
                  Right _ -> do
                    void $
                      suspendEffect_ @LogInfo
                        ( Log.InfoRequest
                            { Log.infoRequestMessage = TL.fromStrict ("MergePR: cleaned up agent " <> slug),
                              Log.infoRequestFields = ""
                            }
                        )
                    pure True
              Nothing -> pure True

            pure (syncOk, syncErr, cleanOk)
          else pure (False, Nothing, True)

      pure $
        Right $
          MergePROutput
            { mpoMerged = mergeSuccess,
              mpoLocalSynced = localSynced,
              mpoCleanupOk = cleanupOk,
              mpoMessage = mergeMsg,
              mpoBranchName = branchName,
              mpoPullError = pullError
            }
