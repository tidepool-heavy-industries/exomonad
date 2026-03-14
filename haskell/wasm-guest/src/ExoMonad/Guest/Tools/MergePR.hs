{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Guest.Tools.MergePR
  ( MergePR,
    MergePRArgs (..),
  )
where

import Control.Monad (void)
import Control.Monad.Freer (Eff)
import Data.Maybe (fromMaybe)
import Data.Aeson (FromJSON, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Effects.Git qualified as Git
import Effects.Github qualified as GH
import Effects.Log qualified as Log
import Effects.MergePr qualified as MP
import Effects.Agent qualified as Agent
import ExoMonad.Effects.Agent (AgentCleanup)
import ExoMonad.Effects.Git (GitGetRepoInfo)
import ExoMonad.Effects.GitHub (GitHubGetPullRequest)
import ExoMonad.Effects.Log (LogEmitEvent, LogError, LogInfo)
import ExoMonad.Effects.MergePR (MergePRMergePr)
import ExoMonad.Guest.Tool.Class (MCPCallOutput, MCPTool (..), ToolEffects, errorResult, successResult)
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
    mpoGitFetched :: Bool
  }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON MergePROutput where
  toJSON (MergePROutput s m g) =
    object
      [ "success" .= s,
        "message" .= m,
        "git_fetched" .= g
      ]

instance MCPTool MergePR where
  type ToolArgs MergePR = MergePRArgs
  toolName = "merge_pr"
  toolDescription = "Merge a GitHub pull request and fetch changes. Checks Copilot review status before merging: requires either a clean review (no change requests) or commits pushed after the latest review. Use force=true to bypass (e.g., after [REVIEW TIMEOUT]). After merging, verify the build — especially when merging multiple PRs in parallel, as changes may interact."
  toolSchema =
    genericToolSchemaWith @MergePRArgs
      [ ("pr_number", "PR number to merge"),
        ("strategy", "Merge strategy: squash (default), merge, or rebase"),
        ("working_dir", "Working directory for git operations"),
        ("force", "Skip Copilot review check and merge immediately (use after [REVIEW TIMEOUT])")
      ]
  toolHandlerEff args = do
    let force = fromMaybe False (mprForce args)
    void $ suspendEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = TL.fromStrict ("MergePR: Merging PR #" <> T.pack (show (mprPrNumber args)) <> if force then " (force)" else ""), Log.infoRequestFields = ""})

    -- Readiness check: verify Copilot review status before merging
    if force
      then doMerge args
      else do
        readiness <- checkCopilotReadiness (mprPrNumber args)
        case readiness of
          Ready -> doMerge args
          NotReady reason -> do
            void $ suspendEffect_ @LogError (Log.ErrorRequest {Log.errorRequestMessage = TL.fromStrict ("MergePR: blocked: " <> reason), Log.errorRequestFields = ""})
            pure $ errorResult reason

-- | Copilot review readiness.
data Readiness = Ready | NotReady Text

-- | Check whether a PR is ready to merge based on Copilot review status.
checkCopilotReadiness :: Int -> Eff ToolEffects Readiness
checkCopilotReadiness prNum = do
  -- Get repo info for API calls
  repoInfoResult <- suspendEffect @GitGetRepoInfo (Git.GetRepoInfoRequest {Git.getRepoInfoRequestWorkingDir = "."})
  case repoInfoResult of
    Left _ -> pure Ready -- Can't determine repo info — allow merge
    Right repoInfo -> do
      let owner = TL.toStrict (Git.getRepoInfoResponseOwner repoInfo)
          repo = TL.toStrict (Git.getRepoInfoResponseName repoInfo)

      -- Fetch PR with reviews
      prResult <- suspendEffect @GitHubGetPullRequest GH.GetPullRequestRequest
        { GH.getPullRequestRequestOwner = TL.fromStrict owner
        , GH.getPullRequestRequestRepo = TL.fromStrict repo
        , GH.getPullRequestRequestNumber = fromIntegral prNum
        , GH.getPullRequestRequestIncludeReviews = True
        }

      case prResult of
        Left _ -> pure Ready -- API error — allow merge
        Right resp -> do
          let reviews = V.toList (GH.getPullRequestResponseReviews resp)
              pr = GH.getPullRequestResponsePullRequest resp
              headSha = case pr of
                Just p -> TL.toStrict (GH.pullRequestHeadSha p)
                Nothing -> ""

          -- Filter to Copilot reviews (author login contains "copilot")
          let copilotReviews = filter isCopilotReview reviews

          case reverse copilotReviews of
            [] -> pure $ NotReady $ "No Copilot review yet on PR #" <> T.pack (show prNum)
                  <> ". Wait for [PR READY] or [REVIEW TIMEOUT] from the event system."
            (latest:_) -> do
              -- Check the latest Copilot review
              let reviewSha = TL.toStrict (GH.reviewCommitId latest)
                  state = GH.reviewState latest
              case state of
                Protobuf.Enumerated (Right GH.ReviewStateREVIEW_STATE_APPROVED) ->
                  pure Ready

                Protobuf.Enumerated (Right GH.ReviewStateREVIEW_STATE_CHANGES_REQUESTED) ->
                  if headSha /= reviewSha && not (T.null headSha) && not (T.null reviewSha)
                    then pure Ready -- Fixes pushed after review
                    else pure $ NotReady $ "Copilot requested changes on PR #" <> T.pack (show prNum)
                          <> ". Wait for the agent to push fixes ([FIXES PUSHED]) or use force=true."

                Protobuf.Enumerated (Right GH.ReviewStateREVIEW_STATE_COMMENTED) ->
                  if headSha /= reviewSha && not (T.null headSha) && not (T.null reviewSha)
                    then pure Ready -- Commits after review
                    else pure $ NotReady $ "Copilot commented on PR #" <> T.pack (show prNum)
                          <> ". Wait for the agent to address comments ([FIXES PUSHED]) or use force=true."

                _ -> pure Ready -- PENDING or unknown — allow merge

-- | Check if a review is from Copilot (author login contains "copilot").
isCopilotReview :: GH.Review -> Bool
isCopilotReview r =
  case GH.reviewAuthor r of
    Just user ->
      let login = T.toLower (TL.toStrict (GH.userLogin user))
      in T.isInfixOf "copilot" login
    Nothing -> False

-- | Extract the slug (last dot-segment) from a branch name.
extractSlug :: Text -> Maybe Text
extractSlug branch
  | T.null branch = Nothing
  | otherwise = case reverse (T.splitOn "." branch) of
      [] -> Nothing
      (slug : _) -> Just slug

-- | Execute the actual merge after readiness check passes.
doMerge :: MergePRArgs -> Eff ToolEffects MCPCallOutput
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
      pure $ errorResult (T.pack (show err))
    Right resp -> do
      let output =
            MergePROutput
              { mpoSuccess = MP.mergePrResponseSuccess resp,
                mpoMessage = TL.toStrict (MP.mergePrResponseMessage resp),
                mpoGitFetched = MP.mergePrResponseGitFetched resp
              }
      void $ suspendEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = TL.fromStrict ("MergePR: " <> mpoMessage output), Log.infoRequestFields = ""})

      if mpoSuccess output
        then do
          let eventPayload = BSL.toStrict $ Aeson.encode $ object
                [ "pr_number" .= mprPrNumber args,
                  "success" .= True
                ]
          void $ suspendEffect_ @LogEmitEvent (Log.EmitEventRequest
            { Log.emitEventRequestEventType = "pr.merged",
              Log.emitEventRequestPayload = eventPayload,
              Log.emitEventRequestTimestamp = 0
            })

          -- Auto-cleanup: close agent tab, remove worktree, unregister
          let branchName = TL.toStrict (MP.mergePrResponseBranchName resp)
          case extractSlug branchName of
            Just slug -> do
              let cleanupReq = Agent.CleanupRequest
                    { Agent.cleanupRequestIssue = TL.fromStrict slug
                    , Agent.cleanupRequestForce = True
                    , Agent.cleanupRequestSubrepo = ""
                    }
              cleanupResult <- suspendEffect @AgentCleanup cleanupReq
              case cleanupResult of
                Left cleanupErr -> void $ suspendEffect_ @LogInfo (Log.InfoRequest
                  { Log.infoRequestMessage = TL.fromStrict ("MergePR: cleanup failed (non-fatal): " <> T.pack (show cleanupErr))
                  , Log.infoRequestFields = ""
                  })
                Right _ -> void $ suspendEffect_ @LogInfo (Log.InfoRequest
                  { Log.infoRequestMessage = TL.fromStrict ("MergePR: cleaned up agent " <> slug)
                  , Log.infoRequestFields = ""
                  })
            Nothing -> pure ()
        else pure ()

      pure $ successResult $
        object
          [ "success" .= mpoSuccess output,
            "message" .= mpoMessage output,
            "git_fetched" .= mpoGitFetched output,
            "next" .= ("Verify build: cargo check --workspace. Especially important after parallel merges — changes may interact." :: Text)
          ]
