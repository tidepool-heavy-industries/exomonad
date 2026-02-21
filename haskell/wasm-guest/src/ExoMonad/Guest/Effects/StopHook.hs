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

import Control.Monad (unless, void)
import Data.Aeson (ToJSON (..), encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Vector qualified as V
import Effects.Git qualified as Git
import Effects.Github qualified as GH
import Effects.Log qualified as Log
import ExoMonad.Effect.Class (runEffect_)
import ExoMonad.Effects.Git (getBranch, getRepoInfo, getStatus, hasUnpushedCommits)
import ExoMonad.Effects.GitHub (getPullRequest, getPullRequestForBranch, getPullRequestReviewComments)
import ExoMonad.Effects.Log (LogInfo)
import ExoMonad.Effects.Messaging (sendNote)
import ExoMonad.Guest.Types (StopDecision (..), StopHookOutput (..), allowStopResponse)
import GHC.Generics (Generic)
import Proto3.Suite.Types qualified as Protobuf
import System.Environment (lookupEnv)

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Helper for fire-and-forget logging via yield_effect.
logInfo_ :: Text -> IO ()
logInfo_ msg = void $ runEffect_ @LogInfo (Log.InfoRequest {Log.infoRequestMessage = TL.fromStrict msg, Log.infoRequestFields = ""})

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
      sendLifecycleNote "agent_completed" [("summary", "All checks passed: PR filed, Copilot approved")]
      pure out
    out@(StopHookOutput Block (Just reason)) -> do
      sendLifecycleNote "agent_blocked" [("reason", reason)]
      pure out
    out -> pure out

-- | Inner check logic (separated so we can wrap with lifecycle messaging).
runStopHookChecksInner :: IO StopHookOutput
runStopHookChecksInner = do
  branch <- getCurrentBranch
  unless (branch `elem` ["main", "master"]) $ do
    performNudges branch
  pure allowStopResponse

performNudges :: Text -> IO ()
performNudges branch = do
  -- Gather context
  repoInfoResult <- getRepoInfo (Git.GetRepoInfoRequest {Git.getRepoInfoRequestWorkingDir = "."})
  case repoInfoResult of
    Left _ -> pure ()
    Right repoInfo -> do
      let repoOwner = TL.toStrict (Git.getRepoInfoResponseOwner repoInfo)
          repoName = TL.toStrict (Git.getRepoInfoResponseName repoInfo)

      -- 1. Check if PR exists
      prResult <- getPullRequestForBranch GH.GetPullRequestForBranchRequest
        { GH.getPullRequestForBranchRequestOwner = TL.fromStrict repoOwner
        , GH.getPullRequestForBranchRequestRepo = TL.fromStrict repoName
        , GH.getPullRequestForBranchRequestBranch = TL.fromStrict branch
        }

      case prResult of
        Right prResp | GH.getPullRequestForBranchResponseFound prResp -> do
          case GH.getPullRequestForBranchResponsePullRequest prResp of
            Nothing -> pure ()
            Just pr -> do
              let prNum = fromIntegral (GH.pullRequestNumber pr) :: Int
              -- Check for unresolved review comments
              fullPrResult <- getPullRequest GH.GetPullRequestRequest
                { GH.getPullRequestRequestOwner = TL.fromStrict repoOwner
                , GH.getPullRequestRequestRepo = TL.fromStrict repoName
                , GH.getPullRequestRequestNumber = fromIntegral prNum
                , GH.getPullRequestRequestIncludeReviews = True
                }
              
              let reviews = case fullPrResult of
                    Right resp -> V.toList (GH.getPullRequestResponseReviews resp)
                    _ -> []
              
              commentsResult <- getPullRequestReviewComments GH.GetPullRequestReviewCommentsRequest
                { GH.getPullRequestReviewCommentsRequestOwner = TL.fromStrict repoOwner
                , GH.getPullRequestReviewCommentsRequestRepo = TL.fromStrict repoName
                , GH.getPullRequestReviewCommentsRequestNumber = fromIntegral prNum
                }
              
              let comments = case commentsResult of
                    Right resp -> V.toList (GH.getPullRequestReviewCommentsResponseComments resp)
                    _ -> []
              
              if not (null comments) || any (\r -> GH.reviewState r == Protobuf.Enumerated (Right GH.ReviewStateREVIEW_STATE_CHANGES_REQUESTED)) reviews
                then logInfo_ $ "PR #" <> T.pack (show prNum) <> " has unresolved review comments."
                else if null reviews
                  then logInfo_ $ "PR #" <> T.pack (show prNum) <> " is open but hasn't been reviewed yet."
                  else pure ()
        
        _ -> do
          -- No PR exists, check git status
          statusResult <- getStatus (Git.GetStatusRequest {Git.getStatusRequestWorkingDir = "."})
          let hasUncommitted = case statusResult of
                Right resp -> not (null (Git.getStatusResponseDirtyFiles resp)) 
                              || not (null (Git.getStatusResponseStagedFiles resp))
                _ -> False
          
          unpushedResult <- hasUnpushedCommits (Git.HasUnpushedCommitsRequest {Git.hasUnpushedCommitsRequestWorkingDir = ".", Git.hasUnpushedCommitsRequestRemote = "origin"})
          let hasUnpushed = case unpushedResult of
                Right resp -> Git.hasUnpushedCommitsResponseHasUnpushed resp
                _ -> False
          
          if hasUncommitted
            then logInfo_ $ "You have uncommitted changes on " <> branch <> " but no PR filed."
            else if hasUnpushed
              then logInfo_ $ "Commits on " <> branch <> " aren't in a PR yet."
              else pure ()

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
      let baseFields =
            [ "type" .= noteType,
              "agent_id" .= agent,
              "branch" .= branchName
            ]
          extraFields = map (\(k, v) -> Key.fromText k .= v) fields
          jsonContent = TL.toStrict $ TLE.decodeUtf8 $ encode $ object (baseFields <> extraFields)
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
