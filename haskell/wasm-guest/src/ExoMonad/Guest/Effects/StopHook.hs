{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Guest.Effects.StopHook
  ( runStopHookChecks,
    ReviewStatus (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, object, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.HostCall
  ( callHost,
    host_git_get_dirty_files,
    host_git_get_repo_info,
    host_git_has_unpushed_commits,
    host_github_get_pr_for_branch,
    host_wait_for_copilot_review,
  )
import ExoMonad.Guest.Types (StopHookOutput, allowStopResponse, blockStopResponse)
import GHC.Generics (Generic)

-- ============================================================================
-- Types
-- ============================================================================

data GitHostInput = GitHostInput
  { workingDir :: Text,
    containerId :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

-- Removed GitHostOutput wrapper - host functions now return T directly

data RepoInfo = RepoInfo
  { branch :: Text,
    owner :: Maybe Text,
    name :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

-- Removed GitHubHostOutput wrapper - host functions now return T directly

data Repo = Repo
  { repoOwner :: Text,
    repoName :: Text
  }
  deriving stock (Show, Generic)

instance ToJSON Repo where
  toJSON r =
    object
      [ "owner" .= repoOwner r,
        "name" .= repoName r
      ]

data PullRequest = PullRequest
  { prNumber :: Int,
    prTitle :: Text,
    prUrl :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON PullRequest where
  parseJSON = Aeson.withObject "PullRequest" $ \v ->
    PullRequest
      <$> v .: "number"
      <*> v .: "title"
      <*> v .: "url"

data GetPRInput = GetPRInput
  { repo :: Repo,
    prHead :: Text
  }
  deriving stock (Show, Generic)

instance ToJSON GetPRInput where
  toJSON i =
    object
      [ "repo" .= repo i,
        "head" .= prHead i
      ]

data WaitForCopilotReviewInput = WaitForCopilotReviewInput
  { wcrPrNumber :: Int,
    wcrTimeoutSecs :: Int,
    wcrPollIntervalSecs :: Int
  }
  deriving stock (Show, Generic)

instance ToJSON WaitForCopilotReviewInput where
  toJSON i =
    object
      [ "pr_number" .= wcrPrNumber i,
        "timeout_secs" .= wcrTimeoutSecs i,
        "poll_interval_secs" .= wcrPollIntervalSecs i
      ]

-- | Copilot review status (strongly typed).
-- Serialized to "reviewed", "pending", or "timeout" at the JSON boundary.
data ReviewStatus = Reviewed | Pending | Timeout
  deriving stock (Show, Eq, Generic)

instance ToJSON ReviewStatus where
  toJSON Reviewed = Aeson.String "reviewed"
  toJSON Pending = Aeson.String "pending"
  toJSON Timeout = Aeson.String "timeout"

instance FromJSON ReviewStatus where
  parseJSON = Aeson.withText "ReviewStatus" $ \case
    "reviewed" -> pure Reviewed
    "pending" -> pure Pending
    "timeout" -> pure Timeout
    other -> fail $ "Unknown review status: " <> T.unpack other

data CopilotReviewOutput = CopilotReviewOutput
  { croStatus :: ReviewStatus,
    croComments :: [CopilotComment]
  }
  deriving stock (Show, Generic)

instance FromJSON CopilotReviewOutput where
  parseJSON = Aeson.withObject "CopilotReviewOutput" $ \v ->
    CopilotReviewOutput
      <$> v .: "status"
      <*> v .: "comments"

data CopilotComment = CopilotComment
  { ccPath :: Text,
    ccLine :: Maybe Int,
    ccBody :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON CopilotComment where
  parseJSON = Aeson.withObject "CopilotComment" $ \v ->
    CopilotComment
      <$> v .: "path"
      <*> v .: "line"
      <*> v .: "body"

-- Removed CopilotHostOutput wrapper - host functions now return T directly

-- ============================================================================
-- Main Check Logic
-- ============================================================================

runStopHookChecks :: IO StopHookOutput
runStopHookChecks = do
  let hostInput =
        GitHostInput
          { workingDir = ".",
            containerId = "local"
          }

  -- Check 1: Uncommitted changes
  dirtyResult <- callHost host_git_get_dirty_files hostInput
  case dirtyResult of
    Left err -> pure $ blockStopResponse $ "Failed to check dirty files: " <> err
    Right dirtyFiles ->
      if not (null (dirtyFiles :: [Text]))
        then pure $ blockStopResponse "You have uncommitted changes. Commit them with a message describing your work."
        else checkUnpushedCommits hostInput

checkUnpushedCommits :: GitHostInput -> IO StopHookOutput
checkUnpushedCommits hostInput = do
  -- Get repo info (branch + GitHub owner/name)
  repoResult <- callHost host_git_get_repo_info hostInput
  case repoResult of
    Left err -> pure $ blockStopResponse $ "Failed to get repo info: " <> err
    Right repoInfo -> do
      -- Check for unpushed commits
      unpushedResult <- callHost host_git_has_unpushed_commits hostInput
      case unpushedResult of
        Left err -> pure $ blockStopResponse $ "Failed to check unpushed commits: " <> err
        Right hasUnpushed ->
          if hasUnpushed
            then pure $ blockStopResponse $ "Your commits aren't pushed. Push with: git push -u origin " <> branch repoInfo
            else checkPRFiled repoInfo

checkPRFiled :: RepoInfo -> IO StopHookOutput
checkPRFiled repoInfo = do
  case (owner repoInfo, name repoInfo) of
    (Nothing, _) -> pure $ blockStopResponse "Could not determine GitHub owner from remote URL"
    (_, Nothing) -> pure $ blockStopResponse "Could not determine GitHub repo name from remote URL"
    (Just repoOwner, Just repoName) -> do
      let repo = Repo {repoOwner = repoOwner, repoName = repoName}
      let prInput = GetPRInput {repo = repo, prHead = branch repoInfo}

      -- Check for PR
      prResult <- callHost host_github_get_pr_for_branch prInput
      case prResult of
        Left err -> pure $ blockStopResponse $ "Failed to check for PR: " <> err
        Right Nothing -> pure $ blockStopResponse "No PR filed for this branch. Use the file_pr tool to create a PR and wait for Copilot review."
        Right (Just pr) -> checkReviewComments repo pr

checkReviewComments :: Repo -> PullRequest -> IO StopHookOutput
checkReviewComments _repo pr = do
  -- Wait for Copilot to review the PR (polls with 30s interval, 5 min timeout)
  let waitInput =
        WaitForCopilotReviewInput
          { wcrPrNumber = prNumber pr,
            wcrTimeoutSecs = 300, -- 5 minutes
            wcrPollIntervalSecs = 30
          }

  reviewResult <- callHost host_wait_for_copilot_review waitInput
  case reviewResult of
    Left err -> pure $ blockStopResponse $ "Failed to wait for Copilot review: " <> err
    Right reviewOutput ->
      case croStatus reviewOutput of
        Timeout ->
          pure $
            blockStopResponse $
              "Copilot hasn't reviewed PR #"
                <> T.pack (show (prNumber pr))
                <> " yet. Wait for Copilot review or check if Copilot is enabled for this repo."
        Reviewed ->
          if null (croComments reviewOutput)
            then pure allowStopResponse
            else
              pure $
                blockStopResponse $
                  "Copilot left "
                    <> T.pack (show (length (croComments reviewOutput)))
                    <> " comment(s) on PR #"
                    <> T.pack (show (prNumber pr))
                    <> ". Address them, commit, push, then stop again."
        Pending ->
          pure $
            blockStopResponse $
              "Copilot review is still pending for PR #"
                <> T.pack (show (prNumber pr))
                <> ". Wait for review to complete."
