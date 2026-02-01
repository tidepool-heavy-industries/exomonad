{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Guest.Effects.StopHook
  ( runStopHookChecks,
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
    host_github_get_pr_review_comments,
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

-- Output types for host calls
data GitHostOutput a
  = Success a
  | Error GitError
  deriving stock (Show, Generic)

instance (FromJSON a) => FromJSON (GitHostOutput a) where
  parseJSON = Aeson.withObject "GitHostOutput" $ \v -> do
    kind <- v .: "kind"
    case kind :: Text of
      "Success" -> Success <$> v .: "payload"
      "Error" -> Error <$> v .: "payload"
      _ -> fail "Unknown kind"

data GitError = GitError
  { message :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data RepoInfo = RepoInfo
  { branch :: Text,
    owner :: Maybe Text,
    name :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data GitHubHostOutput a
  = GHSuccess a
  | GHError GitHubError
  deriving stock (Show, Generic)

instance (FromJSON a) => FromJSON (GitHubHostOutput a) where
  parseJSON = Aeson.withObject "GitHubHostOutput" $ \v -> do
    kind <- v .: "kind"
    case kind :: Text of
      "Success" -> GHSuccess <$> v .: "payload"
      "Error" -> GHError <$> v .: "payload"
      _ -> fail "Unknown kind"

data GitHubError = GitHubError
  { ghMessage :: Text,
    ghCode :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON GitHubError where
  parseJSON = Aeson.withObject "GitHubError" $ \v ->
    GitHubError
      <$> v .: "message"
      <*> v .: "code"

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

data ReviewComment = ReviewComment
  { rcId :: Int,
    rcBody :: Text,
    rcAuthor :: Text
  }
  deriving stock (Show, Generic)

instance FromJSON ReviewComment where
  parseJSON = Aeson.withObject "ReviewComment" $ \v ->
    ReviewComment
      <$> v .: "id"
      <*> v .: "body"
      <*> v .: "author"

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

data GetCommentsInput = GetCommentsInput
  { commentsRepo :: Repo,
    commentsPrNumber :: Int
  }
  deriving stock (Show, Generic)

instance ToJSON GetCommentsInput where
  toJSON i =
    object
      [ "repo" .= commentsRepo i,
        "pr_number" .= commentsPrNumber i
      ]

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
    Left err -> pure $ blockStopResponse $ "Failed to check dirty files: " <> T.pack err
    Right (Error gitErr) -> pure $ blockStopResponse $ "Git error checking dirty files: " <> message gitErr
    Right (Success dirtyFiles) ->
      if not (null (dirtyFiles :: [Text]))
        then pure $ blockStopResponse "You have uncommitted changes. Commit them with a message describing your work."
        else checkUnpushedCommits hostInput

checkUnpushedCommits :: GitHostInput -> IO StopHookOutput
checkUnpushedCommits hostInput = do
  -- Get repo info (branch + GitHub owner/name)
  repoResult <- callHost host_git_get_repo_info hostInput
  case repoResult of
    Left err -> pure $ blockStopResponse $ "Failed to get repo info: " <> T.pack err
    Right (Error gitErr) -> pure $ blockStopResponse $ "Git error getting repo info: " <> message gitErr
    Right (Success repoInfo) -> do
      -- Check for unpushed commits
      unpushedResult <- callHost host_git_has_unpushed_commits hostInput
      case unpushedResult of
        Left err -> pure $ blockStopResponse $ "Failed to check unpushed commits: " <> T.pack err
        Right (Error gitErr) -> pure $ blockStopResponse $ "Git error checking unpushed commits: " <> message gitErr
        Right (Success hasUnpushed) ->
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
        Left err -> pure $ blockStopResponse $ "Failed to check for PR: " <> T.pack err
        Right (GHError ghErr) -> pure $ blockStopResponse $ "GitHub error checking for PR: " <> ghMessage ghErr
        Right (GHSuccess Nothing) -> pure $ blockStopResponse "No PR filed for this branch. Use the file_pr tool to create a PR and wait for Copilot review."
        Right (GHSuccess (Just pr)) -> checkReviewComments repo pr

checkReviewComments :: Repo -> PullRequest -> IO StopHookOutput
checkReviewComments repo pr = do
  let commentsInput =
        GetCommentsInput
          { commentsRepo = repo,
            commentsPrNumber = prNumber pr
          }

  commentsResult <- callHost host_github_get_pr_review_comments commentsInput
  case commentsResult of
    Left err -> pure $ blockStopResponse $ "Failed to check review comments: " <> T.pack err
    Right (GHError ghErr) -> pure $ blockStopResponse $ "GitHub error checking review comments: " <> ghMessage ghErr
    Right (GHSuccess comments) ->
      if not (null (comments :: [ReviewComment]))
        then
          pure $
            blockStopResponse $
              "Copilot left comments to address on PR #"
                <> T.pack (show (prNumber pr))
                <> ". Fix these, commit, push, then stop again."
        else pure allowStopResponse
