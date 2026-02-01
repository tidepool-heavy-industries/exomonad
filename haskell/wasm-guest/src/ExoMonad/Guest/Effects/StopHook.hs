{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExoMonad.Guest.Effects.StopHook
  ( runStopHookChecks,
  )
where

import Control.Monad (unless, when)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Guest.HostCall
  ( callHost,
    host_git_get_branch,
    host_git_get_dirty_files,
    host_git_get_remote_url,
    host_git_has_unpushed_commits,
    host_github_get_pr_for_branch,
    host_github_get_pr_review_comments,
  )
import ExoMonad.Guest.Types (StopHookOutput, allowStopResponse, blockStopResponse)
import GHC.Generics (Generic)

-- ============================================================================
-- Types
-- ============================================================================

-- Input types for host calls
data EmptyInput = EmptyInput deriving (Show, Generic)

instance ToJSON EmptyInput where
  toJSON EmptyInput = object []

data GitHostInput = GitHostInput
  { workingDir :: Text,
    containerId :: Text
  }
  deriving (Show, Generic)

instance ToJSON GitHostInput where
  toJSON i =
    object
      [ "workingDir" .= workingDir i,
        "containerId" .= containerId i
      ]

-- Output types for host calls
data GitHostOutput a
  = Success a
  | Error GitError
  deriving (Show, Generic)

instance (FromJSON a) => FromJSON (GitHostOutput a) where
  parseJSON = Aeson.withObject "GitHostOutput" $ \v -> do
    kind <- v Aeson..: "kind"
    case kind :: Text of
      "Success" -> Success <$> v Aeson..: "payload"
      "Error" -> Error <$> v Aeson..: "payload"
      _ -> fail "Unknown kind"

data GitError = GitError
  { message :: Text
  }
  deriving (Show, Generic)

instance FromJSON GitError where
  parseJSON = Aeson.withObject "GitError" $ \v ->
    GitError <$> v Aeson..: "message"

data GitHubHostOutput a
  = GHSuccess a
  | GHError GitHubError
  deriving (Show, Generic)

instance (FromJSON a) => FromJSON (GitHubHostOutput a) where
  parseJSON = Aeson.withObject "GitHubHostOutput" $ \v -> do
    kind <- v Aeson..: "kind"
    case kind :: Text of
      "Success" -> GHSuccess <$> v Aeson..: "payload"
      "Error" -> GHError <$> v Aeson..: "payload"
      _ -> fail "Unknown kind"

data GitHubError = GitHubError
  { ghMessage :: Text,
    ghCode :: Text
  }
  deriving (Show, Generic)

instance FromJSON GitHubError where
  parseJSON = Aeson.withObject "GitHubError" $ \v ->
    GitHubError
      <$> v Aeson..: "message"
      <*> v Aeson..: "code"

data Repo = Repo
  { owner :: Text,
    name :: Text
  }
  deriving (Show, Generic)

instance ToJSON Repo

data PullRequest = PullRequest
  { prNumber :: Int,
    prTitle :: Text,
    prUrl :: Text
  }
  deriving (Show, Generic)

instance FromJSON PullRequest where
  parseJSON = Aeson.withObject "PullRequest" $ \v ->
    PullRequest
      <$> v Aeson..: "number"
      <*> v Aeson..: "title"
      <*> v Aeson..: "url"

data ReviewComment = ReviewComment
  { rcId :: Int,
    rcBody :: Text,
    rcAuthor :: Text
  }
  deriving (Show, Generic)

instance FromJSON ReviewComment where
  parseJSON = Aeson.withObject "ReviewComment" $ \v ->
    ReviewComment
      <$> v Aeson..: "id"
      <*> v Aeson..: "body"
      <*> v Aeson..: "author"

data GetPRInput = GetPRInput
  { repo :: Repo,
    head :: Text
  }
  deriving (Show, Generic)

instance ToJSON GetPRInput

data GetCommentsInput = GetCommentsInput
  { commentsRepo :: Repo,
    commentsPrNumber :: Int
  }
  deriving (Show, Generic)

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
  -- Get current branch
  branchResult <- callHost host_git_get_branch hostInput
  case branchResult of
    Left err -> pure $ blockStopResponse $ "Failed to get branch: " <> T.pack err
    Right (Error gitErr) -> pure $ blockStopResponse $ "Git error getting branch: " <> message gitErr
    Right (Success branch) -> do
      -- Check for unpushed commits
      unpushedResult <- callHost host_git_has_unpushed_commits hostInput
      case unpushedResult of
        Left err -> pure $ blockStopResponse $ "Failed to check unpushed commits: " <> T.pack err
        Right (Error gitErr) -> pure $ blockStopResponse $ "Git error checking unpushed commits: " <> message gitErr
        Right (Success hasUnpushed) ->
          if hasUnpushed
            then pure $ blockStopResponse $ "Your commits aren't pushed. Push with: git push -u origin " <> branch
            else checkPRFiled hostInput branch

checkPRFiled :: GitHostInput -> Text -> IO StopHookOutput
checkPRFiled hostInput branch = do
  -- Get remote URL
  remoteResult <- callHost host_git_get_remote_url hostInput
  case remoteResult of
    Left err -> pure $ blockStopResponse $ "Failed to get remote URL: " <> T.pack err
    Right (Error gitErr) -> pure $ blockStopResponse $ "Git error getting remote URL: " <> message gitErr
    Right (Success remoteUrl) -> do
      case parseGitHubUrl remoteUrl of
        Nothing -> pure $ blockStopResponse $ "Could not parse GitHub URL from remote: " <> remoteUrl
        Just (repoOwner, repoName) -> do
          let repo = Repo {owner = repoOwner, name = repoName}
          let prInput = GetPRInput {repo = repo, ExoMonad.Guest.Effects.StopHook.head = branch}

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

-- ============================================================================
-- Helpers
-- ============================================================================

parseGitHubUrl :: Text -> Maybe (Text, Text)
parseGitHubUrl url =
  let cleaned = T.replace "git@github.com:" "https://github.com/" url
      cleaned2 = T.replace ".git" "" cleaned
      parts = T.splitOn "/" cleaned2
   in if length parts >= 2
        then
          let owner = parts !! (length parts - 2)
              repo = parts !! (length parts - 1)
           in Just (owner, repo)
        else Nothing
