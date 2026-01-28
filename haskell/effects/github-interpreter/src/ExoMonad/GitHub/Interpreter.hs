-- | GitHub effect interpreter - Socket Client.
--
-- Implements GitHub effect by communicating with the exomonad-control-server
-- via Unix Domain Socket, which in turn uses the Rust Octocrab service.
--
-- = Usage
--
-- @
-- import ExoMonad.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
-- import ExoMonad.Effects.GitHub
--
-- main = runM $ runGitHubIO defaultGitHubConfig $ do
--   issues <- listIssues (Repo "owner/repo") defaultIssueFilter
--   prs <- listPullRequests (Repo "owner/repo") defaultPRFilter
--   ...
-- @
module ExoMonad.GitHub.Interpreter
  ( -- * Interpreter
    runGitHubIO

    -- * Configuration
  , GitHubConfig(..)
  , defaultGitHubConfig
  ) where

import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson (toJSON, fromJSON)
import Data.Aeson qualified as Aeson

import ExoMonad.Effects.SocketClient
  ( SocketConfig(..)
  , ServiceRequest(..)
  , ServiceResponse(..)
  , ServiceError(..)
  , sendRequest
  )
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesFileExist)

import ExoMonad.Effects.GitHub
  ( GitHub(..)
  , GitHubError(..)
  , Issue(..)
  , IssueFilter(..)
  , IssueState(..)
  , PullRequest(..)
  , PRFilter(..)
  , PRState(..)
  , Repo(..)
  , PRCreateSpec(..)
  , PRUrl(..)
  , ReviewComment(..)
  , Discussion(..)
  , DiscussionComment(..)
  , CreateIssueInput(..)
  , UpdateIssueInput(..)
  , Author(..)
  )


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for GitHub interpreter.
data GitHubConfig
  = GitHubSocketConfig
      { ghcSocketPath :: FilePath
      }
  deriving (Show, Eq)

-- | Default configuration (Socket mode).
defaultGitHubConfig :: GitHubConfig
defaultGitHubConfig = GitHubSocketConfig
  { ghcSocketPath = ".exomonad/sockets/control.sock"
  }


-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run GitHub effects using the socket client.
runGitHubIO :: LastMember IO effs => GitHubConfig -> Eff (GitHub ': effs) a -> Eff effs a
runGitHubIO (GitHubSocketConfig path) = interpret $ \case
  -- Issue operations
  CreateIssue input -> sendM $ socketCreateIssue path input
  UpdateIssue (Repo repo) num input -> sendM $ socketUpdateIssue path repo num input
  CloseIssue (Repo repo) num -> sendM $ socketCloseIssue path repo num
  ReopenIssue (Repo repo) num -> sendM $ socketReopenIssue path repo num
  AddIssueLabel (Repo repo) num label -> sendM $ socketAddIssueLabel path repo num label
  RemoveIssueLabel (Repo repo) num label -> sendM $ socketRemoveIssueLabel path repo num label
  AddIssueAssignee (Repo repo) num assignee -> sendM $ socketAddIssueAssignee path repo num assignee
  RemoveIssueAssignee (Repo repo) num assignee -> sendM $ socketRemoveIssueAssignee path repo num assignee
  GetIssue (Repo repo) num includeComments -> sendM $ socketGetIssue path repo num includeComments
  ListIssues (Repo repo) filt -> sendM $ socketListIssues path repo filt

  -- PR operations
  CreatePR spec -> sendM $ socketCreatePR path spec
  GetPullRequest (Repo repo) num _includeDetails -> sendM $ socketGetPR path repo num
  ListPullRequests (Repo repo) filt -> sendM $ socketListPullRequests path repo filt
  GetPullRequestReviews (Repo repo) num -> sendM $ socketGetPullRequestReviews path repo num
  GetDiscussion (Repo repo) num -> sendM $ socketGetDiscussion path repo num

  -- Auth
  CheckAuth -> sendM $ socketCheckAuth path


-- ════════════════════════════════════════════════════════════════════════════
-- SOCKET FUNCTIONS
-- ════════════════════════════════════════════════════════════════════════════

socketCheckAuth :: FilePath -> IO Bool
socketCheckAuth path = do
  exists <- doesFileExist path
  if not exists then pure False else do
    let req = GitHubCheckAuth
    result <- sendRequest (SocketConfig path 10000) req
    case result of
      Right (GitHubAuthResponse auth _) -> pure auth
      _ -> pure False

socketGetIssue :: FilePath -> Text -> Int -> Bool -> IO (Either GitHubError (Maybe Issue))
socketGetIssue path repo num _includeComments = do
  case parseRepo repo of
    Left err -> pure $ Left err
    Right (owner, repoName) -> do
      let req = GitHubGetIssue owner repoName num
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right (GitHubIssueResponse n t b s ls u) -> 
          pure $ Right $ Just $ Issue
            { issueNumber = n
            , issueTitle = t
            , issueBody = b
            , issueAuthor = Author "unknown" Nothing
            , issueLabels = ls
            , issueState = if s == "open" then IssueOpen else IssueClosed
            , issueUrl = u
            , issueComments = []
            }
        Right (ErrorResponse 404 _) -> pure $ Right Nothing
        Right (ErrorResponse code msg) -> pure $ Left $ GHUnexpected code msg
        Right _ -> pure $ Left $ GHParseError "Unexpected response type for GitHubGetIssue"
        Left err -> pure $ Left $ socketErrorToGitHubError err

socketCreateIssue :: FilePath -> CreateIssueInput -> IO (Either GitHubError Int)
socketCreateIssue path input = do
  case parseRepo (input.ciiRepo.unRepo) of
    Left err -> pure $ Left err
    Right (owner, repoName) -> do
      let req = GitHubCreateIssue owner repoName input.ciiTitle input.ciiBody input.ciiLabels
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right (GitHubIssueResponse n _ _ _ _ _) -> pure $ Right n
        Right (ErrorResponse code msg) -> pure $ Left $ GHUnexpected code msg
        Right _ -> pure $ Left $ GHParseError "Unexpected response type for GitHubCreateIssue"
        Left err -> pure $ Left $ socketErrorToGitHubError err

socketUpdateIssue :: FilePath -> Text -> Int -> UpdateIssueInput -> IO (Either GitHubError ())
socketUpdateIssue path repo num input = do
  case parseRepo repo of
    Left err -> pure $ Left err
    Right (owner, repoName) -> do
      let stateStr = case input.uiiState of
            Just IssueOpen -> Just "open"
            Just IssueClosed -> Just "closed"
            Nothing -> Nothing
      
      let req = GitHubUpdateIssue 
            { owner = owner
            , repo = repoName
            , number = num
            , title = input.uiiTitle
            , body = input.uiiBody
            , state = stateStr
            , labels = input.uiiLabels
            , assignees = input.uiiAssignees
            }
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right (GitHubIssueResponse {}) -> pure $ Right ()
        Right (OtelAckResponse) -> pure $ Right () -- Accept Ack too
        Right (ErrorResponse code msg) -> pure $ Left $ GHUnexpected code msg
        Right _ -> pure $ Left $ GHParseError "Unexpected response type for GitHubUpdateIssue"
        Left err -> pure $ Left $ socketErrorToGitHubError err

socketCloseIssue :: FilePath -> Text -> Int -> IO (Either GitHubError ())
socketCloseIssue path repo num = do
  socketUpdateIssue path repo num (UpdateIssueInput Nothing Nothing (Just IssueClosed) Nothing Nothing)

socketReopenIssue :: FilePath -> Text -> Int -> IO (Either GitHubError ())
socketReopenIssue path repo num = do
  socketUpdateIssue path repo num (UpdateIssueInput Nothing Nothing (Just IssueOpen) Nothing Nothing)

socketAddIssueLabel :: FilePath -> Text -> Int -> Text -> IO (Either GitHubError ())
socketAddIssueLabel path repo num label = do
  case parseRepo repo of
    Left err -> pure $ Left err
    Right (owner, repoName) -> do
      let req = GitHubAddIssueLabel owner repoName num label
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right OtelAckResponse -> pure $ Right ()
        Right (ErrorResponse code msg) -> pure $ Left $ GHUnexpected code msg
        Right _ -> pure $ Left $ GHParseError "Unexpected response type for GitHubAddIssueLabel"
        Left err -> pure $ Left $ socketErrorToGitHubError err

socketRemoveIssueLabel :: FilePath -> Text -> Int -> Text -> IO (Either GitHubError ())
socketRemoveIssueLabel path repo num label = do
  case parseRepo repo of
    Left err -> pure $ Left err
    Right (owner, repoName) -> do
      let req = GitHubRemoveIssueLabel owner repoName num label
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right OtelAckResponse -> pure $ Right ()
        Right (ErrorResponse code msg) -> pure $ Left $ GHUnexpected code msg
        Right _ -> pure $ Left $ GHParseError "Unexpected response type for GitHubRemoveIssueLabel"
        Left err -> pure $ Left $ socketErrorToGitHubError err

socketAddIssueAssignee :: FilePath -> Text -> Int -> Text -> IO (Either GitHubError ())
socketAddIssueAssignee path repo num assignee = do
  case parseRepo repo of
    Left err -> pure $ Left err
    Right (owner, repoName) -> do
      let req = GitHubAddIssueAssignee owner repoName num assignee
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right OtelAckResponse -> pure $ Right ()
        Right (ErrorResponse code msg) -> pure $ Left $ GHUnexpected code msg
        Right _ -> pure $ Left $ GHParseError "Unexpected response type for GitHubAddIssueAssignee"
        Left err -> pure $ Left $ socketErrorToGitHubError err

socketRemoveIssueAssignee :: FilePath -> Text -> Int -> Text -> IO (Either GitHubError ())
socketRemoveIssueAssignee path repo num assignee = do
  case parseRepo repo of
    Left err -> pure $ Left err
    Right (owner, repoName) -> do
      let req = GitHubRemoveIssueAssignee owner repoName num assignee
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right OtelAckResponse -> pure $ Right ()
        Right (ErrorResponse code msg) -> pure $ Left $ GHUnexpected code msg
        Right _ -> pure $ Left $ GHParseError "Unexpected response type for GitHubRemoveIssueAssignee"
        Left err -> pure $ Left $ socketErrorToGitHubError err

socketListIssues :: FilePath -> Text -> IssueFilter -> IO (Either GitHubError [Issue])
socketListIssues path repo filt = do
  case parseRepo repo of
    Left err -> pure $ Left err
    Right (owner, repoName) -> do
      let req = GitHubListIssues owner repoName (stateToText <$> filt.ifState) filt.ifLabels
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right (GitHubIssuesResponse issues) ->
          case fromJSON (toJSON issues) of
            Aeson.Success is -> pure $ Right is
            err -> pure $ Left $ GHParseError $ T.pack $ "Failed to parse issues: " <> show err
        Right (ErrorResponse code msg) -> pure $ Left $ GHUnexpected code msg
        Right _ -> pure $ Left $ GHParseError "Unexpected response type for GitHubListIssues"
        Left err -> pure $ Left $ socketErrorToGitHubError err

socketCreatePR :: FilePath -> PRCreateSpec -> IO (Either GitHubError PRUrl)
socketCreatePR path spec = do
  case parseRepo (spec.prcsRepo.unRepo) of
    Left err -> pure $ Left err
    Right (owner, repoName) -> do
      let req = GitHubCreatePR owner repoName spec.prcsTitle spec.prcsBody spec.prcsHead spec.prcsBase
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right (GitHubPRResponse _ url _) -> pure $ Right $ PRUrl url
        Right (ErrorResponse code msg) -> pure $ Left $ GHUnexpected code msg
        Right _ -> pure $ Left $ GHParseError "Unexpected response type for GitHubCreatePR"
        Left err -> pure $ Left $ socketErrorToGitHubError err

socketGetPR :: FilePath -> Text -> Int -> IO (Either GitHubError (Maybe PullRequest))
socketGetPR path repo num = do
  case parseRepo repo of
    Left err -> pure $ Left err
    Right (owner, repoName) -> do
      let req = GitHubGetPR owner repoName num
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right (GitHubPRResponse n u s) -> 
          pure $ Right $ Just $ PullRequest
            { prNumber = n
            , prTitle = "unknown" -- Rust protocol needs update to return full PR details?
            , prBody = "unknown"
            , prAuthor = Author "unknown" Nothing
            , prState = if s == "open" then PROpen else if s == "merged" then PRMerged else PRClosed
            , prUrl = u
            , prHeadRef = "unknown"
            , prBaseRef = "unknown"
            , prCreatedAt = "unknown"
            , prMergedAt = Nothing
            , prLabels = []
            , prReviewDecision = Nothing
            }
        Right (ErrorResponse 404 _) -> pure $ Right Nothing
        Right (ErrorResponse code msg) -> pure $ Left $ GHUnexpected code msg
        Right _ -> pure $ Left $ GHParseError "Unexpected response type for GitHubGetPR"
        Left err -> pure $ Left $ socketErrorToGitHubError err

socketListPullRequests :: FilePath -> Text -> PRFilter -> IO (Either GitHubError [PullRequest])
socketListPullRequests path repo filt = do
  case parseRepo repo of
    Left err -> pure $ Left err
    Right (owner, repoName) -> do
      let s = case filt.pfState of
            Just PROpen -> Just "open"
            Just PRClosed -> Just "closed"
            Just PRMerged -> Just "all" -- Octocrab doesn't have "merged" filter easily? Or "closed" covers it?
            Nothing -> Nothing
      let req = GitHubListPullRequests owner repoName s filt.pfLimit
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right (GitHubPullRequestsResponse prs) ->
          case fromJSON (toJSON prs) of
            Aeson.Success ps -> pure $ Right ps
            err -> pure $ Left $ GHParseError $ T.pack $ "Failed to parse PRs: " <> show err
        Right (ErrorResponse code msg) -> pure $ Left $ GHUnexpected code msg
        Right _ -> pure $ Left $ GHParseError "Unexpected response type for GitHubListPullRequests"
        Left err -> pure $ Left $ socketErrorToGitHubError err

socketGetPullRequestReviews :: FilePath -> Text -> Int -> IO (Either GitHubError [ReviewComment])
socketGetPullRequestReviews path repo num = do
  case parseRepo repo of
    Left err -> pure $ Left err
    Right (owner, repoName) -> do
      let req = GitHubGetPullRequestReviews owner repoName num
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right (GitHubReviewsResponse reviews) ->
           case fromJSON (toJSON reviews) of
            Aeson.Success rs -> pure $ Right rs
            err -> pure $ Left $ GHParseError $ T.pack $ "Failed to parse reviews: " <> show err
        Right (ErrorResponse code msg) -> pure $ Left $ GHUnexpected code msg
        Right _ -> pure $ Left $ GHParseError "Unexpected response type for GitHubGetPullRequestReviews"
        Left err -> pure $ Left $ socketErrorToGitHubError err

socketGetDiscussion :: FilePath -> Text -> Int -> IO (Either GitHubError Discussion)
socketGetDiscussion path repo num = do
  case parseRepo repo of
    Left err -> pure $ Left err
    Right (owner, repoName) -> do
      let req = GitHubGetDiscussion owner repoName num
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right (GitHubDiscussionResponse n t b a u cs) -> 
          case fromJSON (toJSON cs) of
            Aeson.Success comments -> 
              pure $ Right $ Discussion
                { discNumber = n
                , discTitle = t
                , discBody = b
                , discAuthor = Author a Nothing
                , discUrl = u
                , discComments = comments
                }
            err -> pure $ Left $ GHParseError $ T.pack $ "Failed to parse discussion comments: " <> show err
        Right (ErrorResponse code msg) -> pure $ Left $ GHUnexpected code msg
        Right _ -> pure $ Left $ GHParseError "Unexpected response type for GitHubGetDiscussion"
        Left err -> pure $ Left $ socketErrorToGitHubError err


parseRepo :: Text -> Either GitHubError (Text, Text)
parseRepo repo = case T.splitOn "/" repo of
  [o, r] | not (T.null o) && not (T.null r) -> Right (o, r)
  _ -> Left $ GHParseError ("Invalid repository format (expected owner/repo): " <> repo)

stateToText :: IssueState -> Text
stateToText IssueOpen = "open"
stateToText IssueClosed = "closed"

socketErrorToGitHubError :: ServiceError -> GitHubError
socketErrorToGitHubError = \case
  SocketError msg -> GHNetworkError msg
  DecodeError msg -> GHParseError (T.pack msg)
  TimeoutError -> GHTimeout