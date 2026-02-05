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
    runGitHubIO,

    -- * Configuration
    GitHubConfig (..),
    defaultGitHubConfig,
  )
where

import Control.Lens ((^?))
import Data.Aeson (Value (..), fromJSON, toJSON)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (key, _String)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import ExoMonad.Effects.GitHub
  ( Author (..),
    Comment (..),
    CreateIssueInput (..),
    Discussion (..),
    GitHub (..),
    GitHubError (..),
    Issue (..),
    IssueFilter (..),
    IssueState (..),
    PRCreateSpec (..),
    PRFilter (..),
    PRState (..),
    PRUrl (..),
    PullRequest (..),
    Repo (..),
    Review (..),
    ReviewComment (..),
    ReviewState (..),
    UpdateIssueInput (..),
  )
import ExoMonad.Effects.SocketClient
  ( ServiceError (..),
    ServiceRequest (..),
    ServiceResponse (..),
    SocketConfig (..),
    sendRequest,
  )
import Polysemy (Member, Sem, embed, interpret)
import Polysemy.Embed (Embed)
import Polysemy.Error (Error, throw)
import System.Directory (doesFileExist)

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
defaultGitHubConfig =
  GitHubSocketConfig
    { ghcSocketPath = ".exomonad/sockets/control.sock"
    }

-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run GitHub effects using the socket client.
--
-- Errors from the socket client are thrown via the 'Error GitHubError' effect.
runGitHubIO :: (Member (Embed IO) r, Member (Error GitHubError) r) => GitHubConfig -> Sem (GitHub ': r) a -> Sem r a
runGitHubIO (GitHubSocketConfig path) = interpret $ \case
  -- Issue operations
  CreateIssue input -> embedEither $ socketCreateIssue path input
  UpdateIssue (Repo repo) num input -> embedEither $ socketUpdateIssue path repo num input
  CloseIssue (Repo repo) num -> embedEither $ socketCloseIssue path repo num
  ReopenIssue (Repo repo) num -> embedEither $ socketReopenIssue path repo num
  AddIssueLabel (Repo repo) num label -> embedEither $ socketAddIssueLabel path repo num label
  RemoveIssueLabel (Repo repo) num label -> embedEither $ socketRemoveIssueLabel path repo num label
  AddIssueAssignee (Repo repo) num assignee -> embedEither $ socketAddIssueAssignee path repo num assignee
  RemoveIssueAssignee (Repo repo) num assignee -> embedEither $ socketRemoveIssueAssignee path repo num assignee
  GetIssue (Repo repo) num includeComments -> embedEither $ socketGetIssue path repo num includeComments
  ListIssues (Repo repo) filt -> embedEither $ socketListIssues path repo filt
  -- PR operations
  CreatePR spec -> embedEither $ socketCreatePR path spec
  GetPullRequest (Repo repo) num includeDetails -> embedEither $ socketGetPR path repo num includeDetails
  ListPullRequests (Repo repo) filt -> embedEither $ socketListPullRequests path repo filt
  GetPullRequestReviews (Repo repo) num -> embedEither $ socketGetPullRequestReviews path repo num
  GetDiscussion (Repo repo) num -> embedEither $ socketGetDiscussion path repo num
  -- Auth
  CheckAuth -> embed $ socketCheckAuth path
  where
    embedEither :: (Member (Embed IO) r', Member (Error GitHubError) r') => IO (Either GitHubError a') -> Sem r' a'
    embedEither action = do
      result <- embed action
      case result of
        Left err -> throw err
        Right val -> pure val

-- ════════════════════════════════════════════════════════════════════════════
-- SOCKET FUNCTIONS
-- ════════════════════════════════════════════════════════════════════════════

socketCheckAuth :: FilePath -> IO Bool
socketCheckAuth path = do
  exists <- doesFileExist path
  if not exists
    then pure False
    else do
      let req = GitHubCheckAuth
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right (GitHubAuthResponse auth _) -> pure auth
        _ -> pure False

socketGetIssue :: FilePath -> Text -> Int -> Bool -> IO (Either GitHubError (Maybe Issue))
socketGetIssue path repo num includeComments = do
  case parseRepo repo of
    Left err -> pure $ Left err
    Right (owner, repoName) -> do
      let req = GitHubGetIssue {owner = owner, repo = repoName, number = num, includeComments = includeComments}
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right (GitHubIssueResponse n t b s ls u a cs) ->
          pure $
            Right $
              Just $
                Issue
                  { issueNumber = n,
                    issueTitle = t,
                    issueBody = b,
                    issueAuthor = Author a Nothing,
                    issueLabels = fromMaybe [] ls,
                    issueState = if s == "open" then IssueOpen else IssueClosed,
                    issueUrl = u,
                    issueComments = parseCommentValues (fromMaybe [] cs)
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
      let req =
            GitHubCreateIssue
              { owner = owner,
                repo = repoName,
                title = Just input.ciiTitle,
                body = Just input.ciiBody,
                labels = Just input.ciiLabels
              }
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right (GitHubIssueResponse n _ _ _ _ _ _ _) -> pure $ Right n
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

      let req =
            GitHubUpdateIssue
              { owner = owner,
                repo = repoName,
                number = num,
                title = input.uiiTitle,
                body = input.uiiBody,
                state = stateStr,
                labels = input.uiiLabels,
                assignees = input.uiiAssignees
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
      let req = GitHubAddIssueLabel {owner = owner, repo = repoName, number = num, label = label}
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
      let req = GitHubRemoveIssueLabel {owner = owner, repo = repoName, number = num, label = label}
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
      let req = GitHubAddIssueAssignee {owner = owner, repo = repoName, number = num, assignee = assignee}
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
      let req = GitHubRemoveIssueAssignee {owner = owner, repo = repoName, number = num, assignee = assignee}
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
      let req = GitHubListIssues {owner = owner, repo = repoName, state = stateToText <$> filt.ifState, labels = Just filt.ifLabels}
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
      let req =
            GitHubCreatePR
              { owner = owner,
                repo = repoName,
                title = Just spec.prcsTitle,
                body = Just spec.prcsBody,
                head = spec.prcsHead,
                base = spec.prcsBase
              }
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right (GitHubPRResponse _ _ _ _ url _ _ _ _ _ _ _ _) -> pure $ Right $ PRUrl url
        Right (ErrorResponse code msg) -> pure $ Left $ GHUnexpected code msg
        Right _ -> pure $ Left $ GHParseError "Unexpected response type for GitHubCreatePR"
        Left err -> pure $ Left $ socketErrorToGitHubError err

socketGetPR :: FilePath -> Text -> Int -> Bool -> IO (Either GitHubError (Maybe PullRequest))
socketGetPR path repo num includeDetails = do
  case parseRepo repo of
    Left err -> pure $ Left err
    Right (owner, repoName) -> do
      let req = GitHubGetPR {owner = owner, repo = repoName, number = num, includeDetails = includeDetails}
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right (GitHubPRResponse n t b a u s h ba c ma ls cs rs) -> do
          let createdAt = parseRFC3339 c
          let mergedAt = ma >>= parseRFC3339Maybe
          pure $
            Right $
              Just $
                PullRequest
                  { prNumber = n,
                    prTitle = t,
                    prBody = b,
                    prAuthor = Author a Nothing,
                    prState = if s == "open" then PROpen else if s == "merged" then PRMerged else PRClosed,
                    prUrl = u,
                    prHeadRefName = h,
                    prBaseRefName = ba,
                    prCreatedAt = createdAt,
                    prMergedAt = mergedAt,
                    prLabels = fromMaybe [] ls,
                    prComments = parseCommentValues (fromMaybe [] cs),
                    prReviews = parseReviewValues (fromMaybe [] rs)
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
      let req = GitHubListPullRequests {owner = owner, repo = repoName, state = s, limit = filt.pfLimit}
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
      let req = GitHubGetPullRequestReviews {owner = owner, repo = repoName, number = num}
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right (GitHubReviewsResponse reviews) ->
          case fromJSON (toJSON (fromMaybe [] reviews)) of
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
      let req = GitHubGetDiscussion {owner = owner, repo = repoName, number = num}
      result <- sendRequest (SocketConfig path 10000) req
      case result of
        Right (GitHubDiscussionResponse n t b a u cs) ->
          case fromJSON (toJSON (fromMaybe [] cs)) of
            Aeson.Success comments ->
              pure $
                Right $
                  Discussion
                    { discNumber = n,
                      discTitle = t,
                      discBody = b,
                      discAuthor = Author a Nothing,
                      discUrl = u,
                      discComments = comments
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

-- | Parse an ISO8601/RFC3339 timestamp, erroring on failure.
-- Handles both "Z" and "+00:00" offset formats (Rust's chrono emits the latter).
parseRFC3339 :: Text -> UTCTime
parseRFC3339 t = case iso8601ParseM (T.unpack t) of
  Just time -> time
  Nothing -> error $ "parseRFC3339: invalid timestamp: " <> T.unpack t

-- | Parse an ISO8601/RFC3339 timestamp, returning Nothing on failure.
parseRFC3339Maybe :: Text -> Maybe UTCTime
parseRFC3339Maybe t = iso8601ParseM (T.unpack t)

-- | Parse JSON comment values from Rust's GitHubDiscussionComment format into Haskell Comments.

-- Rust sends: {"author": "login", "body": "...", "created_at": "...", "replies": [...]}

-- Haskell expects: Comment { commentAuthor :: Author, commentBody :: Text, commentCreatedAt :: UTCTime }

parseCommentValues :: [Value] -> [Comment]
parseCommentValues = mapMaybe parseOneComment
  where
    parseOneComment v = do
      a <- v ^? key "author" . _String
      b <- v ^? key "body" . _String
      ca <- v ^? key "created_at" . _String
      let time = parseRFC3339 ca
      pure
        Comment
          { commentAuthor = Author a Nothing,
            commentBody = b,
            commentCreatedAt = time
          }

-- | Parse JSON review values from Rust's GitHubReviewComment format into Haskell Reviews.
-- Rust sends: {"author": "login", "body": "...", "state": "APPROVED", ...}
-- Haskell expects: Review { reviewAuthor :: Author, reviewBody :: Text, reviewState :: ReviewState }
parseReviewValues :: [Value] -> [Review]
parseReviewValues = mapMaybe parseOneReview
  where
    parseOneReview v = do
      a <- v ^? key "author" . _String
      b <- v ^? key "body" . _String
      let s = v ^? key "state" . _String
      let st = case s of
            Just ("APPROVED" :: Text) -> ReviewApproved
            Just "CHANGES_REQUESTED" -> ReviewChangesRequested
            Just "DISMISSED" -> ReviewDismissed
            Just "PENDING" -> ReviewPending
            _ -> ReviewCommented
      pure
        Review
          { reviewAuthor = Author a Nothing,
            reviewBody = b,
            reviewState = st
          }
