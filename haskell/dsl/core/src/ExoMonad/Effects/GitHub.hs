{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | GitHub integration effect
--
-- Provides typed access to GitHub issues and pull requests via the @gh@ CLI.
--
-- = Usage
--
-- @
-- import ExoMonad.Effects.GitHub
-- import ExoMonad.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
--
-- main = runM $ runGitHubIO defaultGitHubConfig $ do
--   issues <- listIssues (Repo "owner/repo") defaultIssueFilter
--   prs <- listPullRequests (Repo "owner/repo") defaultPRFilter
--   ...
-- @
--
-- = Implementation
--
-- Operations are implemented via the @gh@ CLI tool in
-- @exomonad-github-interpreter@.
module ExoMonad.Effects.GitHub
  ( -- * Effect
    GitHub (..),
    createIssue,
    updateIssue,
    closeIssue,
    reopenIssue,
    addIssueLabel,
    removeIssueLabel,
    addIssueAssignee,
    removeIssueAssignee,
    createPR,
    getIssue,
    listIssues,
    getPullRequest,
    listPullRequests,
    getPullRequestReviews,
    getDiscussion,
    checkAuth,

    -- * Types - Errors
    GitHubError (..),
    isRetryable,

    -- * Types - Core
    Repo (..),
    authorLogin,
    authorName,
    defaultRepo,
    Author (..),
    Comment (..),

    -- * Types - Issues
    Issue (..),
    IssueState (..),
    IssueFilter (..),
    defaultIssueFilter,
    CreateIssueInput (..),
    defaultCreateIssueInput,
    UpdateIssueInput (..),
    emptyUpdateIssueInput,

    -- * Types - Pull Requests
    PullRequest (..),
    PRState (..),
    PRFilter (..),
    defaultPRFilter,
    Review (..),
    ReviewState (..),
    PRCreateSpec (..),
    PRUrl (..),
    ReviewComment (..),

    -- * Types - Discussions
    Discussion (..),
    DiscussionComment (..),

    -- * Types - Legacy (kept for compatibility)
    IssueUrl (..),
    Label (..),

    -- * Runner (stub)
    runGitHubStub,
  )
where

import Control.Applicative ((<|>))
import Polysemy (Sem, Member, interpret, makeSem)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.!=), (.:), (.:?), (.=))
import Data.Text (Text)
import Data.Text qualified
import Data.Time (UTCTime)
import ExoMonad.Effect (Log, logInfo)
import GHC.Generics (Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- ERROR TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Errors that can occur during GitHub operations.
--
-- These are explicit errors returned from GitHub operations, making
-- failure modes visible in the type system rather than silently
-- returning empty results.
data GitHubError
  = -- | Issue/PR doesn't exist
    GHNotFound Int
  | -- | Rate limited until time
    GHRateLimit UTCTime
  | -- | Connection failed
    GHNetworkError Text
  | -- | Request timed out
    GHTimeout
  | -- | Auth issue (e.g. not logged in)
    GHPermissionDenied Text
  | -- | Other HTTP error (code, message)
    GHUnexpected Int Text
  | -- | Failed to parse gh output as JSON
    GHParseError Text
  deriving (Show, Eq, Generic)

-- | Check if a GitHub error is transient and should be retried.
isRetryable :: GitHubError -> Bool
isRetryable = \case
  GHRateLimit _ -> True
  GHNetworkError _ -> True
  GHTimeout -> True
  GHUnexpected c _ -> c >= 500 && c < 600
  _ -> False

instance ToJSON GitHubError where
  toJSON (GHNotFound num) =
    object
      [ "error" .= ("not_found" :: Text),
        "number" .= num
      ]
  toJSON (GHRateLimit resetAt) =
    object
      [ "error" .= ("rate_limit" :: Text),
        "reset_at" .= resetAt
      ]
  toJSON (GHNetworkError msg) =
    object
      [ "error" .= ("network_error" :: Text),
        "message" .= msg
      ]
  toJSON GHTimeout =
    object
      [ "error" .= ("timeout" :: Text)
      ]
  toJSON (GHPermissionDenied msg) =
    object
      [ "error" .= ("permission_denied" :: Text),
        "message" .= msg
      ]
  toJSON (GHUnexpected code msg) =
    object
      [ "error" .= ("unexpected" :: Text),
        "exit_code" .= code,
        "message" .= msg
      ]
  toJSON (GHParseError msg) =
    object
      [ "error" .= ("parse_error" :: Text),
        "message" .= msg
      ]

-- ════════════════════════════════════════════════════════════════════════════
-- CORE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Repository identifier in "owner/repo" format.
newtype Repo = Repo {unRepo :: Text}
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | Default GitHub repository for all tools.
--
-- Central constant to avoid hardcoding repo names throughout the codebase.
defaultRepo :: Repo
defaultRepo = Repo "tidepool-heavy-industries/exomonad"

-- | GitHub user/author info.
data Author = Author
  { authorLogin :: Text,
    authorName :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Author where
  parseJSON = withObject "Author" $ \v ->
    Author
      <$> v .: "login"
      <*> v .:? "name"

-- | Comment on an issue or PR.
data Comment = Comment
  { commentAuthor :: Author,
    commentBody :: Text,
    commentCreatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Comment where
  parseJSON = withObject "Comment" $ \v ->
    Comment
      <$> v .: "author"
      <*> v .: "body"
      <*> v .: "createdAt"

-- ════════════════════════════════════════════════════════════════════════════
-- ISSUE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Issue state.
data IssueState = IssueOpen | IssueClosed
  deriving (Show, Eq, Generic, Enum, Bounded)

instance ToJSON IssueState where
  toJSON IssueOpen = "OPEN"
  toJSON IssueClosed = "CLOSED"

instance FromJSON IssueState where
  parseJSON = withText "IssueState" $ \case
    "OPEN" -> pure IssueOpen
    "CLOSED" -> pure IssueClosed
    t -> fail $ "Unknown issue state: " ++ show t

-- | GitHub issue.
data Issue = Issue
  { issueNumber :: Int,
    issueTitle :: Text,
    issueBody :: Text,
    issueAuthor :: Author,
    issueLabels :: [Text],
    issueState :: IssueState,
    issueUrl :: Text,
    issueComments :: [Comment]
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Issue where
  parseJSON = withObject "Issue" $ \v -> do
    labels <- v .:? "labels" .!= []
    labelNames <- mapM (.: "name") labels
    Issue
      <$> v .: "number"
      <*> v .: "title"
      <*> v .:? "body" .!= ""
      <*> v .: "author"
      <*> pure labelNames
      <*> v .: "state"
      <*> v .: "url"
      <*> ( v .:? "comments" >>= \case
              Nothing -> pure []
              Just cs -> pure cs
          )

-- | Filter for listing issues.
data IssueFilter = IssueFilter
  { ifLabels :: [Text],
    ifState :: Maybe IssueState,
    ifLimit :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Default issue filter (no filtering, no limit).
defaultIssueFilter :: IssueFilter
defaultIssueFilter = IssueFilter [] Nothing Nothing

-- | Input for creating a new issue.
data CreateIssueInput = CreateIssueInput
  { ciiRepo :: Repo,
    ciiTitle :: Text,
    ciiBody :: Text,
    ciiLabels :: [Text],
    ciiAssignees :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Default create issue input.
defaultCreateIssueInput :: Repo -> Text -> CreateIssueInput
defaultCreateIssueInput repo title =
  CreateIssueInput
    { ciiRepo = repo,
      ciiTitle = title,
      ciiBody = "",
      ciiLabels = [],
      ciiAssignees = []
    }

-- | Input for updating an existing issue.
data UpdateIssueInput = UpdateIssueInput
  { uiiTitle :: Maybe Text,
    uiiBody :: Maybe Text,
    uiiState :: Maybe IssueState,
    -- | If Just, replaces all labels
    uiiLabels :: Maybe [Text],
    -- | If Just, replaces all assignees
    uiiAssignees :: Maybe [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Empty update issue input (no changes).
emptyUpdateIssueInput :: UpdateIssueInput
emptyUpdateIssueInput =
  UpdateIssueInput
    { uiiTitle = Nothing,
      uiiBody = Nothing,
      uiiState = Nothing,
      uiiLabels = Nothing,
      uiiAssignees = Nothing
    }

-- ════════════════════════════════════════════════════════════════════════════
-- PULL REQUEST TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Pull request state.
data PRState = PROpen | PRClosed | PRMerged
  deriving (Show, Eq, Generic, Enum, Bounded)

instance ToJSON PRState where
  toJSON PROpen = "OPEN"
  toJSON PRClosed = "CLOSED"
  toJSON PRMerged = "MERGED"

instance FromJSON PRState where
  parseJSON = withText "PRState" $ \case
    "OPEN" -> pure PROpen
    "CLOSED" -> pure PRClosed
    "MERGED" -> pure PRMerged
    t -> fail $ "Unknown PR state: " ++ show t

-- | Review state.
data ReviewState
  = ReviewPending
  | ReviewCommented
  | ReviewApproved
  | ReviewChangesRequested
  | ReviewDismissed
  deriving (Show, Eq, Generic, Enum, Bounded)

instance ToJSON ReviewState where
  toJSON ReviewPending = "PENDING"
  toJSON ReviewCommented = "COMMENTED"
  toJSON ReviewApproved = "APPROVED"
  toJSON ReviewChangesRequested = "CHANGES_REQUESTED"
  toJSON ReviewDismissed = "DISMISSED"

instance FromJSON ReviewState where
  parseJSON = withText "ReviewState" $ \case
    "PENDING" -> pure ReviewPending
    "COMMENTED" -> pure ReviewCommented
    "APPROVED" -> pure ReviewApproved
    "CHANGES_REQUESTED" -> pure ReviewChangesRequested
    "DISMISSED" -> pure ReviewDismissed
    t -> fail $ "Unknown review state: " ++ show t

-- | PR review.
data Review = Review
  { reviewAuthor :: Author,
    reviewBody :: Text,
    reviewState :: ReviewState
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Review where
  parseJSON = withObject "Review" $ \v ->
    Review
      <$> v .: "author"
      <*> v .: "body"
      <*> v .: "state"

-- | Review comment (on code).
data ReviewComment = ReviewComment
  { rcAuthor :: Text,
    rcBody :: Text,
    rcPath :: Maybe Text,
    rcLine :: Maybe Int,
    rcState :: ReviewState,
    rcCreatedAt :: UTCTime,
    rcIsResolved :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON)

-- | FromJSON instance supports both GitHub API format and internal round-tripping.
--
-- Primary formats (GitHub API):
--   - "user": { "login": "..." } - Nested user object with login field
--   - "created_at": "..." - ISO 8601 timestamp
--   - "state": "COMMENTED" | "CHANGES_REQUESTED" | etc.
--
-- Fallback formats (for internal use / testing / tool results):
--   - "author": "..." - Direct author string
--   - "rcAuthor", "rcBody", "rcPath", "rcLine", "rcState", "rcCreatedAt" - Record field names
--     These enable round-trip serialization via the derived ToJSON instance.
instance FromJSON ReviewComment where
  parseJSON = withObject "ReviewComment" $ \v -> do
    -- Handle both "user": { "login": "..." } (GitHub API) and "author": "..." (internal/tool result)
    author <-
      (v .: "user" >>= (.: "login"))
        <|> (v .: "author")
        <|> (v .: "rcAuthor")

    -- Handle both "created_at" (GitHub API) and "createdAt" or "rcCreatedAt"
    createdAt <-
      v .: "created_at"
        <|> v .: "createdAt"
        <|> v .: "rcCreatedAt"

    ReviewComment
      author
      <$> (v .: "body" <|> v .: "rcBody")
      <*> (v .: "path" <|> v .: "rcPath" <|> pure Nothing)
      <*> (v .: "line" <|> v .: "rcLine" <|> pure Nothing)
      <*> (v .: "state" <|> v .: "rcState" <|> pure ReviewCommented)
      <*> pure createdAt
      <*> (v .: "isResolved" <|> v .: "rcIsResolved" <|> pure False)

-- | GitHub pull request.
data PullRequest = PullRequest
  { prNumber :: Int,
    prTitle :: Text,
    prBody :: Text,
    prAuthor :: Author,
    prLabels :: [Text],
    prState :: PRState,
    prUrl :: Text,
    prHeadRefName :: Text,
    prBaseRefName :: Text,
    prCreatedAt :: UTCTime,
    prMergedAt :: Maybe UTCTime,
    prComments :: [Comment],
    prReviews :: [Review]
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON PullRequest where
  parseJSON = withObject "PullRequest" $ \v -> do
    labels <- v .:? "labels" .!= []
    labelNames <- mapM (.: "name") labels
    PullRequest
      <$> v .: "number"
      <*> v .: "title"
      <*> v .:? "body" .!= ""
      <*> v .: "author"
      <*> pure labelNames
      <*> v .: "state"
      <*> v .: "url"
      <*> v .: "headRefName"
      <*> v .: "baseRefName"
      <*> v .: "createdAt"
      <*> v .:? "mergedAt"
      <*> ( v .:? "comments" >>= \case
              Nothing -> pure []
              Just cs -> pure cs
          )
      <*> ( v .:? "reviews" >>= \case
              Nothing -> pure []
              Just rs -> pure rs
          )

-- | Filter for listing pull requests.
data PRFilter = PRFilter
  { pfState :: Maybe PRState,
    pfBase :: Maybe Text,
    pfLimit :: Maybe Int,
    pfSearch :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Default PR filter (no filtering, no limit).
defaultPRFilter :: PRFilter
defaultPRFilter = PRFilter Nothing Nothing Nothing Nothing

-- | Spec for creating a PR.
data PRCreateSpec = PRCreateSpec
  { prcsRepo :: Repo,
    -- | Source branch
    prcsHead :: Text,
    -- | Target branch (e.g. "main")
    prcsBase :: Text,
    prcsTitle :: Text,
    prcsBody :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Pull request URL.
newtype PRUrl = PRUrl {unPRUrl :: Text}
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

-- ════════════════════════════════════════════════════════════════════════════
-- DISCUSSION TYPES
-- ════════════════════════════════════════════════════════════════════════════

data DiscussionComment = DiscussionComment
  { dcAuthor :: Author,
    dcBody :: Text,
    dcCreatedAt :: UTCTime,
    dcReplies :: [DiscussionComment]
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON DiscussionComment where
  parseJSON = withObject "DiscussionComment" $ \v ->
    DiscussionComment
      <$> v .: "author"
      <*> v .: "body"
      <*> v .: "createdAt"
      <*> ( v .:? "replies" >>= \case
              Nothing -> pure []
              Just rs -> pure rs
          )

data Discussion = Discussion
  { discNumber :: Int,
    discTitle :: Text,
    discBody :: Text,
    discAuthor :: Author,
    discUrl :: Text,
    discComments :: [DiscussionComment]
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Discussion where
  parseJSON = withObject "Discussion" $ \v ->
    Discussion
      <$> v .: "number"
      <*> v .: "title"
      <*> v .: "body"
      <*> v .: "author"
      <*> v .: "url"
      <*> ( v .:? "comments" >>= \case
              Nothing -> pure []
              Just cs -> pure cs
          )

-- ════════════════════════════════════════════════════════════════════════════
-- LEGACY TYPES (kept for compatibility)
-- ════════════════════════════════════════════════════════════════════════════

-- | Issue URL (legacy, for CreateIssue return type).
newtype IssueUrl = IssueUrl {unIssueUrl :: Text}
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | Label (legacy, for CreateIssue parameter).
newtype Label = Label {unLabel :: Text}
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

data GitHub m a where
  -- Issue operations
  CreateIssue ::
    CreateIssueInput ->
    -- | Create an issue, returns the issue number or error.
    GitHub m (Either GitHubError Int)
  UpdateIssue ::
    Repo ->
    Int ->
    UpdateIssueInput ->
    -- | Update an existing issue.
    GitHub m (Either GitHubError ())
  CloseIssue ::
    Repo ->
    Int ->
    -- | Close an issue.
    GitHub m (Either GitHubError ())
  ReopenIssue ::
    Repo ->
    Int ->
    -- | Reopen an issue.
    GitHub m (Either GitHubError ())
  AddIssueLabel ::
    Repo ->
    Int ->
    Text ->
    -- | Add a label to an issue.
    GitHub m (Either GitHubError ())
  RemoveIssueLabel ::
    Repo ->
    Int ->
    Text ->
    -- | Remove a label from an issue.
    GitHub m (Either GitHubError ())
  AddIssueAssignee ::
    Repo ->
    Int ->
    Text ->
    -- | Add an assignee to an issue.
    GitHub m (Either GitHubError ())
  RemoveIssueAssignee ::
    Repo ->
    Int ->
    Text ->
    -- | Remove an assignee from an issue.
    GitHub m (Either GitHubError ())
  GetIssue ::
    Repo ->
    Int ->
    Bool ->
    -- | Get issue by number. Bool = include comments.
    GitHub m (Either GitHubError (Maybe Issue))
  ListIssues ::
    Repo ->
    IssueFilter ->
    -- | List issues with filter. Returns error on failure, empty list only when truly empty.
    GitHub m (Either GitHubError [Issue])
  -- Pull request operations
  CreatePR ::
    PRCreateSpec ->
    -- | Create a pull request.
    GitHub m (Either GitHubError PRUrl)
  GetPullRequest ::
    Repo ->
    Int ->
    Bool ->
    -- | Get PR by number. Bool = include comments and reviews.
    GitHub m (Either GitHubError (Maybe PullRequest))
  ListPullRequests ::
    Repo ->
    PRFilter ->
    -- | List PRs with filter. Returns error on failure, empty list only when truly empty.
    GitHub m (Either GitHubError [PullRequest])
  GetPullRequestReviews ::
    Repo ->
    Int ->
    -- | Get review comments for a PR.
    GitHub m (Either GitHubError [ReviewComment])
  -- Discussion operations
  GetDiscussion ::
    Repo ->
    Int ->
    -- | Get discussion by number.
    GitHub m (Either GitHubError Discussion)
  -- Auth
  CheckAuth ::
    -- | Check if gh CLI is authenticated.
    GitHub m Bool

makeSem ''GitHub

-- ════════════════════════════════════════════════════════════════════════════
-- STUB RUNNER
-- ════════════════════════════════════════════════════════════════════════════

-- | Stub runner that logs calls and returns stub errors.
--
-- All operations return errors indicating the stub is not implemented.
-- Use @runGitHubIO@ from @exomonad-github-interpreter@ for real implementation.
runGitHubStub :: (Member Log effs) => Sem (GitHub ': effs) a -> Sem effs a
runGitHubStub = interpret $ \case
  CreateIssue input -> do
    logInfo $ "[GitHub:stub] CreateIssue called: " <> input.ciiRepo.unRepo <> " - " <> input.ciiTitle
    pure $ Left $ GHUnexpected 1 "Stub: createIssue not implemented"
  UpdateIssue (Repo repo) num _ -> do
    logInfo $ "[GitHub:stub] UpdateIssue called: " <> repo <> " #" <> showT num
    pure $ Left $ GHUnexpected 1 "Stub: updateIssue not implemented"
  CloseIssue (Repo repo) num -> do
    logInfo $ "[GitHub:stub] CloseIssue called: " <> repo <> " #" <> showT num
    pure $ Left $ GHUnexpected 1 "Stub: closeIssue not implemented"
  ReopenIssue (Repo repo) num -> do
    logInfo $ "[GitHub:stub] ReopenIssue called: " <> repo <> " #" <> showT num
    pure $ Left $ GHUnexpected 1 "Stub: reopenIssue not implemented"
  AddIssueLabel (Repo repo) num label -> do
    logInfo $ "[GitHub:stub] AddIssueLabel called: " <> repo <> " #" <> showT num <> " label=" <> label
    pure $ Left $ GHUnexpected 1 "Stub: addIssueLabel not implemented"
  RemoveIssueLabel (Repo repo) num label -> do
    logInfo $ "[GitHub:stub] RemoveIssueLabel called: " <> repo <> " #" <> showT num <> " label=" <> label
    pure $ Left $ GHUnexpected 1 "Stub: removeIssueLabel not implemented"
  AddIssueAssignee (Repo repo) num assignee -> do
    logInfo $ "[GitHub:stub] AddIssueAssignee called: " <> repo <> " #" <> showT num <> " assignee=" <> assignee
    pure $ Left $ GHUnexpected 1 "Stub: addIssueAssignee not implemented"
  RemoveIssueAssignee (Repo repo) num assignee -> do
    logInfo $ "[GitHub:stub] RemoveIssueAssignee called: " <> repo <> " #" <> showT num <> " assignee=" <> assignee
    pure $ Left $ GHUnexpected 1 "Stub: removeIssueAssignee not implemented"
  CreatePR (PRCreateSpec (Repo repo) headBranch baseBranch title _) -> do
    logInfo $ "[GitHub:stub] CreatePR called: " <> repo <> " (" <> headBranch <> " -> " <> baseBranch <> ") - " <> title
    pure $ Left $ GHUnexpected 1 "Stub: createPR not implemented"
  GetIssue (Repo repo) num _ -> do
    logInfo $ "[GitHub:stub] GetIssue called: " <> repo <> " #" <> showT num
    pure $ Left $ GHUnexpected 1 "Stub: getIssue not implemented"
  ListIssues (Repo repo) _ -> do
    logInfo $ "[GitHub:stub] ListIssues called: " <> repo
    pure $ Left $ GHUnexpected 1 "Stub: listIssues not implemented"
  GetPullRequest (Repo repo) num _ -> do
    logInfo $ "[GitHub:stub] GetPullRequest called: " <> repo <> " #" <> showT num
    pure $ Left $ GHUnexpected 1 "Stub: getPullRequest not implemented"
  ListPullRequests (Repo repo) _ -> do
    logInfo $ "[GitHub:stub] ListPullRequests called: " <> repo
    pure $ Left $ GHUnexpected 1 "Stub: listPullRequests not implemented"
  GetPullRequestReviews (Repo repo) num -> do
    logInfo $ "[GitHub:stub] GetPullRequestReviews called: " <> repo <> " #" <> showT num
    pure $ Left $ GHUnexpected 1 "Stub: getPullRequestReviews not implemented"
  GetDiscussion (Repo repo) num -> do
    logInfo $ "[GitHub:stub] GetDiscussion called: " <> repo <> " #" <> showT num
    pure $ Left $ GHUnexpected 1 "Stub: getDiscussion not implemented"
  CheckAuth -> do
    logInfo "[GitHub:stub] CheckAuth called"
    pure False
  where
    showT :: (Show a) => a -> Text
    showT = Data.Text.pack . show
