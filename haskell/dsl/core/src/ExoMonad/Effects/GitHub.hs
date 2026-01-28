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
    GitHub(..)
  , createIssue
  , updateIssue
  , closeIssue
  , reopenIssue
  , addIssueLabel
  , removeIssueLabel
  , addIssueAssignee
  , removeIssueAssignee
  , createPR
  , getIssue
  , listIssues
  , getPullRequest
  , listPullRequests
  , getPullRequestReviews
  , checkAuth

    -- * Types - Errors
  , GitHubError(..)
  , isRetryable

    -- * Types - Core
  , Repo(..)
  , defaultRepo
  , Author(..)
  , Comment(..)

    -- * Types - Issues
  , Issue(..)
  , IssueState(..)
  , IssueFilter(..)
  , defaultIssueFilter
  , CreateIssueInput(..)
  , defaultCreateIssueInput
  , UpdateIssueInput(..)
  , emptyUpdateIssueInput

    -- * Types - Pull Requests
  , PullRequest(..)
  , PRState(..)
  , PRFilter(..)
  , defaultPRFilter
  , Review(..)
  , ReviewState(..)
  , PRCreateSpec(..)
  , PRUrl(..)
  , ReviewComment(..)

    -- * Types - Legacy (kept for compatibility)
  , IssueUrl(..)
  , Label(..)

    -- * Runner (stub)
  , runGitHubStub
  ) where

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text
import Data.Time (UTCTime)
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, withText, (.:), (.:?), (.!=), (.=), object)
import GHC.Generics (Generic)
import Control.Monad.Freer (Eff, Member, send, interpret)

import ExoMonad.Effect (Log, logInfo)


-- ════════════════════════════════════════════════════════════════════════════
-- ERROR TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Errors that can occur during GitHub operations.
--
-- These are explicit errors returned from GitHub operations, making
-- failure modes visible in the type system rather than silently
-- returning empty results.
data GitHubError
  = GHNotFound Int              -- ^ Issue/PR doesn't exist
  | GHRateLimit UTCTime         -- ^ Rate limited until time
  | GHNetworkError Text         -- ^ Connection failed
  | GHTimeout                   -- ^ Request timed out
  | GHPermissionDenied Text     -- ^ Auth issue (e.g. not logged in)
  | GHUnexpected Int Text       -- ^ Other HTTP error (code, message)
  | GHParseError Text           -- ^ Failed to parse gh output as JSON
  deriving (Show, Eq, Generic)

-- | Check if a GitHub error is transient and should be retried.
isRetryable :: GitHubError -> Bool
isRetryable = \case
  GHRateLimit _    -> True
  GHNetworkError _ -> True
  GHTimeout        -> True
  GHUnexpected c _ -> c >= 500 && c < 600
  _                -> False

instance ToJSON GitHubError where
  toJSON (GHNotFound num) = object
    [ "error" .= ("not_found" :: Text)
    , "number" .= num
    ]
  toJSON (GHRateLimit resetAt) = object
    [ "error" .= ("rate_limit" :: Text)
    , "reset_at" .= resetAt
    ]
  toJSON (GHNetworkError msg) = object
    [ "error" .= ("network_error" :: Text)
    , "message" .= msg
    ]
  toJSON GHTimeout = object
    [ "error" .= ("timeout" :: Text)
    ]
  toJSON (GHPermissionDenied msg) = object
    [ "error" .= ("permission_denied" :: Text)
    , "message" .= msg
    ]
  toJSON (GHUnexpected code msg) = object
    [ "error" .= ("unexpected" :: Text)
    , "exit_code" .= code
    , "message" .= msg
    ]
  toJSON (GHParseError msg) = object
    [ "error" .= ("parse_error" :: Text)
    , "message" .= msg
    ]


-- ════════════════════════════════════════════════════════════════════════════
-- CORE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Repository identifier in "owner/repo" format.
newtype Repo = Repo { unRepo :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | Default GitHub repository for all tools.
--
-- Central constant to avoid hardcoding repo names throughout the codebase.
defaultRepo :: Repo
defaultRepo = Repo "tidepool-heavy-industries/exomonad"

-- | GitHub user/author info.
data Author = Author
  { authorLogin :: Text
  , authorName  :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON Author where
  parseJSON = withObject "Author" $ \v ->
    Author
      <$> v .: "login"
      <*> v .:? "name"

-- | Comment on an issue or PR.
data Comment = Comment
  { commentAuthor    :: Author
  , commentBody      :: Text
  , commentCreatedAt :: UTCTime
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
  toJSON IssueOpen   = "OPEN"
  toJSON IssueClosed = "CLOSED"

instance FromJSON IssueState where
  parseJSON = withText "IssueState" $ \case
    "OPEN"   -> pure IssueOpen
    "CLOSED" -> pure IssueClosed
    t        -> fail $ "Unknown issue state: " ++ show t

-- | GitHub issue.
data Issue = Issue
  { issueNumber   :: Int
  , issueTitle    :: Text
  , issueBody     :: Text
  , issueAuthor   :: Author
  , issueLabels   :: [Text]
  , issueState    :: IssueState
  , issueUrl      :: Text
  , issueComments :: [Comment]
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
      <*> (v .:? "comments" >>= \case
            Nothing -> pure []
            Just cs -> pure cs)

-- | Filter for listing issues.
data IssueFilter = IssueFilter
  { ifLabels :: [Text]
  , ifState  :: Maybe IssueState
  , ifLimit  :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Default issue filter (no filtering, no limit).
defaultIssueFilter :: IssueFilter
defaultIssueFilter = IssueFilter [] Nothing Nothing

-- | Input for creating a new issue.
data CreateIssueInput = CreateIssueInput
  { ciiRepo      :: Repo
  , ciiTitle     :: Text
  , ciiBody      :: Text
  , ciiLabels    :: [Text]
  , ciiAssignees :: [Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Default create issue input.
defaultCreateIssueInput :: Repo -> Text -> CreateIssueInput
defaultCreateIssueInput repo title = CreateIssueInput
  { ciiRepo      = repo
  , ciiTitle     = title
  , ciiBody      = ""
  , ciiLabels    = []
  , ciiAssignees = []
  }

-- | Input for updating an existing issue.
data UpdateIssueInput = UpdateIssueInput
  { uiiTitle     :: Maybe Text
  , uiiBody      :: Maybe Text
  , uiiState     :: Maybe IssueState
  , uiiLabels    :: Maybe [Text] -- ^ If Just, replaces all labels
  , uiiAssignees :: Maybe [Text] -- ^ If Just, replaces all assignees
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Empty update issue input (no changes).
emptyUpdateIssueInput :: UpdateIssueInput
emptyUpdateIssueInput = UpdateIssueInput
  { uiiTitle     = Nothing
  , uiiBody      = Nothing
  , uiiState     = Nothing
  , uiiLabels    = Nothing
  , uiiAssignees = Nothing
  }


-- ════════════════════════════════════════════════════════════════════════════
-- PULL REQUEST TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Pull request state.
data PRState = PROpen | PRClosed | PRMerged
  deriving (Show, Eq, Generic, Enum, Bounded)

instance ToJSON PRState where
  toJSON PROpen   = "OPEN"
  toJSON PRClosed = "CLOSED"
  toJSON PRMerged = "MERGED"

instance FromJSON PRState where
  parseJSON = withText "PRState" $ \case
    "OPEN"   -> pure PROpen
    "CLOSED" -> pure PRClosed
    "MERGED" -> pure PRMerged
    t        -> fail $ "Unknown PR state: " ++ show t

-- | Review state.
data ReviewState
  = ReviewPending
  | ReviewCommented
  | ReviewApproved
  | ReviewChangesRequested
  | ReviewDismissed
  deriving (Show, Eq, Generic, Enum, Bounded)

instance ToJSON ReviewState where
  toJSON ReviewPending          = "PENDING"
  toJSON ReviewCommented        = "COMMENTED"
  toJSON ReviewApproved         = "APPROVED"
  toJSON ReviewChangesRequested = "CHANGES_REQUESTED"
  toJSON ReviewDismissed        = "DISMISSED"

instance FromJSON ReviewState where
  parseJSON = withText "ReviewState" $ \case
    "PENDING"           -> pure ReviewPending
    "COMMENTED"         -> pure ReviewCommented
    "APPROVED"          -> pure ReviewApproved
    "CHANGES_REQUESTED" -> pure ReviewChangesRequested
    "DISMISSED"         -> pure ReviewDismissed
    t                   -> fail $ "Unknown review state: " ++ show t

-- | PR review.
data Review = Review
  { reviewAuthor :: Author
  , reviewBody   :: Text
  , reviewState  :: ReviewState
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
  { rcAuthor    :: Text
  , rcBody      :: Text
  , rcPath      :: Maybe Text
  , rcLine      :: Maybe Int
  , rcState     :: ReviewState
  , rcCreatedAt :: UTCTime
  , rcIsResolved :: Bool
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
    author <- (v .: "user" >>= (.: "login"))
              <|> (v .: "author")
              <|> (v .: "rcAuthor")

    -- Handle both "created_at" (GitHub API) and "createdAt" or "rcCreatedAt"
    createdAt <- v .: "created_at"
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
  { prNumber      :: Int
  , prTitle       :: Text
  , prBody        :: Text
  , prAuthor      :: Author
  , prLabels      :: [Text]
  , prState       :: PRState
  , prUrl         :: Text
  , prHeadRefName :: Text
  , prBaseRefName :: Text
  , prCreatedAt   :: UTCTime
  , prMergedAt    :: Maybe UTCTime
  , prComments    :: [Comment]
  , prReviews     :: [Review]
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
      <*> (v .:? "comments" >>= \case
            Nothing -> pure []
            Just cs -> pure cs)
      <*> (v .:? "reviews" >>= \case
            Nothing -> pure []
            Just rs -> pure rs)

-- | Filter for listing pull requests.
data PRFilter = PRFilter
  { pfState  :: Maybe PRState
  , pfBase   :: Maybe Text
  , pfLimit  :: Maybe Int
  , pfSearch :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Default PR filter (no filtering, no limit).
defaultPRFilter :: PRFilter
defaultPRFilter = PRFilter Nothing Nothing Nothing Nothing

-- | Spec for creating a PR.
data PRCreateSpec = PRCreateSpec
  { prcsRepo   :: Repo
  , prcsHead   :: Text -- ^ Source branch
  , prcsBase   :: Text -- ^ Target branch (e.g. "main")
  , prcsTitle  :: Text
  , prcsBody   :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Pull request URL.
newtype PRUrl = PRUrl { unPRUrl :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- LEGACY TYPES (kept for compatibility)
-- ════════════════════════════════════════════════════════════════════════════

-- | Issue URL (legacy, for CreateIssue return type).
newtype IssueUrl = IssueUrl { unIssueUrl :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | Label (legacy, for CreateIssue parameter).
newtype Label = Label { unLabel :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

data GitHub r where
  -- Issue operations
  CreateIssue :: CreateIssueInput -> GitHub (Either GitHubError Int)
    -- ^ Create an issue, returns the issue number or error.
  UpdateIssue :: Repo -> Int -> UpdateIssueInput -> GitHub (Either GitHubError ())
    -- ^ Update an existing issue.
  CloseIssue  :: Repo -> Int -> GitHub (Either GitHubError ())
    -- ^ Close an issue.
  ReopenIssue :: Repo -> Int -> GitHub (Either GitHubError ())
    -- ^ Reopen an issue.
  AddIssueLabel    :: Repo -> Int -> Text -> GitHub (Either GitHubError ())
    -- ^ Add a label to an issue.
  RemoveIssueLabel :: Repo -> Int -> Text -> GitHub (Either GitHubError ())
    -- ^ Remove a label from an issue.
  AddIssueAssignee    :: Repo -> Int -> Text -> GitHub (Either GitHubError ())
    -- ^ Add an assignee to an issue.
  RemoveIssueAssignee :: Repo -> Int -> Text -> GitHub (Either GitHubError ())
    -- ^ Remove an assignee from an issue.

  GetIssue    :: Repo -> Int -> Bool -> GitHub (Either GitHubError (Maybe Issue))
    -- ^ Get issue by number. Bool = include comments.
  ListIssues  :: Repo -> IssueFilter -> GitHub (Either GitHubError [Issue])
    -- ^ List issues with filter. Returns error on failure, empty list only when truly empty.

  -- Pull request operations
  CreatePR          :: PRCreateSpec -> GitHub (Either GitHubError PRUrl)
    -- ^ Create a pull request.
  GetPullRequest    :: Repo -> Int -> Bool -> GitHub (Either GitHubError (Maybe PullRequest))
    -- ^ Get PR by number. Bool = include comments and reviews.
  ListPullRequests  :: Repo -> PRFilter -> GitHub (Either GitHubError [PullRequest])
    -- ^ List PRs with filter. Returns error on failure, empty list only when truly empty.
  GetPullRequestReviews :: Repo -> Int -> GitHub (Either GitHubError [ReviewComment])
    -- ^ Get review comments for a PR.

  -- Auth
  CheckAuth :: GitHub Bool
    -- ^ Check if gh CLI is authenticated.


-- Smart constructors

createIssue :: Member GitHub effs => CreateIssueInput -> Eff effs (Either GitHubError Int)
createIssue input = send (CreateIssue input)

updateIssue :: Member GitHub effs => Repo -> Int -> UpdateIssueInput -> Eff effs (Either GitHubError ())
updateIssue repo num input = send (UpdateIssue repo num input)

closeIssue :: Member GitHub effs => Repo -> Int -> Eff effs (Either GitHubError ())
closeIssue repo num = send (CloseIssue repo num)

reopenIssue :: Member GitHub effs => Repo -> Int -> Eff effs (Either GitHubError ())
reopenIssue repo num = send (ReopenIssue repo num)

addIssueLabel :: Member GitHub effs => Repo -> Int -> Text -> Eff effs (Either GitHubError ())
addIssueLabel repo num label = send (AddIssueLabel repo num label)

removeIssueLabel :: Member GitHub effs => Repo -> Int -> Text -> Eff effs (Either GitHubError ())
removeIssueLabel repo num label = send (RemoveIssueLabel repo num label)

addIssueAssignee :: Member GitHub effs => Repo -> Int -> Text -> Eff effs (Either GitHubError ())
addIssueAssignee repo num assignee = send (AddIssueAssignee repo num assignee)

removeIssueAssignee :: Member GitHub effs => Repo -> Int -> Text -> Eff effs (Either GitHubError ())
removeIssueAssignee repo num assignee = send (RemoveIssueAssignee repo num assignee)

createPR :: Member GitHub effs => PRCreateSpec -> Eff effs (Either GitHubError PRUrl)
createPR spec = send (CreatePR spec)

getIssue :: Member GitHub effs => Repo -> Int -> Bool -> Eff effs (Either GitHubError (Maybe Issue))
getIssue repo number includeComments = send (GetIssue repo number includeComments)

listIssues :: Member GitHub effs => Repo -> IssueFilter -> Eff effs (Either GitHubError [Issue])
listIssues repo filt = send (ListIssues repo filt)

getPullRequest :: Member GitHub effs => Repo -> Int -> Bool -> Eff effs (Either GitHubError (Maybe PullRequest))
getPullRequest repo number includeDetails = send (GetPullRequest repo number includeDetails)

listPullRequests :: Member GitHub effs => Repo -> PRFilter -> Eff effs (Either GitHubError [PullRequest])
listPullRequests repo filt = send (ListPullRequests repo filt)

getPullRequestReviews :: Member GitHub effs => Repo -> Int -> Eff effs (Either GitHubError [ReviewComment])
getPullRequestReviews repo number = send (GetPullRequestReviews repo number)

checkAuth :: Member GitHub effs => Eff effs Bool
checkAuth = send CheckAuth


-- ════════════════════════════════════════════════════════════════════════════
-- STUB RUNNER
-- ════════════════════════════════════════════════════════════════════════════

-- | Stub runner that logs calls and returns stub errors.
--
-- All operations return errors indicating the stub is not implemented.
-- Use @runGitHubIO@ from @exomonad-github-interpreter@ for real implementation.
runGitHubStub :: Member Log effs => Eff (GitHub ': effs) a -> Eff effs a
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

  CheckAuth -> do
    logInfo "[GitHub:stub] CheckAuth called"
    pure False

  where
    showT :: Show a => a -> Text
    showT = Data.Text.pack . show
