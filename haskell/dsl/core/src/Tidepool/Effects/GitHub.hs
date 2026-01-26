-- | GitHub integration effect
--
-- Provides typed access to GitHub issues and pull requests via the @gh@ CLI.
--
-- = Usage
--
-- @
-- import Tidepool.Effects.GitHub
-- import Tidepool.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
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
-- @tidepool-github-interpreter@.
module Tidepool.Effects.GitHub
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

    -- * Types - Core
  , Repo(..)
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
import Data.Aeson (FromJSON(..), ToJSON(..), withObject, withText, (.:), (.:?))
import GHC.Generics (Generic)
import Control.Monad.Freer (Eff, Member, send, interpret)

import Tidepool.Effect (Log, logInfo)


-- ════════════════════════════════════════════════════════════════════════════
-- CORE TYPES
-- ════════════════════════════════════════════════════════════════════════════

-- | Repository identifier in "owner/repo" format.
newtype Repo = Repo { unRepo :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

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
    labels <- v .: "labels"
    labelNames <- mapM (.: "name") labels
    Issue
      <$> v .: "number"
      <*> v .: "title"
      <*> v .: "body"
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
    labels <- v .: "labels"
    labelNames <- mapM (.: "name") labels
    PullRequest
      <$> v .: "number"
      <*> v .: "title"
      <*> v .: "body"
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
  CreateIssue :: CreateIssueInput -> GitHub Int
    -- ^ Create an issue, returns the issue number.
  UpdateIssue :: Repo -> Int -> UpdateIssueInput -> GitHub ()
    -- ^ Update an existing issue.
  CloseIssue  :: Repo -> Int -> GitHub ()
    -- ^ Close an issue.
  ReopenIssue :: Repo -> Int -> GitHub ()
    -- ^ Reopen an issue.
  AddIssueLabel    :: Repo -> Int -> Text -> GitHub ()
    -- ^ Add a label to an issue.
  RemoveIssueLabel :: Repo -> Int -> Text -> GitHub ()
    -- ^ Remove a label from an issue.
  AddIssueAssignee    :: Repo -> Int -> Text -> GitHub ()
    -- ^ Add an assignee to an issue.
  RemoveIssueAssignee :: Repo -> Int -> Text -> GitHub ()
    -- ^ Remove an assignee from an issue.

  GetIssue    :: Repo -> Int -> Bool -> GitHub (Maybe Issue)
    -- ^ Get issue by number. Bool = include comments.
  ListIssues  :: Repo -> IssueFilter -> GitHub [Issue]
    -- ^ List issues with filter.

  -- Pull request operations
  CreatePR          :: PRCreateSpec -> GitHub PRUrl
    -- ^ Create a pull request.
  GetPullRequest    :: Repo -> Int -> Bool -> GitHub (Maybe PullRequest)
    -- ^ Get PR by number. Bool = include comments and reviews.
  ListPullRequests  :: Repo -> PRFilter -> GitHub [PullRequest]
    -- ^ List PRs with filter.
  GetPullRequestReviews :: Repo -> Int -> GitHub [ReviewComment]
    -- ^ Get review comments for a PR.

  -- Auth
  CheckAuth :: GitHub Bool
    -- ^ Check if gh CLI is authenticated.


-- Smart constructors

createIssue :: Member GitHub effs => CreateIssueInput -> Eff effs Int
createIssue input = send (CreateIssue input)

updateIssue :: Member GitHub effs => Repo -> Int -> UpdateIssueInput -> Eff effs ()
updateIssue repo num input = send (UpdateIssue repo num input)

closeIssue :: Member GitHub effs => Repo -> Int -> Eff effs ()
closeIssue repo num = send (CloseIssue repo num)

reopenIssue :: Member GitHub effs => Repo -> Int -> Eff effs ()
reopenIssue repo num = send (ReopenIssue repo num)

addIssueLabel :: Member GitHub effs => Repo -> Int -> Text -> Eff effs ()
addIssueLabel repo num label = send (AddIssueLabel repo num label)

removeIssueLabel :: Member GitHub effs => Repo -> Int -> Text -> Eff effs ()
removeIssueLabel repo num label = send (RemoveIssueLabel repo num label)

addIssueAssignee :: Member GitHub effs => Repo -> Int -> Text -> Eff effs ()
addIssueAssignee repo num assignee = send (AddIssueAssignee repo num assignee)

removeIssueAssignee :: Member GitHub effs => Repo -> Int -> Text -> Eff effs ()
removeIssueAssignee repo num assignee = send (RemoveIssueAssignee repo num assignee)

createPR :: Member GitHub effs => PRCreateSpec -> Eff effs PRUrl
createPR spec = send (CreatePR spec)

getIssue :: Member GitHub effs => Repo -> Int -> Bool -> Eff effs (Maybe Issue)
getIssue repo number includeComments = send (GetIssue repo number includeComments)

listIssues :: Member GitHub effs => Repo -> IssueFilter -> Eff effs [Issue]
listIssues repo filt = send (ListIssues repo filt)

getPullRequest :: Member GitHub effs => Repo -> Int -> Bool -> Eff effs (Maybe PullRequest)
getPullRequest repo number includeDetails = send (GetPullRequest repo number includeDetails)

listPullRequests :: Member GitHub effs => Repo -> PRFilter -> Eff effs [PullRequest]
listPullRequests repo filt = send (ListPullRequests repo filt)

getPullRequestReviews :: Member GitHub effs => Repo -> Int -> Eff effs [ReviewComment]
getPullRequestReviews repo number = send (GetPullRequestReviews repo number)

checkAuth :: Member GitHub effs => Eff effs Bool
checkAuth = send CheckAuth


-- ════════════════════════════════════════════════════════════════════════════
-- STUB RUNNER
-- ════════════════════════════════════════════════════════════════════════════

-- | Stub runner that logs calls and errors on write operations.
--
-- Read operations return empty results; use @runGitHubIO@ from
-- @tidepool-github-interpreter@ for real implementation.
runGitHubStub :: Member Log effs => Eff (GitHub ': effs) a -> Eff effs a
runGitHubStub = interpret $ \case
  CreateIssue input -> do
    logInfo $ "[GitHub:stub] CreateIssue called: " <> input.ciiRepo.unRepo <> " - " <> input.ciiTitle
    error "GitHub.createIssue: not implemented"

  UpdateIssue (Repo repo) num _ -> do
    logInfo $ "[GitHub:stub] UpdateIssue called: " <> repo <> " #" <> showT num
    pure ()

  CloseIssue (Repo repo) num -> do
    logInfo $ "[GitHub:stub] CloseIssue called: " <> repo <> " #" <> showT num
    pure ()

  ReopenIssue (Repo repo) num -> do
    logInfo $ "[GitHub:stub] ReopenIssue called: " <> repo <> " #" <> showT num
    pure ()

  AddIssueLabel (Repo repo) num label -> do
    logInfo $ "[GitHub:stub] AddIssueLabel called: " <> repo <> " #" <> showT num <> " label=" <> label
    pure ()

  RemoveIssueLabel (Repo repo) num label -> do
    logInfo $ "[GitHub:stub] RemoveIssueLabel called: " <> repo <> " #" <> showT num <> " label=" <> label
    pure ()

  AddIssueAssignee (Repo repo) num assignee -> do
    logInfo $ "[GitHub:stub] AddIssueAssignee called: " <> repo <> " #" <> showT num <> " assignee=" <> assignee
    pure ()

  RemoveIssueAssignee (Repo repo) num assignee -> do
    logInfo $ "[GitHub:stub] RemoveIssueAssignee called: " <> repo <> " #" <> showT num <> " assignee=" <> assignee
    pure ()

  CreatePR (PRCreateSpec (Repo repo) headBranch baseBranch title _) -> do
    logInfo $ "[GitHub:stub] CreatePR called: " <> repo <> " (" <> headBranch <> " -> " <> baseBranch <> ") - " <> title
    error "GitHub.createPR: not implemented"

  GetIssue (Repo repo) num _ -> do
    logInfo $ "[GitHub:stub] GetIssue called: " <> repo <> " #" <> showT num
    pure Nothing

  ListIssues (Repo repo) _ -> do
    logInfo $ "[GitHub:stub] ListIssues called: " <> repo
    pure []

  GetPullRequest (Repo repo) num _ -> do
    logInfo $ "[GitHub:stub] GetPullRequest called: " <> repo <> " #" <> showT num
    pure Nothing

  ListPullRequests (Repo repo) _ -> do
    logInfo $ "[GitHub:stub] ListPullRequests called: " <> repo
    pure []

  GetPullRequestReviews (Repo repo) num -> do
    logInfo $ "[GitHub:stub] GetPullRequestReviews called: " <> repo <> " #" <> showT num
    pure []

  CheckAuth -> do
    logInfo "[GitHub:stub] CheckAuth called"
    pure False

  where
    showT :: Show a => a -> Text
    showT = Data.Text.pack . show
