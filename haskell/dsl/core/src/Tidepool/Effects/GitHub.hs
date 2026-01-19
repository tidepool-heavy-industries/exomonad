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
-- Read operations are implemented via the @gh@ CLI tool in
-- @tidepool-github-interpreter@. Write operations (CreateIssue) remain stubbed.
module Tidepool.Effects.GitHub
  ( -- * Effect
    GitHub(..)
  , createIssue
  , createPR
  , getIssue
  , listIssues
  , getPullRequest
  , listPullRequests
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

    -- * Types - Pull Requests
  , PullRequest(..)
  , PRState(..)
  , PRFilter(..)
  , defaultPRFilter
  , Review(..)
  , ReviewState(..)
  , PRCreateSpec(..)
  , PRUrl(..)

    -- * Types - Legacy (kept for compatibility)
  , IssueUrl(..)
  , Label(..)

    -- * Runner (stub)
  , runGitHubStub
  ) where

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
      <*> (v .:? "comments" >>= \case
            Nothing -> pure []
            Just cs -> pure cs)
      <*> (v .:? "reviews" >>= \case
            Nothing -> pure []
            Just rs -> pure rs)

-- | Filter for listing pull requests.
data PRFilter = PRFilter
  { pfState :: Maybe PRState
  , pfBase  :: Maybe Text
  , pfLimit :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Default PR filter (no filtering, no limit).
defaultPRFilter :: PRFilter
defaultPRFilter = PRFilter Nothing Nothing Nothing

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
  -- Legacy (stub - errors at runtime)
  CreateIssue :: Repo -> Text -> Text -> [Label] -> GitHub IssueUrl

  -- Issue operations
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

  -- Auth
  CheckAuth :: GitHub Bool
    -- ^ Check if gh CLI is authenticated.


-- Smart constructors

createIssue :: Member GitHub effs => Repo -> Text -> Text -> [Label] -> Eff effs IssueUrl
createIssue repo title body labels = send (CreateIssue repo title body labels)

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
  CreateIssue (Repo repo) title _ _ -> do
    logInfo $ "[GitHub:stub] CreateIssue called: " <> repo <> " - " <> title
    error "GitHub.createIssue: not implemented"

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

  CheckAuth -> do
    logInfo "[GitHub:stub] CheckAuth called"
    pure False

  where
    showT :: Show a => a -> Text
    showT = Data.Text.pack . show
