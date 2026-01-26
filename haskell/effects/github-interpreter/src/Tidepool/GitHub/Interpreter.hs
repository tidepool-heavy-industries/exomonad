-- | GitHub effect interpreter - gh CLI client.
--
-- Implements GitHub effect by calling the gh CLI tool.
-- Supports read operations for issues and pull requests.
--
-- = Usage
--
-- @
-- import Tidepool.GitHub.Interpreter (runGitHubIO, defaultGitHubConfig)
-- import Tidepool.Effects.GitHub
--
-- main = runM $ runGitHubIO defaultGitHubConfig $ do
--   issues <- listIssues (Repo "owner/repo") defaultIssueFilter
--   prs <- listPullRequests (Repo "owner/repo") defaultPRFilter
--   ...
-- @
--
-- = Requirements
--
-- Requires the @gh@ CLI tool to be installed and authenticated:
--
-- @
-- gh auth login
-- @
module Tidepool.GitHub.Interpreter
  ( -- * Interpreter
    runGitHubIO

    -- * Configuration
  , GitHubConfig(..)
  , defaultGitHubConfig

    -- * Low-Level CLI Access
  , ghIssueList
  , ghIssueView
  , ghPrList
  , ghPrView
  , ghPrCreate
  , ghPrReviews
  , ghAuthCheck
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (unless)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson (eitherDecode, FromJSON(..), withObject, (.:), (.:?), Value)
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Tidepool.Effects.GitHub
  ( GitHub(..)
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
  , ReviewState(..)
  , CreateIssueInput(..)
  , UpdateIssueInput(..)
  )


-- ════════════════════════════════════════════════════════════════════════════
-- CONFIGURATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Configuration for GitHub interpreter.
data GitHubConfig = GitHubConfig
  { ghcQuiet :: Bool
    -- ^ Suppress stderr warnings from gh CLI.
  }
  deriving (Show, Eq)

-- | Default configuration (suppress warnings).
defaultGitHubConfig :: GitHubConfig
defaultGitHubConfig = GitHubConfig
  { ghcQuiet = True
  }


-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run GitHub effects using the gh CLI.
--
-- This interpreter shells out to the gh command for each operation.
runGitHubIO :: LastMember IO effs => GitHubConfig -> Eff (GitHub ': effs) a -> Eff effs a
runGitHubIO config = interpret $ \case
  -- Issue operations
  CreateIssue input ->
    sendM $ ghIssueCreate config input

  UpdateIssue (Repo repo) num input ->
    sendM $ ghIssueEdit config repo num input

  CloseIssue (Repo repo) num ->
    sendM $ ghIssueClose config repo num

  ReopenIssue (Repo repo) num ->
    sendM $ ghIssueReopen config repo num

  AddIssueLabel (Repo repo) num label ->
    sendM $ ghIssueLabelAdd config repo num label

  RemoveIssueLabel (Repo repo) num label ->
    sendM $ ghIssueLabelRemove config repo num label

  AddIssueAssignee (Repo repo) num assignee ->
    sendM $ ghIssueAssigneeAdd config repo num assignee

  RemoveIssueAssignee (Repo repo) num assignee ->
    sendM $ ghIssueAssigneeRemove config repo num assignee

  GetIssue (Repo repo) num includeComments ->
    sendM $ ghIssueView config repo num includeComments

  ListIssues (Repo repo) filt ->
    sendM $ ghIssueList config repo filt

  -- PR operations
  CreatePR spec ->
    sendM $ ghPrCreate config spec

  GetPullRequest (Repo repo) num includeDetails ->
    sendM $ ghPrView config repo num includeDetails

  ListPullRequests (Repo repo) filt ->
    sendM $ ghPrList config repo filt

  GetPullRequestReviews (Repo repo) num ->
    sendM $ ghPrReviews config repo num

  -- Auth
  CheckAuth ->
    sendM $ ghAuthCheck


-- ════════════════════════════════════════════════════════════════════════════
-- CLI FUNCTIONS - ISSUES
-- ════════════════════════════════════════════════════════════════════════════

-- | Create an issue using gh CLI.
ghIssueCreate :: GitHubConfig -> CreateIssueInput -> IO Int
ghIssueCreate config input = do
  let args = [ "issue", "create"
             , "--repo", T.unpack input.ciiRepo.unRepo
             , "--title", T.unpack input.ciiTitle
             , "--body", T.unpack input.ciiBody
             ]
             ++ concatMap (\l -> ["--label", T.unpack l]) input.ciiLabels
             ++ concatMap (\a -> ["--assignee", T.unpack a]) input.ciiAssignees

  result <- runGhCommand config args
  case result of
    Left err -> do
      logDebug config $ "ghIssueCreate: gh command failed: " <> T.unpack err
      error $ "Failed to create issue: " <> T.unpack err
    Right output ->
      -- gh issue create returns the URL of the created issue, e.g.
      -- https://github.com/owner/repo/issues/123
      let parts = T.splitOn "/" (T.strip output)
      in case (reverse parts) of
        (numStr : _) -> case (reads (T.unpack numStr) :: [(Int, String)]) of
          [(n, "")] -> pure n
          _ -> error $ "ghIssueCreate: failed to parse issue number from " <> T.unpack output
        _ -> error $ "ghIssueCreate: unexpected output format: " <> T.unpack output


-- | Edit an issue using gh CLI.
ghIssueEdit :: GitHubConfig -> Text -> Int -> UpdateIssueInput -> IO ()
ghIssueEdit config repo num input = do
  let args = [ "issue", "edit", show num
             , "--repo", T.unpack repo
             ]
             ++ maybe [] (\t -> ["--title", T.unpack t]) input.uiiTitle
             ++ maybe [] (\b -> ["--body", T.unpack b]) input.uiiBody
             ++ maybe [] (\case
                           IssueOpen -> ["--state", "open"]
                           IssueClosed -> ["--state", "closed"]) input.uiiState
             ++ maybe [] (\ls -> ["--label", T.unpack $ T.intercalate "," ls]) input.uiiLabels
             ++ maybe [] (\as -> ["--assignee", T.unpack $ T.intercalate "," as]) input.uiiAssignees

  -- Only run if there are actual changes
  if length args <= 4 -- ["issue", "edit", numStr, "--repo", repo]
    then pure ()
    else do
      result <- runGhCommand config args
      case result of
        Left err -> logDebug config $ "ghIssueEdit: gh command failed: " <> T.unpack err
        Right _ -> pure ()


-- | Close an issue using gh CLI.
ghIssueClose :: GitHubConfig -> Text -> Int -> IO ()
ghIssueClose config repo num = do
  let args = ["issue", "close", show num, "--repo", T.unpack repo]
  result <- runGhCommand config args
  case result of
    Left err -> logDebug config $ "ghIssueClose: gh command failed: " <> T.unpack err
    Right _ -> pure ()


-- | Reopen an issue using gh CLI.
ghIssueReopen :: GitHubConfig -> Text -> Int -> IO ()
ghIssueReopen config repo num = do
  let args = ["issue", "reopen", show num, "--repo", T.unpack repo]
  result <- runGhCommand config args
  case result of
    Left err -> logDebug config $ "ghIssueReopen: gh command failed: " <> T.unpack err
    Right _ -> pure ()


-- | Add a label to an issue.
ghIssueLabelAdd :: GitHubConfig -> Text -> Int -> Text -> IO ()
ghIssueLabelAdd config repo num label = do
  let args = ["issue", "edit", show num, "--repo", T.unpack repo, "--add-label", T.unpack label]
  result <- runGhCommand config args
  case result of
    Left err -> logDebug config $ "ghIssueLabelAdd: gh command failed: " <> T.unpack err
    Right _ -> pure ()


-- | Remove a label from an issue.
ghIssueLabelRemove :: GitHubConfig -> Text -> Int -> Text -> IO ()
ghIssueLabelRemove config repo num label = do
  let args = ["issue", "edit", show num, "--repo", T.unpack repo, "--remove-label", T.unpack label]
  result <- runGhCommand config args
  case result of
    Left err -> logDebug config $ "ghIssueLabelRemove: gh command failed: " <> T.unpack err
    Right _ -> pure ()


-- | Add an assignee to an issue.
ghIssueAssigneeAdd :: GitHubConfig -> Text -> Int -> Text -> IO ()
ghIssueAssigneeAdd config repo num assignee = do
  let args = ["issue", "edit", show num, "--repo", T.unpack repo, "--add-assignee", T.unpack assignee]
  result <- runGhCommand config args
  case result of
    Left err -> logDebug config $ "ghIssueAssigneeAdd: gh command failed: " <> T.unpack err
    Right _ -> pure ()


-- | Remove an assignee from an issue.
ghIssueAssigneeRemove :: GitHubConfig -> Text -> Int -> Text -> IO ()
ghIssueAssigneeRemove config repo num assignee = do
  let args = ["issue", "edit", show num, "--repo", T.unpack repo, "--remove-assignee", T.unpack assignee]
  result <- runGhCommand config args
  case result of
    Left err -> logDebug config $ "ghIssueAssigneeRemove: gh command failed: " <> T.unpack err
    Right _ -> pure ()


-- | List issues using gh CLI.
ghIssueList :: GitHubConfig -> Text -> IssueFilter -> IO [Issue]
ghIssueList config repo filt = do
  let baseFields = "number,title,body,author,labels,state,url"
      args = ["issue", "list", "--repo", T.unpack repo, "--json", baseFields]
            ++ stateArgs filt.ifState
            ++ labelArgs filt.ifLabels
            ++ limitArgs filt.ifLimit

  result <- runGhCommand config args
  case result of
    Left err -> do
      logDebug config $ "ghIssueList: gh command failed: " <> T.unpack err
      pure []
    Right output ->
      case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 output) of
        Right issues -> pure issues
        Left decodeErr -> do
          logDebug config $ "ghIssueList: JSON decode failed: " <> decodeErr
          pure []

  where
    stateArgs Nothing           = []
    stateArgs (Just IssueOpen)   = ["--state", "open"]
    stateArgs (Just IssueClosed) = ["--state", "closed"]

    labelArgs [] = []
    labelArgs ls = concatMap (\l -> ["--label", T.unpack l]) ls

    limitArgs Nothing  = []
    limitArgs (Just n) = ["--limit", show n]


-- | View a single issue using gh CLI.
ghIssueView :: GitHubConfig -> Text -> Int -> Bool -> IO (Maybe Issue)
ghIssueView config repo num includeComments = do
  let baseFields = "number,title,body,author,labels,state,url"
      fields = if includeComments then baseFields <> ",comments" else baseFields
      args = ["issue", "view", show num, "--repo", T.unpack repo, "--json", fields]

  result <- runGhCommand config args
  case result of
    Left err -> do
      logDebug config $ "ghIssueView: gh command failed: " <> T.unpack err
      pure Nothing
    Right output
      | T.null (T.strip output) -> pure Nothing
      | otherwise ->
          case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 output) of
            Right issue -> pure $ Just issue
            Left decodeErr -> do
              logDebug config $ "ghIssueView: JSON decode failed: " <> decodeErr
              pure Nothing


-- ════════════════════════════════════════════════════════════════════════════
-- CLI FUNCTIONS - PULL REQUESTS
-- ════════════════════════════════════════════════════════════════════════════

-- | List pull requests using gh CLI.
ghPrList :: GitHubConfig -> Text -> PRFilter -> IO [PullRequest]
ghPrList config repo filt = do
  let baseFields = "number,title,body,author,labels,state,url,headRefName,baseRefName,createdAt,mergedAt"
      args = ["pr", "list", "--repo", T.unpack repo, "--json", baseFields]
            ++ stateArgs filt.pfState
            ++ baseArgs filt.pfBase
            ++ limitArgs filt.pfLimit
            ++ searchArgs filt.pfSearch

  result <- runGhCommand config args
  case result of
    Left err -> do
      logDebug config $ "ghPrList: gh command failed: " <> T.unpack err
      pure []
    Right output ->
      case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 output) of
        Right prs -> pure prs
        Left decodeErr -> do
          logDebug config $ "ghPrList: JSON decode failed: " <> decodeErr
          pure []

  where
    stateArgs Nothing         = []
    stateArgs (Just PROpen)   = ["--state", "open"]
    stateArgs (Just PRClosed) = ["--state", "closed"]
    stateArgs (Just PRMerged) = ["--state", "merged"]

    baseArgs Nothing  = []
    baseArgs (Just b) = ["--base", T.unpack b]

    limitArgs Nothing  = []
    limitArgs (Just n) = ["--limit", show n]

    searchArgs Nothing  = []
    searchArgs (Just s) = ["--search", T.unpack s]


-- | View a single pull request using gh CLI.
ghPrView :: GitHubConfig -> Text -> Int -> Bool -> IO (Maybe PullRequest)
ghPrView config repo num includeDetails = do
  let baseFields = "number,title,body,author,labels,state,url,headRefName,baseRefName,createdAt,mergedAt"
      fields = if includeDetails
               then baseFields <> ",comments,reviews"
               else baseFields
      args = ["pr", "view", show num, "--repo", T.unpack repo, "--json", fields]

  result <- runGhCommand config args
  case result of
    Left err -> do
      logDebug config $ "ghPrView: gh command failed: " <> T.unpack err
      pure Nothing
    Right output
      | T.null (T.strip output) -> pure Nothing
      | otherwise ->
          case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 output) of
            Right pr -> pure $ Just pr
            Left decodeErr -> do
              logDebug config $ "ghPrView: JSON decode failed: " <> decodeErr
              pure Nothing


-- | Create a pull request using gh CLI.
ghPrCreate :: GitHubConfig -> PRCreateSpec -> IO PRUrl
ghPrCreate config spec = do
  let baseArgs = if T.null spec.prcsBase then [] else ["--base", T.unpack spec.prcsBase]
      args = [ "pr", "create"
             , "--repo", T.unpack spec.prcsRepo.unRepo
             , "--head", T.unpack spec.prcsHead
             , "--title", T.unpack spec.prcsTitle
             , "--body", T.unpack spec.prcsBody
             ] ++ baseArgs

  result <- runGhCommand config args
  case result of
    Left err -> do
      logDebug config $ "ghPrCreate: gh command failed: " <> T.unpack err
      -- For creation, we error out as we can't return a "Nothing" for PRUrl
      error $ "Failed to create PR: " <> T.unpack err
    Right output ->
      pure $ PRUrl $ T.strip output

-- | Get pull request review comments using gh CLI (via GraphQL for resolution status).
ghPrReviews :: GitHubConfig -> Text -> Int -> IO [ReviewComment]
ghPrReviews config repo num = do
  let (owner, repoName) = case T.splitOn "/" repo of
        [o, r] -> (o, r)
        _      -> (repo, "")

  let query =
        "query($owner: String!, $repo: String!, $number: Int!) { \
        \  repository(owner: $owner, name: $repo) { \
        \    pullRequest(number: $number) { \
        \      reviewThreads(last: 100) { \
        \        nodes { \
        \          isResolved \
        \          comments(first: 100) { \
        \            nodes { \
        \              author { login } \
        \              body \
        \              path \
        \              line \
        \              state \
        \              createdAt \
        \            } \
        \          } \
        \        } \
        \      } \
        \    } \
        \  } \
        \}"

  let args = [ "api", "graphql"
             , "-f", "owner=" <> T.unpack owner
             , "-f", "repo=" <> T.unpack repoName
             , "-F", "number=" <> show num
             , "-f", "query=" <> query
             ]

  result <- runGhCommand config args
  case result of
    Left err -> do
      logDebug config $ "ghPrReviews: gh command failed: " <> T.unpack err
      pure []
    Right output ->
      case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 output) of
        Right (GqlResult comments) -> pure comments
        Left decodeErr -> do
          logDebug config $ "ghPrReviews: JSON decode failed: " <> decodeErr
          pure []

-- | Helper type for parsing GraphQL response
newtype GqlResult = GqlResult { _unGqlResult :: [ReviewComment] }

instance FromJSON GqlResult where
  parseJSON = withObject "GqlResult" $ \v -> do
    -- data.repository.pullRequest.reviewThreads.nodes
    nodes <- v .: "data" 
         >>= (.: "repository") 
         >>= (.: "pullRequest") 
         >>= (.: "reviewThreads") 
         >>= (.: "nodes") :: Parser [Value]
    
    comments <- mapM parseThread nodes
    pure $ GqlResult (concat comments)
    where
      parseThread :: Value -> Parser [ReviewComment]
      parseThread = withObject "thread" $ \t -> do
        isResolved <- t .: "isResolved"
        commentsNodes <- t .: "comments" >>= (.: "nodes")
        mapM (parseComment isResolved) commentsNodes

      parseComment :: Bool -> Value -> Parser ReviewComment
      parseComment isResolved = withObject "comment" $ \c -> do
        authorObj <- c .:? "author"
        login <- case authorObj of
          Just a -> a .: "login"
          Nothing -> pure "unknown"
        
        body <- c .: "body"
        path <- c .: "path"
        line <- c .:? "line"
        stateStr <- c .: "state"
        createdAt <- c .: "createdAt"
        
        let state = case (stateStr :: Text) of
              "PENDING" -> ReviewPending
              _         -> ReviewCommented
              
        pure ReviewComment
          { rcAuthor = login
          , rcBody = body
          , rcPath = Just path
          , rcLine = line
          , rcState = state
          , rcCreatedAt = createdAt
          , rcIsResolved = isResolved
          }


-- ════════════════════════════════════════════════════════════════════════════
-- CLI FUNCTIONS - AUTH
-- ════════════════════════════════════════════════════════════════════════════

-- | Check if gh CLI is authenticated.
ghAuthCheck :: IO Bool
ghAuthCheck = do
  result <- try $ readProcessWithExitCode "gh" ["auth", "status"] ""
  case result of
    Left (_ :: SomeException) -> pure False
    Right (exitCode, _, _) ->
      case exitCode of
        ExitSuccess   -> pure True
        ExitFailure _ -> pure False


-- ════════════════════════════════════════════════════════════════════════════
-- HELPERS
-- ════════════════════════════════════════════════════════════════════════════

-- | Run a gh command and return stdout or error.
runGhCommand :: GitHubConfig -> [String] -> IO (Either Text Text)
runGhCommand config args = do
  result <- try $ readProcessWithExitCode "gh" args ""
  case result of
    Left (e :: SomeException) ->
      pure $ Left $ "gh command failed: " <> T.pack (show e)

    Right (exitCode, stdout, stderrOutput) ->
      case exitCode of
        ExitSuccess -> pure $ Right $ T.pack stdout
        ExitFailure code
          -- gh returns non-zero for "not found" which is OK (return empty)
          | "not found" `T.isInfixOf` T.toLower (T.pack stderrOutput) ->
              pure $ Right ""
          -- Auth errors get special messaging
          | "authentication" `T.isInfixOf` T.toLower (T.pack stderrOutput) ->
              pure $ Left $ "Not authenticated. Run: gh auth login"
          | otherwise ->
              pure $ Left $ "gh exited with code " <> T.pack (show code)
                          <> if config.ghcQuiet then "" else ": " <> T.pack stderrOutput


-- | Log debug message to stderr when not in quiet mode.
logDebug :: GitHubConfig -> String -> IO ()
logDebug config msg = unless config.ghcQuiet $ hPutStrLn stderr msg