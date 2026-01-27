-- | GitHub effect interpreter - gh CLI client.
--
-- Implements GitHub effect by calling the gh CLI tool.
-- Supports read operations for issues and pull requests.
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
--
-- = Requirements
--
-- Requires the @gh@ CLI tool to be installed and authenticated:
--
-- @
-- gh auth login
-- @
module ExoMonad.GitHub.Interpreter
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
  , GqlResult(..)
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (unless)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson (eitherDecode, FromJSON(..), withObject, (.:), (.:?), Value(..))
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Timeout (timeout)
import Data.Time.Clock (getCurrentTime, addUTCTime)

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
ghIssueCreate :: GitHubConfig -> CreateIssueInput -> IO (Either GitHubError Int)
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
    Left err -> pure $ Left err
    Right output ->
      -- gh issue create returns the URL of the created issue, e.g.
      -- https://github.com/owner/repo/issues/123
      let parts = T.splitOn "/" (T.strip output)
      in case reverse parts of
        (numStr : _) -> case reads (T.unpack numStr) :: [(Int, String)] of
          [(n, "")] -> pure $ Right n
          _ -> pure $ Left $ GHParseError $ "Failed to parse issue number from: " <> output
        _ -> pure $ Left $ GHParseError $ "Unexpected output format: " <> output


-- | Edit an issue using gh CLI.
ghIssueEdit :: GitHubConfig -> Text -> Int -> UpdateIssueInput -> IO (Either GitHubError ())
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
    then pure $ Right ()
    else do
      result <- runGhCommand config args
      case result of
        Left err -> pure $ Left err
        Right _ -> pure $ Right ()


-- | Close an issue using gh CLI.
ghIssueClose :: GitHubConfig -> Text -> Int -> IO (Either GitHubError ())
ghIssueClose config repo num = do
  let args = ["issue", "close", show num, "--repo", T.unpack repo]
  result <- runGhCommand config args
  case result of
    Left err -> pure $ Left err
    Right _ -> pure $ Right ()


-- | Reopen an issue using gh CLI.
ghIssueReopen :: GitHubConfig -> Text -> Int -> IO (Either GitHubError ())
ghIssueReopen config repo num = do
  let args = ["issue", "reopen", show num, "--repo", T.unpack repo]
  result <- runGhCommand config args
  case result of
    Left err -> pure $ Left err
    Right _ -> pure $ Right ()


-- | Add a label to an issue.
ghIssueLabelAdd :: GitHubConfig -> Text -> Int -> Text -> IO (Either GitHubError ())
ghIssueLabelAdd config repo num label = do
  let args = ["issue", "edit", show num, "--repo", T.unpack repo, "--add-label", T.unpack label]
  result <- runGhCommand config args
  case result of
    Left err -> pure $ Left err
    Right _ -> pure $ Right ()


-- | Remove a label from an issue.
ghIssueLabelRemove :: GitHubConfig -> Text -> Int -> Text -> IO (Either GitHubError ())
ghIssueLabelRemove config repo num label = do
  let args = ["issue", "edit", show num, "--repo", T.unpack repo, "--remove-label", T.unpack label]
  result <- runGhCommand config args
  case result of
    Left err -> pure $ Left err
    Right _ -> pure $ Right ()


-- | Add an assignee to an issue.
ghIssueAssigneeAdd :: GitHubConfig -> Text -> Int -> Text -> IO (Either GitHubError ())
ghIssueAssigneeAdd config repo num assignee = do
  let args = ["issue", "edit", show num, "--repo", T.unpack repo, "--add-assignee", T.unpack assignee]
  result <- runGhCommand config args
  case result of
    Left err -> pure $ Left err
    Right _ -> pure $ Right ()


-- | Remove an assignee from an issue.
ghIssueAssigneeRemove :: GitHubConfig -> Text -> Int -> Text -> IO (Either GitHubError ())
ghIssueAssigneeRemove config repo num assignee = do
  let args = ["issue", "edit", show num, "--repo", T.unpack repo, "--remove-assignee", T.unpack assignee]
  result <- runGhCommand config args
  case result of
    Left err -> pure $ Left err
    Right _ -> pure $ Right ()


-- | List issues using gh CLI.
--
-- Returns Either GitHubError [Issue]:
-- - Left GitHubError: Command failed (auth, network, etc.)
-- - Right []: Successfully queried, no issues match filter
ghIssueList :: GitHubConfig -> Text -> IssueFilter -> IO (Either GitHubError [Issue])
ghIssueList config repo filt = do
  let baseFields = "number,title,body,author,labels,state,url"
      args = ["issue", "list", "--repo", T.unpack repo, "--json", baseFields]
            ++ stateArgs filt.ifState
            ++ labelArgs filt.ifLabels
            ++ limitArgs filt.ifLimit

  result <- runGhCommand config args
  case result of
    Left err -> pure $ Left err
    Right output ->
      case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 output) of
        Right issues -> pure $ Right issues
        Left decodeErr -> do
          hPutStrLn stderr $ "[GitHub] ERROR: JSON decode failed: " <> decodeErr
          pure $ Left $ GHParseError $ T.pack decodeErr

  where
    stateArgs Nothing           = []
    stateArgs (Just IssueOpen)   = ["--state", "open"]
    stateArgs (Just IssueClosed) = ["--state", "closed"]

    labelArgs [] = []
    labelArgs ls = concatMap (\l -> ["--label", T.unpack l]) ls

    limitArgs Nothing  = []
    limitArgs (Just n) = ["--limit", show n]


-- | View a single issue using gh CLI.
--
-- Returns Either GitHubError (Maybe Issue):
-- - Left GitHubError: Command failed (auth, network, etc.)
-- - Right Nothing: Issue not found (valid query, but issue doesn't exist)
-- - Right (Just issue): Issue found successfully
ghIssueView :: GitHubConfig -> Text -> Int -> Bool -> IO (Either GitHubError (Maybe Issue))
ghIssueView config repo num includeComments = do
  let baseFields = "number,title,body,author,labels,state,url"
      fields = if includeComments then baseFields <> ",comments" else baseFields
      args = ["issue", "view", show num, "--repo", T.unpack repo, "--json", fields]

  result <- runGhCommand config args
  case result of
    Left (GHUnexpected 404 _) -> pure $ Left $ GHNotFound num
    Left err -> pure $ Left err
    Right output
      | T.null (T.strip output) -> pure $ Right Nothing
      | otherwise ->
          case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 output) of
            Right issue -> pure $ Right $ Just issue
            Left decodeErr -> do
              hPutStrLn stderr $ "[GitHub] ERROR: JSON decode failed: " <> decodeErr
              pure $ Left $ GHParseError $ T.pack decodeErr


-- ════════════════════════════════════════════════════════════════════════════
-- CLI FUNCTIONS - PULL REQUESTS
-- ════════════════════════════════════════════════════════════════════════════

-- | List pull requests using gh CLI.
--
-- Returns Either GitHubError [PullRequest]:
-- - Left GitHubError: Command failed (auth, network, etc.)
-- - Right []: Successfully queried, no PRs match filter
ghPrList :: GitHubConfig -> Text -> PRFilter -> IO (Either GitHubError [PullRequest])
ghPrList config repo filt = do
  let baseFields = "number,title,body,author,labels,state,url,headRefName,baseRefName,createdAt,mergedAt"
      args = ["pr", "list", "--repo", T.unpack repo, "--json", baseFields]
            ++ stateArgs filt.pfState
            ++ baseArgs filt.pfBase
            ++ limitArgs filt.pfLimit
            ++ searchArgs filt.pfSearch

  result <- runGhCommand config args
  case result of
    Left err -> pure $ Left err
    Right output ->
      case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 output) of
        Right prs -> pure $ Right prs
        Left decodeErr -> do
          hPutStrLn stderr $ "[GitHub] ERROR: JSON decode failed: " <> decodeErr
          pure $ Left $ GHParseError $ T.pack decodeErr

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
--
-- Returns Either GitHubError (Maybe PullRequest):
-- - Left GitHubError: Command failed (auth, network, etc.)
-- - Right Nothing: PR not found (valid query, but PR doesn't exist)
-- - Right (Just pr): PR found successfully
ghPrView :: GitHubConfig -> Text -> Int -> Bool -> IO (Either GitHubError (Maybe PullRequest))
ghPrView config repo num includeDetails = do
  let baseFields = "number,title,body,author,labels,state,url,headRefName,baseRefName,createdAt,mergedAt"
      fields = if includeDetails
               then baseFields <> ",comments,reviews"
               else baseFields
      args = ["pr", "view", show num, "--repo", T.unpack repo, "--json", fields]

  result <- runGhCommand config args
  case result of
    Left (GHUnexpected 404 _) -> pure $ Left $ GHNotFound num
    Left err -> pure $ Left err
    Right output
      | T.null (T.strip output) -> pure $ Right Nothing
      | otherwise ->
          case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 output) of
            Right pr -> pure $ Right $ Just pr
            Left decodeErr -> do
              hPutStrLn stderr $ "[GitHub] ERROR: JSON decode failed: " <> decodeErr
              pure $ Left $ GHParseError $ T.pack decodeErr


-- | Create a pull request using gh CLI.
ghPrCreate :: GitHubConfig -> PRCreateSpec -> IO (Either GitHubError PRUrl)
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
    Left err -> pure $ Left err
    Right output -> pure $ Right $ PRUrl $ T.strip output

-- | Get pull request review comments using gh CLI (via GraphQL for resolution status).
--
-- Returns Either GitHubError [ReviewComment]:
-- - Left GitHubError: Command failed (auth, network, etc.)
-- - Right []: Successfully queried, no review comments
ghPrReviews :: GitHubConfig -> Text -> Int -> IO (Either GitHubError [ReviewComment])
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
    Left err -> pure $ Left err
    Right output ->
      case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 output) of
        Right (GqlResult comments) -> pure $ Right comments
        Left decodeErr -> do
          hPutStrLn stderr $ "[GitHub] ERROR: JSON decode failed: " <> decodeErr
          pure $ Left $ GHParseError $ T.pack decodeErr

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
          Just (Object a) -> a .: "login"
          _               -> pure "unknown"
        
        body <- c .: "body"
        path <- c .: "path"
        line <- c .:? "line"
        stateStr <- c .: "state"
        createdAt <- c .: "createdAt"
        
        let state = case (stateStr :: Text) of
              "PENDING"   -> ReviewPending
              "SUBMITTED" -> ReviewCommented
              _           -> ReviewCommented
              
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

-- | Run a gh command and return stdout or structured error.
--
-- IMPORTANT: This function returns structured GitHubError for failures,
-- making error conditions explicit in the type system.
runGhCommand :: GitHubConfig -> [String] -> IO (Either GitHubError Text)
runGhCommand _config args = do
  -- Log command before execution (aggressive logging per CLAUDE.md)
  hPutStrLn stderr $ "[GitHub] Executing: gh " <> unwords args

  -- 10 second timeout for all GitHub CLI calls.
  --
  -- Rationale:
  --   * This interpreter is only used for relatively small, read-only GitHub
  --     operations (e.g. fetching a single issue/PR, small listings), where
  --     normal end-to-end latency is expected to be well under a few seconds.
  --   * We prefer failing fast with GHTimeout over letting an agent turn hang
  --     indefinitely if the gh CLI stalls or the network misbehaves.
  --   * If you need to support genuinely long-running gh operations (such as
  --     listing thousands of issues), you should introduce a dedicated
  --     interpreter/helper with a higher or configurable timeout, rather than
  --     routing those operations through this helper.
  --
  -- Given those constraints, 10 seconds is chosen as a conservative upper bound
  -- that is high enough for expected use cases but low enough to detect hung
  -- or misbehaving gh processes.
  let timeoutUs = 10 * 1000000 
  mResult <- timeout timeoutUs $ try $ readProcessWithExitCode "gh" args ""
  
  case mResult of
    Nothing -> do
      hPutStrLn stderr "[GitHub] ERROR: Request timed out after 10s"
      pure $ Left GHTimeout

    Just (Left (e :: SomeException)) -> do
      let errMsg = "gh command exception: " <> T.pack (show e)
      hPutStrLn stderr $ "[GitHub] ERROR: " <> T.unpack errMsg
      pure $ Left $ GHNetworkError errMsg

    Just (Right (exitCode, stdout, stderrOutput)) -> do
      -- Log exit code (aggressive logging per CLAUDE.md)
      hPutStrLn stderr $ "[GitHub] Exit code: " <> show exitCode
      unless (null stderrOutput) $
        hPutStrLn stderr $ "[GitHub] stderr: " <> take 500 stderrOutput

      case exitCode of
        ExitSuccess -> pure $ Right $ T.pack stdout
        ExitFailure code -> do
          let stderrText = T.pack stderrOutput
              stderrLower = T.toLower stderrText

          -- Parse specific error conditions into typed errors
          if "authentication" `T.isInfixOf` stderrLower ||
             "not logged in" `T.isInfixOf` stderrLower ||
             "gh auth login" `T.isInfixOf` stderrLower
          then do
            hPutStrLn stderr "[GitHub] ERROR: Not authenticated"
            pure $ Left $ GHPermissionDenied "Not authenticated. Run: gh auth login"

          -- Rate limit detection
          else if "rate limit" `T.isInfixOf` stderrLower
          then do
            hPutStrLn stderr "[GitHub] ERROR: Rate limit exceeded"
            now <- getCurrentTime
            -- GitHub's HTTP API exposes an X-RateLimit-Reset header, but when using the
            -- `gh` CLI we only see stderr, which does not reliably include that value.
            -- We therefore use a conservative 1-hour reset window here so that callers'
            -- retry logic waits long enough to avoid immediately hitting the limit again.
            pure $ Left $ GHRateLimit (addUTCTime 3600 now)

          -- "not found" for specific resources
          else if "not found" `T.isInfixOf` stderrLower ||
                  "could not find" `T.isInfixOf` stderrLower
          then do
            hPutStrLn stderr $ "[GitHub] ERROR: Resource not found: " <> T.unpack stderrText
            -- Try to parse a numeric resource ID from stderr to return GHNotFound consistently.
            let mResourceId :: Maybe Int
                mResourceId =
                  let s = T.unpack stderrText
                      dropNonDigits = dropWhile (\c -> c < '0' || c > '9') s
                  in case dropNonDigits of
                       [] -> Nothing
                       cs ->
                         let (digits, _) = span (\c -> c >= '0' && c <= '9') cs
                         in if null digits then Nothing else Just (read digits)
            case mResourceId of
              Just rid -> pure $ Left $ GHNotFound rid
              Nothing  -> pure $ Left $ GHUnexpected 404 stderrText

          -- Generic command failure with exit code and stderr
          else do
            hPutStrLn stderr $ "[GitHub] ERROR: Command failed with code " <> show code
            pure $ Left $ GHUnexpected code stderrText
            