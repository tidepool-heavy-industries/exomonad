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
  , ghAuthCheck
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (unless)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
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
  -- Legacy (stub - still errors)
  CreateIssue (Repo repo) title _ _ -> do
    sendM $ error $ "GitHub.createIssue: not implemented for " <> T.unpack repo <> " - " <> T.unpack title

  -- Issue operations
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

  -- Auth
  CheckAuth ->
    sendM $ ghAuthCheck


-- ════════════════════════════════════════════════════════════════════════════
-- CLI FUNCTIONS - ISSUES
-- ════════════════════════════════════════════════════════════════════════════

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
  let baseFields = "number,title,body,author,labels,state,url,headRefName,baseRefName"
      args = ["pr", "list", "--repo", T.unpack repo, "--json", baseFields]
            ++ stateArgs filt.pfState
            ++ baseArgs filt.pfBase
            ++ limitArgs filt.pfLimit

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


-- | View a single pull request using gh CLI.
ghPrView :: GitHubConfig -> Text -> Int -> Bool -> IO (Maybe PullRequest)
ghPrView config repo num includeDetails = do
  let baseFields = "number,title,body,author,labels,state,url,headRefName,baseRefName"
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
