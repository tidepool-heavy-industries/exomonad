{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Crosstalk support for parallel Claude Code agents.
--
-- Enables mid-session context injection between agents working in parallel
-- worktrees. When one agent commits changes, the other agent receives
-- context about those changes via PostToolUse hooks.
--
-- == Usage
--
-- @
-- -- Create crosstalk state pair for parallel agents
-- (testsCrosstalk, implCrosstalk) <- newCrosstalkStatePair
--     testsWorktree "tests"
--     implWorktree "impl"
--
-- -- Use in callbacks (for the impl agent)
-- let callbacks = defaultHookCallbacks
--       { hcOnPostToolUseTyped = \\ctx ->
--           crosstalkPostToolUse implCrosstalk
--             ctx.ptcToolName ctx.ptcToolInput ctx.ptcToolResponse
--       }
--
-- -- Run agent with crosstalk-enabled callbacks
-- runClaudeCodeRequestWithHooks config callbacks ...
-- @
--
-- The crosstalk callback will inject context like:
--
-- @
-- 🔄 CROSSTALK: The tests agent has made progress!
--
-- New commits:
-- - abc123: Add property tests for push/pop
--
-- Changed files:
-- - test/Main.hs
--
-- Consider how these changes affect your implementation.
-- @
module Tidepool.ClaudeCode.Crosstalk
  ( -- * Crosstalk State
    CrosstalkState(..)
  , newCrosstalkState
  , newCrosstalkStatePair

    -- * Commit Tracking
  , CommitInfo(..)
  , checkForNewCommits
  , formatCrosstalkContext

    -- * Hook Callback
  , crosstalkPostToolUse
  , PostToolUseContext(..)
  , PostToolUseAction(..)
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, writeTVar)
import Control.Exception (try, SomeException)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Aeson (Value)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))


-- ============================================================================
-- Crosstalk State
-- ============================================================================

-- | State for tracking another agent's progress.
data CrosstalkState = CrosstalkState
  { csOtherWorktree :: FilePath
    -- ^ Path to the other agent's worktree
  , csOtherAgentName :: Text
    -- ^ Name of the other agent (e.g., "tests", "impl")
  , csLastSeenCommit :: TVar (Maybe Text)
    -- ^ Last commit hash we've seen from the other agent
  , csEnabled :: TVar Bool
    -- ^ Whether crosstalk is enabled (can be toggled)
  }

-- | Create a new crosstalk state for tracking another agent.
newCrosstalkState
  :: FilePath  -- ^ Other agent's worktree path
  -> Text      -- ^ Other agent's name
  -> IO CrosstalkState
newCrosstalkState otherWorktree otherName = do
  -- Get current HEAD of other worktree as starting point
  initialCommit <- getHeadCommit otherWorktree
  lastSeen <- newTVarIO initialCommit
  enabled <- newTVarIO True
  pure CrosstalkState
    { csOtherWorktree = otherWorktree
    , csOtherAgentName = otherName
    , csLastSeenCommit = lastSeen
    , csEnabled = enabled
    }

-- | Create a pair of crosstalk states for two parallel agents.
--
-- Returns (stateForAgent1, stateForAgent2) where each tracks the other.
newCrosstalkStatePair
  :: FilePath  -- ^ Agent 1's worktree
  -> Text      -- ^ Agent 1's name
  -> FilePath  -- ^ Agent 2's worktree
  -> Text      -- ^ Agent 2's name
  -> IO (CrosstalkState, CrosstalkState)
newCrosstalkStatePair wt1 name1 wt2 name2 = do
  -- Agent 1 tracks Agent 2's worktree
  state1 <- newCrosstalkState wt2 name2
  -- Agent 2 tracks Agent 1's worktree
  state2 <- newCrosstalkState wt1 name1
  pure (state1, state2)


-- ============================================================================
-- Commit Tracking
-- ============================================================================

-- | Information about a commit.
data CommitInfo = CommitInfo
  { ciHash :: Text
    -- ^ Short commit hash
  , ciMessage :: Text
    -- ^ Commit message (first line)
  , ciFiles :: [Text]
    -- ^ Changed files
  }
  deriving stock (Show, Eq)

-- | Get the HEAD commit hash of a worktree.
getHeadCommit :: FilePath -> IO (Maybe Text)
getHeadCommit worktree = do
  result <- try $ readProcessWithExitCode
    "git"
    ["-C", worktree, "rev-parse", "--short", "HEAD"]
    ""
  case result of
    Left (_ :: SomeException) -> pure Nothing
    Right (ExitSuccess, stdout, _) -> pure $ Just $ T.strip $ T.pack stdout
    Right (ExitFailure _, _, _) -> pure Nothing

-- | Check for new commits since we last looked.
--
-- Returns list of new commits (newest first) and updates the last-seen marker.
checkForNewCommits :: CrosstalkState -> IO [CommitInfo]
checkForNewCommits state = do
  enabled <- readTVarIO (csEnabled state)
  if not enabled
    then pure []
    else do
      lastSeen <- readTVarIO (csLastSeenCommit state)
      currentHead <- getHeadCommit (csOtherWorktree state)

      case (lastSeen, currentHead) of
        (Just lastCommit, Just current) | lastCommit /= current -> do
          -- There are new commits, fetch them
          commits <- getCommitsBetween (csOtherWorktree state) lastCommit current
          -- Update last seen
          atomically $ writeTVar (csLastSeenCommit state) currentHead
          pure commits

        (Nothing, Just current) -> do
          -- First time seeing commits, just update marker
          atomically $ writeTVar (csLastSeenCommit state) (Just current)
          pure []

        _ -> pure []

-- | Get commits between two hashes.
getCommitsBetween :: FilePath -> Text -> Text -> IO [CommitInfo]
getCommitsBetween worktree fromHash toHash = do
  -- Get commit hashes and messages
  let range = T.unpack fromHash <> ".." <> T.unpack toHash
  result <- try $ readProcessWithExitCode
    "git"
    ["-C", worktree, "log", "--oneline", "--no-decorate", range]
    ""

  case result of
    Left (_ :: SomeException) -> pure []
    Right (ExitFailure _, _, _) -> pure []
    Right (ExitSuccess, stdout, _) -> do
      let lines_ = filter (not . T.null) $ T.lines $ T.pack stdout
      mapM (parseCommitLine worktree) lines_

-- | Parse a git log --oneline line into CommitInfo.
parseCommitLine :: FilePath -> Text -> IO CommitInfo
parseCommitLine worktree line = do
  let (hash, rest) = T.breakOn " " line
      message = T.strip $ T.drop 1 rest
  files <- getChangedFiles worktree hash
  pure CommitInfo
    { ciHash = hash
    , ciMessage = message
    , ciFiles = files
    }

-- | Get files changed in a commit.
getChangedFiles :: FilePath -> Text -> IO [Text]
getChangedFiles worktree hash = do
  result <- try $ readProcessWithExitCode
    "git"
    ["-C", worktree, "diff-tree", "--no-commit-id", "--name-only", "-r", T.unpack hash]
    ""
  case result of
    Left (_ :: SomeException) -> pure []
    Right (ExitFailure _, _, _) -> pure []
    Right (ExitSuccess, stdout, _) ->
      pure $ filter (not . T.null) $ T.lines $ T.pack stdout


-- ============================================================================
-- Context Formatting
-- ============================================================================

-- | Format crosstalk context for injection.
formatCrosstalkContext :: Text -> [CommitInfo] -> Text
formatCrosstalkContext agentName commits = T.unlines
  [ "🔄 CROSSTALK: The " <> agentName <> " agent has made progress!"
  , ""
  , "New commits:"
  , T.unlines $ map formatCommit commits
  , "Changed files:"
  , T.unlines $ map ("  - " <>) $ concatMap ciFiles commits
  , "Consider how these changes affect your work."
  ]
  where
    formatCommit ci = "  - " <> ciHash ci <> ": " <> ciMessage ci


-- ============================================================================
-- PostToolUse Hook
-- ============================================================================

-- | Context provided to PostToolUse callbacks.
data PostToolUseContext = PostToolUseContext
  { ptcToolName :: Text
  , ptcToolInput :: Value
  , ptcToolResponse :: Value
  }
  deriving stock (Show, Eq)

-- | Action to take after tool use.
data PostToolUseAction
  = PostToolAllow
    -- ^ Allow with no additional context
  | PostToolAddContext Text
    -- ^ Add context that Claude will see
  deriving stock (Show, Eq)

-- | PostToolUse callback that checks for crosstalk.
--
-- After Write or Bash tool calls (which might indicate the other agent
-- committed), checks for new commits and injects context if found.
crosstalkPostToolUse
  :: CrosstalkState
  -> Text   -- ^ Tool name
  -> Value  -- ^ Tool input
  -> Value  -- ^ Tool response
  -> IO PostToolUseAction
crosstalkPostToolUse state toolName _input _response = do
  -- Only check after tools that might indicate progress
  if toolName `elem` ["Write", "Bash", "Edit"]
    then do
      newCommits <- checkForNewCommits state
      case newCommits of
        [] -> pure PostToolAllow
        commits -> do
          let context = formatCrosstalkContext (csOtherAgentName state) commits
          pure $ PostToolAddContext context
    else pure PostToolAllow
