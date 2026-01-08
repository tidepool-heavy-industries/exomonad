{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Session state management for Claude Code sessions.
--
-- Provides types and persistence for tracking session state across
-- Claude Code invocations, enabling session resumption on retries
-- and incremental work.
--
-- Session state is persisted per-worktree at:
-- @<worktree-path>/.claude/tidepool-session.json@
--
-- For example, if the worktree path is @\/home\/user\/project\/worktrees\/impl@,
-- the session state file will be at:
-- @\/home\/user\/project\/worktrees\/impl\/.claude\/tidepool-session.json@
module Tidepool.ClaudeCode.SessionState
  ( -- * Exit Reason
    SessionExitReason(..)
  , parseExitReason
  , exitReasonToText

    -- * Resume Strategy
  , ResumeStrategy(..)
  , shouldResume

    -- * Session State
  , SessionState(..)
  , loadSessionState
  , saveSessionState
  , clearSessionState

    -- * Session End Types
  , SessionEndContext(..)
  , SessionEndAction(..)
  ) where

import Control.Exception (catch, SomeException)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , withObject
  , withText
  , object
  , (.:)
  , (.:?)
  , (.=)
  , (.!=)
  )
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, removeFile, doesFileExist)
import System.FilePath ((</>))
import Data.ByteString.Lazy qualified as LBS
import Data.Aeson qualified as Aeson

import Tidepool.StructuredOutput (StructuredOutput)


-- ============================================================================
-- Exit Reason
-- ============================================================================

-- | Why a Claude Code session ended.
--
-- Maps to the @reason@ field from Claude Code's SessionEnd hook input.
data SessionExitReason
  = NormalExit
    -- ^ Normal completion ("exit")
  | ClearExit
    -- ^ User ran @/clear@ command
  | LogoutExit
    -- ^ User logged out
  | PromptInputExit
    -- ^ User exited while prompt input was visible
  | UnknownExit Text
    -- ^ Other/unexpected reason
  deriving stock (Show, Eq, Generic)

instance ToJSON SessionExitReason where
  toJSON NormalExit = "exit"
  toJSON ClearExit = "clear"
  toJSON LogoutExit = "logout"
  toJSON PromptInputExit = "prompt_input_exit"
  toJSON (UnknownExit t) = toJSON t

instance FromJSON SessionExitReason where
  parseJSON = withText "SessionExitReason" $ \t ->
    pure $ parseExitReason (Just t)

-- | Parse exit reason from optional text.
parseExitReason :: Maybe Text -> SessionExitReason
parseExitReason Nothing = UnknownExit "unknown"
parseExitReason (Just t) = case T.toLower t of
  "exit" -> NormalExit
  "clear" -> ClearExit
  "logout" -> LogoutExit
  "prompt_input_exit" -> PromptInputExit
  other -> UnknownExit other

-- | Convert exit reason back to text.
exitReasonToText :: SessionExitReason -> Text
exitReasonToText NormalExit = "exit"
exitReasonToText ClearExit = "clear"
exitReasonToText LogoutExit = "logout"
exitReasonToText PromptInputExit = "prompt_input_exit"
exitReasonToText (UnknownExit t) = t


-- ============================================================================
-- Resume Strategy
-- ============================================================================

-- | Strategy for resuming Claude Code sessions.
--
-- This configures when to pass @--resume <sessionId>@ to Claude Code.
data ResumeStrategy
  = AlwaysFresh
    -- ^ Always start a fresh session (current behavior)
  | ResumeOnRetry
    -- ^ Resume only on build failure retry
  | AlwaysResume
    -- ^ Always resume if session state exists
  | SmartResume
    -- ^ Resume based on previous exit reason
  deriving stock (Show, Eq, Generic)
  deriving anyclass (StructuredOutput)

instance ToJSON ResumeStrategy where
  toJSON AlwaysFresh = "always_fresh"
  toJSON ResumeOnRetry = "resume_on_retry"
  toJSON AlwaysResume = "always_resume"
  toJSON SmartResume = "smart_resume"

instance FromJSON ResumeStrategy where
  parseJSON = withText "ResumeStrategy" $ \t ->
    case T.toLower t of
      "always_fresh" -> pure AlwaysFresh
      "resume_on_retry" -> pure ResumeOnRetry
      "always_resume" -> pure AlwaysResume
      "smart_resume" -> pure SmartResume
      _ -> fail $ "Unknown resume strategy: " <> T.unpack t

-- | Determine if a session should be resumed based on exit reason.
--
-- Used by 'SmartResume' strategy:
--
-- * 'NormalExit' -> Yes (can continue incrementally)
-- * 'ClearExit' -> Yes (user paused, context cleared but session valid)
-- * 'LogoutExit' -> No (session invalidated)
-- * 'PromptInputExit' -> No (user abandoned)
-- * 'UnknownExit' -> Yes (be permissive)
shouldResume :: SessionExitReason -> Bool
shouldResume NormalExit = True
shouldResume ClearExit = True
shouldResume LogoutExit = False
shouldResume PromptInputExit = False
shouldResume (UnknownExit _) = True


-- ============================================================================
-- Session State
-- ============================================================================

-- | Persisted session state.
--
-- Stored at @<worktree>/.claude/tidepool-session.json@.
data SessionState = SessionState
  { ssSessionId :: Text
    -- ^ Claude Code session ID for resumption
  , ssLastExitReason :: Maybe SessionExitReason
    -- ^ Why the last session ended
  , ssTimestamp :: UTCTime
    -- ^ When state was saved
  , ssAgentName :: Text
    -- ^ Agent name (e.g., "tests", "impl", "stubs")
  , ssRetryCount :: Int
    -- ^ Number of retries in current workflow
  , ssTranscriptPath :: Maybe Text
    -- ^ Path to session transcript
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON SessionState where
  toJSON SessionState{..} = object
    [ "session_id" .= ssSessionId
    , "last_exit_reason" .= ssLastExitReason
    , "timestamp" .= ssTimestamp
    , "agent_name" .= ssAgentName
    , "retry_count" .= ssRetryCount
    , "transcript_path" .= ssTranscriptPath
    ]

instance FromJSON SessionState where
  parseJSON = withObject "SessionState" $ \o -> SessionState
    <$> o .: "session_id"
    <*> o .:? "last_exit_reason"
    <*> o .: "timestamp"
    <*> o .: "agent_name"
    <*> o .:? "retry_count" .!= 0
    <*> o .:? "transcript_path"

-- | Session state file path within a worktree.
sessionStateFile :: FilePath -> FilePath
sessionStateFile worktreePath = worktreePath </> ".claude" </> "tidepool-session.json"

-- | Load session state from disk.
--
-- Returns 'Nothing' if file doesn't exist or can't be parsed.
loadSessionState :: FilePath -> IO (Maybe SessionState)
loadSessionState worktreePath = do
  let path = sessionStateFile worktreePath
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      contents <- LBS.readFile path `catch` \(_ :: SomeException) -> pure ""
      case Aeson.decode contents of
        Nothing -> pure Nothing
        Just state -> pure (Just state)

-- | Save session state to disk.
--
-- Creates @.claude/@ directory if needed.
saveSessionState :: FilePath -> SessionState -> IO ()
saveSessionState worktreePath state = do
  let dir = worktreePath </> ".claude"
      path = sessionStateFile worktreePath
  createDirectoryIfMissing True dir
  LBS.writeFile path (Aeson.encode state)

-- | Clear session state (delete the file).
clearSessionState :: FilePath -> IO ()
clearSessionState worktreePath = do
  let path = sessionStateFile worktreePath
  removeFile path `catch` \(_ :: SomeException) -> pure ()


-- ============================================================================
-- Session End Types (for hook callbacks)
-- ============================================================================

-- | Rich context for SessionEnd hook.
--
-- Contains all information available when a session ends.
data SessionEndContext = SessionEndContext
  { secSessionId :: Text
    -- ^ Session ID from Claude Code
  , secTranscriptPath :: Text
    -- ^ Path to conversation transcript
  , secCwd :: Text
    -- ^ Working directory when hook fired
  , secReason :: SessionExitReason
    -- ^ Why the session ended
  }
  deriving stock (Show, Eq, Generic)

-- | Action to take when a session ends.
--
-- Returned by the typed SessionEnd callback.
data SessionEndAction
  = CommitWork Text
    -- ^ Auto-commit with the given message
  | PreserveForResume
    -- ^ Save state for later session resume
  | Cleanup
    -- ^ Clean up session artifacts
  | NoAction
    -- ^ Default behavior (no special action)
  deriving stock (Show, Eq, Generic)
