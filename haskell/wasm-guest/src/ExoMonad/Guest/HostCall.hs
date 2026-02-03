{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module ExoMonad.Guest.HostCall
  ( callHost,
    callHostVoid,
    FFIResult (..),
    FFIError (..),
    ErrorCode (..),
    ErrorContext (..),
    -- Git
    host_git_get_branch,
    host_git_get_worktree,
    host_git_get_dirty_files,
    host_git_get_recent_commits,
    host_git_has_unpushed_commits,
    host_git_get_remote_url,
    host_git_get_repo_info,
    -- GitHub
    host_github_list_issues,
    host_github_get_issue,
    host_github_create_pr,
    host_github_list_prs,
    host_github_get_pr_for_branch,
    host_github_get_pr_review_comments,
    -- Log
    host_log_info,
    host_log_error,
    host_emit_event,
    LogLevel (..),
    LogPayload (..),
    -- Agent Control
    host_agent_spawn,
    host_agent_spawn_batch,
    host_agent_cleanup,
    host_agent_cleanup_batch,
    host_agent_cleanup_merged,
    host_agent_list,
    -- Filesystem
    host_fs_read_file,
    host_fs_write_file,
    -- File PR
    host_file_pr,
    -- Copilot Review
    host_wait_for_copilot_review,
    -- UI
    host_ui_show_popup,
  )
where

import Control.Exception (bracket)
import Data.Aeson (FromJSON, ToJSON (..), Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import ExoMonad.Guest.FFI
import Extism.PDK.Memory (alloc, findMemory, free, load, memoryOffset)
import GHC.Generics (Generic)

-- ============================================================================ 
-- Log Types (matches Rust LogPayload/LogLevel)
-- ============================================================================ 

data LogLevel = Debug | Info | Warn | Error
  deriving (Show, Eq, Generic)

instance ToJSON LogLevel where
  toJSON Debug = Aeson.String "debug"
  toJSON Info = Aeson.String "info"
  toJSON Warn = Aeson.String "warn"
  toJSON Error = Aeson.String "error"

instance FromJSON LogLevel where
  parseJSON = Aeson.withText "LogLevel" $ \case
    "debug" -> pure Debug
    "info" -> pure Info
    "warn" -> pure Warn
    "error" -> pure Error
    other -> fail $ "Unknown LogLevel: " <> T.unpack other

data LogPayload = LogPayload
  { level :: LogLevel,
    message :: Text,
    fields :: Maybe [(Text, Text)]
  }
  deriving (Show, Generic)

instance ToJSON LogPayload
instance FromJSON LogPayload

instance FFIBoundary LogPayload

-- ============================================================================ 
-- Git host functions
-- ============================================================================ 
foreign import ccall "git_get_branch" host_git_get_branch :: Word64 -> IO Word64

foreign import ccall "git_get_worktree" host_git_get_worktree :: Word64 -> IO Word64

foreign import ccall "git_get_dirty_files" host_git_get_dirty_files :: Word64 -> IO Word64

foreign import ccall "git_get_recent_commits" host_git_get_recent_commits :: Word64 -> IO Word64

foreign import ccall "git_has_unpushed_commits" host_git_has_unpushed_commits :: Word64 -> IO Word64

foreign import ccall "git_get_remote_url" host_git_get_remote_url :: Word64 -> IO Word64

foreign import ccall "git_get_repo_info" host_git_get_repo_info :: Word64 -> IO Word64

-- GitHub host functions
foreign import ccall "github_list_issues" host_github_list_issues :: Word64 -> IO Word64

foreign import ccall "github_get_issue" host_github_get_issue :: Word64 -> IO Word64

foreign import ccall "github_create_pr" host_github_create_pr :: Word64 -> IO Word64

foreign import ccall "github_list_prs" host_github_list_prs :: Word64 -> IO Word64

foreign import ccall "github_get_pr_for_branch" host_github_get_pr_for_branch :: Word64 -> IO Word64

foreign import ccall "github_get_pr_review_comments" host_github_get_pr_review_comments :: Word64 -> IO Word64

-- Log host functions (void return - fire and forget)
foreign import ccall "log_info" host_log_info :: Word64 -> IO ()

foreign import ccall "log_error" host_log_error :: Word64 -> IO ()

foreign import ccall "emit_event" host_emit_event :: Word64 -> IO ()

-- Agent Control host functions (high-level semantic operations)
foreign import ccall "agent_spawn" host_agent_spawn :: Word64 -> IO Word64

foreign import ccall "agent_spawn_batch" host_agent_spawn_batch :: Word64 -> IO Word64

foreign import ccall "agent_cleanup" host_agent_cleanup :: Word64 -> IO Word64

foreign import ccall "agent_cleanup_batch" host_agent_cleanup_batch :: Word64 -> IO Word64

foreign import ccall "agent_cleanup_merged" host_agent_cleanup_merged :: Word64 -> IO Word64

foreign import ccall "agent_list" host_agent_list :: Word64 -> IO Word64

-- Filesystem host functions
foreign import ccall "fs_read_file" host_fs_read_file :: Word64 -> IO Word64

foreign import ccall "fs_write_file" host_fs_write_file :: Word64 -> IO Word64

-- File PR host function (create/update PRs via gh CLI)
foreign import ccall "file_pr" host_file_pr :: Word64 -> IO Word64

-- Copilot Review host function (poll for Copilot review comments)
foreign import ccall "wait_for_copilot_review" host_wait_for_copilot_review :: Word64 -> IO Word64

-- UI host function
foreign import ccall "ui_show_popup" host_ui_show_popup :: Word64 -> IO Word64

-- | Call a host function with automatic JSON serialization/deserialization.
-- Request uses FFIBoundary, response uses FFIBoundary to handle FFIResult wrapper.
-- Returns Either Text for errors (no String conversion needed at call sites).
callHost :: (FFIBoundary req, FFIBoundary resp) => (Word64 -> IO Word64) -> req -> IO (Either Text resp)
callHost rawFn request = do
  -- Allocate request with automatic JSON encoding via Aeson (via FFIBoundary)
  reqMem <- alloc (toStrict (toFFI request))

  bracket (pure reqMem) free $ \_ -> do
    -- Call host function
    respOffset <- rawFn (memoryOffset reqMem)

    -- Read response (ensure we free the response memory)
    respMem <- findMemory respOffset
    bracket (pure respMem) free $ \_ -> do
      -- Load raw bytes
      respBytes <- (load respMem :: IO (Either String ByteString))

      case respBytes of
        Left loadErr -> pure $ Left ("Load error: " <> T.pack loadErr)
        Right bytes -> 
            -- Decode with FFIBoundary
            case fromFFI (fromStrict bytes) of
              Left err -> pure $ Left (formatFFIError err)
              Right val -> pure $ Right val

formatFFIError :: FFIError -> Text
formatFFIError err =
  "[" <> T.pack (show (feCode err)) <> "] " <> feMessage err
    <> maybe "" (\s -> "\nSuggestion: " <> s) (feSuggestion err)
    <> maybe "" formatContext (feContext err)

formatContext :: ErrorContext -> Text
formatContext ctx = 
  "\nContext:"
    <> maybe "" (\c -> "\n  Command: " <> c) (errorContextCommand ctx)
    <> maybe "" (\e -> "\n  Exit Code: " <> T.pack (show e)) (errorContextExitCode ctx)
    <> maybe "" (\s -> "\n  Stderr: " <> s) (errorContextStderr ctx)
    <> maybe "" (\s -> "\n  Stdout: " <> s) (errorContextStdout ctx)
    <> maybe "" (\p -> "\n  File: " <> p) (errorContextFilePath ctx)
    <> maybe "" (\w -> "\n  Working Dir: " <> w) (errorContextWorkingDir ctx)

-- | Call a host function that returns void (fire-and-forget, e.g., logging)
callHostVoid :: (FFIBoundary req) => (Word64 -> IO ()) -> req -> IO ()
callHostVoid rawFn request = do
  -- Allocate request with automatic JSON encoding
  reqMem <- alloc (toStrict (toFFI request))
  bracket (pure reqMem) free $ \_ -> do
    rawFn (memoryOffset reqMem)