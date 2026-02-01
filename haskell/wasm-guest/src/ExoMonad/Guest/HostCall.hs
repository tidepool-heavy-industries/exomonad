{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExoMonad.Guest.HostCall
  ( callHost,
    callHostVoid,
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
    -- Agent Control
    host_agent_spawn,
    host_agent_spawn_batch,
    host_agent_cleanup,
    host_agent_cleanup_batch,
    host_agent_list,
    -- Filesystem
    host_fs_read_file,
    host_fs_write_file,
    -- File PR
    host_file_pr,
    -- Copilot Review
    host_wait_for_copilot_review,
  )
where

import Control.Exception (bracket)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Word (Word64)
import Extism.PDK.Memory (Memory, alloc, findMemory, free, load, memoryOffset)

-- Git host functions
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

foreign import ccall "agent_list" host_agent_list :: Word64 -> IO Word64

-- Filesystem host functions
foreign import ccall "fs_read_file" host_fs_read_file :: Word64 -> IO Word64

foreign import ccall "fs_write_file" host_fs_write_file :: Word64 -> IO Word64

-- File PR host function (create/update PRs via gh CLI)
foreign import ccall "file_pr" host_file_pr :: Word64 -> IO Word64

-- Copilot Review host function (poll for Copilot review comments)
foreign import ccall "wait_for_copilot_review" host_wait_for_copilot_review :: Word64 -> IO Word64

callHost :: (ToJSON req, FromJSON resp) => (Word64 -> IO Word64) -> req -> IO (Either String resp)
callHost rawFn request = do
  -- Encode and allocate
  let reqBs = toStrict (encode request)

  bracket (alloc reqBs) free $ \reqMem -> do
    -- Call host
    respOffset <- rawFn (memoryOffset reqMem)

    -- Read response (ensure we free the response memory)
    respMem <- findMemory respOffset
    bracket (pure respMem) free $ \_ -> do
      loadResult <- load respMem

      -- Decode (load returns Either String ByteString)
      case loadResult of
        Left loadErr -> pure $ Left ("Load error: " ++ loadErr)
        Right respBs -> case eitherDecode (fromStrict respBs) of
          Left err -> pure $ Left ("Decode error: " ++ err)
          Right v -> pure $ Right v

-- | Call a host function that returns void (fire-and-forget, e.g., logging)
callHostVoid :: (ToJSON req) => (Word64 -> IO ()) -> req -> IO ()
callHostVoid rawFn request = do
  let reqBs = toStrict (encode request)
  bracket (alloc reqBs) free $ \reqMem -> do
    rawFn (memoryOffset reqMem)
