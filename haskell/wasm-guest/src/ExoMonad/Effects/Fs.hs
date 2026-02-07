{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Filesystem effects for reading, writing, and querying files.
--
-- All effects are dispatched via the @fs@ namespace.
-- Request and response types are proto-generated from @proto/effects/fs.proto@.
module ExoMonad.Effects.Fs
  ( -- * Effect Types
    FsReadFile,
    FsWriteFile,
    FsFileExists,
    FsListDirectory,
    FsDeleteFile,

    -- * Smart Constructors
    readFile,
    writeFile,
    fileExists,
    listDirectory,
    deleteFile,

    -- * Re-exported proto types
    module Effects.Fs,
  )
where

import Effects.EffectError (EffectError)
import Effects.Fs
import ExoMonad.Effect.Class (Effect (..), runEffect)
import Prelude hiding (readFile, writeFile)

-- ============================================================================
-- Effect phantom types + instances
-- ============================================================================

data FsReadFile

instance Effect FsReadFile where
  type Input FsReadFile = ReadFileRequest
  type Output FsReadFile = ReadFileResponse
  effectId = "fs.read_file"

data FsWriteFile

instance Effect FsWriteFile where
  type Input FsWriteFile = WriteFileRequest
  type Output FsWriteFile = WriteFileResponse
  effectId = "fs.write_file"

data FsFileExists

instance Effect FsFileExists where
  type Input FsFileExists = FileExistsRequest
  type Output FsFileExists = FileExistsResponse
  effectId = "fs.file_exists"

data FsListDirectory

instance Effect FsListDirectory where
  type Input FsListDirectory = ListDirectoryRequest
  type Output FsListDirectory = ListDirectoryResponse
  effectId = "fs.list_directory"

data FsDeleteFile

instance Effect FsDeleteFile where
  type Input FsDeleteFile = DeleteFileRequest
  type Output FsDeleteFile = DeleteFileResponse
  effectId = "fs.delete_file"

-- ============================================================================
-- Smart constructors
-- ============================================================================

readFile :: ReadFileRequest -> IO (Either EffectError ReadFileResponse)
readFile = runEffect @FsReadFile

writeFile :: WriteFileRequest -> IO (Either EffectError WriteFileResponse)
writeFile = runEffect @FsWriteFile

fileExists :: FileExistsRequest -> IO (Either EffectError FileExistsResponse)
fileExists = runEffect @FsFileExists

listDirectory :: ListDirectoryRequest -> IO (Either EffectError ListDirectoryResponse)
listDirectory = runEffect @FsListDirectory

deleteFile :: DeleteFileRequest -> IO (Either EffectError DeleteFileResponse)
deleteFile = runEffect @FsDeleteFile
