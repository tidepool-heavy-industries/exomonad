{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | FileSystem effect for basic file operations.
--
-- Effect type only - interpreter lives in exomonad-filesystem-interpreter.
-- Provides pure abstraction over file system operations for testability.
--
-- = Example Usage
--
-- @
-- import ExoMonad.Effects.FileSystem
--
-- setupHandler :: Member FileSystem r => Sem r (Either FileSystemError ())
-- setupHandler = do
--   result <- createDirectory "/path/to/.exomonad"
--   case result of
--     Right () -> writeFileText "/path/to/.exomonad/config" "..."
--     Left err -> pure (Left err)
-- @
--
-- = Design Notes
--
-- Operations return @Either FileSystemError a@ for explicit error handling.
-- This keeps agents IO-blind while enabling file system operations.
module ExoMonad.Effects.FileSystem
  ( -- * Effect
    FileSystem (..),

    -- * Smart Constructors
    createDirectory,
    writeFileText,
    copyFile,
    createSymlink,
    fileExists,
    directoryExists,
    readFileText,

    -- * Error Types
    FileSystemError (..),
  )
where

import Prelude hiding (readFileText, writeFileText)

import Polysemy (Sem, Member, makeSem)
import Data.Kind (Type)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- ════════════════════════════════════════════════════════════════════════════
-- ERRORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Errors that can occur during file system operations.
data FileSystemError
  = FSIOError
  { -- | Operation that failed (e.g., "createDirectory", "writeFile")
    fseOperation :: Text,
    -- | Path involved in the operation
    fsePath :: FilePath,
    -- | Description of what went wrong
    fseReason :: Text
  }
  -- \^ General IO error during file operation
  deriving stock (Eq, Show, Generic)

instance ToJSON FileSystemError

instance FromJSON FileSystemError

-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | FileSystem effect for basic file operations.
--
-- All operations return @Either FileSystemError@ for explicit error handling.
data FileSystem m a where
  -- | Create a directory (including parent directories).
  -- Returns () on success, or an error if creation failed.
  CreateDirectory ::
    FilePath ->
    FileSystem m (Either FileSystemError ())
  -- | Write text content to a file.
  -- Creates the file if it doesn't exist, overwrites if it does.
  WriteFileText ::
    FilePath ->
    Text ->
    FileSystem m (Either FileSystemError ())
  -- | Copy a file from source to destination.
  -- Creates parent directories of destination if needed.
  CopyFile ::
    -- | Source path
    FilePath ->
    -- | Destination path
    FilePath ->
    FileSystem m (Either FileSystemError ())
  -- | Create a symbolic link.
  -- Target is the file/directory the link points to.
  -- Link is the new symlink path.
  CreateSymlink ::
    -- | Target (what the link points to)
    FilePath ->
    -- | Link path (the new symlink)
    FilePath ->
    FileSystem m (Either FileSystemError ())
  -- | Check if a file exists.
  FileExists ::
    FilePath ->
    FileSystem m (Either FileSystemError Bool)
  -- | Check if a directory exists.
  DirectoryExists ::
    FilePath ->
    FileSystem m (Either FileSystemError Bool)
  -- | Read text content from a file.
  ReadFileText ::
    FilePath ->
    FileSystem m (Either FileSystemError Text)

makeSem ''FileSystem
