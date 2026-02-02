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

import ExoMonad.Path
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
-- Operations throw 'FileSystemError' via Polysemy Error effect.
-- Using 'Path' types ensures we don't mix up files and directories or
-- relative and absolute paths at the type level.
data FileSystem m a where
  -- | Create a directory (including parent directories).
  CreateDirectory ::
    Path b Dir ->
    FileSystem m ()
  -- | Write text content to a file.
  -- Creates the file if it doesn't exist, overwrites if it does.
  WriteFileText ::
    Path b File ->
    Text ->
    FileSystem m ()
  -- | Copy a file from source to destination.
  CopyFile ::
    -- | Source path
    Path b1 File ->
    -- | Destination path
    Path b2 File ->
    FileSystem m ()
  -- | Create a symbolic link.
  CreateSymlink ::
    -- | Target (what the link points to)
    Path b1 t1 ->
    -- | Link path (the new symlink)
    Path b2 t2 ->
    FileSystem m ()
  -- | Check if a file exists.
  FileExists ::
    Path b File ->
    FileSystem m Bool
  -- | Check if a directory exists.
  DirectoryExists ::
    Path b Dir ->
    FileSystem m Bool
  -- | Read text content from a file.
  ReadFileText ::
    Path b File ->
    FileSystem m Text

makeSem ''FileSystem
