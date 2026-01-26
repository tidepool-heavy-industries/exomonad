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
-- setupHandler :: Member FileSystem effs => Eff effs (Either FileSystemError ())
-- setupHandler = do
--   result <- createDirectory "/path/to/.exomonad"
--   case result of
--     Right () -> writeFile "/path/to/.exomonad/config" "..."
--     Left err -> pure (Left err)
-- @
--
-- = Design Notes
--
-- Operations return @Either FileSystemError a@ for explicit error handling.
-- This keeps agents IO-blind while enabling file system operations.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module ExoMonad.Effects.FileSystem
  ( -- * Effect
    FileSystem(..)

    -- * Smart Constructors
  , createDirectory
  , writeFileText
  , copyFile
  , createSymlink
  , fileExists
  , directoryExists
  , readFileText

    -- * Error Types
  , FileSystemError(..)
  ) where

import Control.Monad.Freer (Eff, Member, send)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)


-- ════════════════════════════════════════════════════════════════════════════
-- ERRORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Errors that can occur during file system operations.
data FileSystemError
  = FSIOError
      { fseOperation :: Text
        -- ^ Operation that failed (e.g., "createDirectory", "writeFile")
      , fsePath :: FilePath
        -- ^ Path involved in the operation
      , fseReason :: Text
        -- ^ Description of what went wrong
      }
    -- ^ General IO error during file operation
  deriving stock (Eq, Show, Generic)

instance ToJSON FileSystemError
instance FromJSON FileSystemError


-- ════════════════════════════════════════════════════════════════════════════
-- EFFECT
-- ════════════════════════════════════════════════════════════════════════════

-- | FileSystem effect for basic file operations.
--
-- All operations return @Either FileSystemError@ for explicit error handling.
data FileSystem r where
  -- | Create a directory (including parent directories).
  -- Returns () on success, or an error if creation failed.
  CreateDirectory
    :: FilePath
    -> FileSystem (Either FileSystemError ())

  -- | Write text content to a file.
  -- Creates the file if it doesn't exist, overwrites if it does.
  WriteFileText
    :: FilePath
    -> Text
    -> FileSystem (Either FileSystemError ())

  -- | Copy a file from source to destination.
  -- Creates parent directories of destination if needed.
  CopyFile
    :: FilePath       -- ^ Source path
    -> FilePath       -- ^ Destination path
    -> FileSystem (Either FileSystemError ())

  -- | Create a symbolic link.
  -- Target is the file/directory the link points to.
  -- Link is the new symlink path.
  CreateSymlink
    :: FilePath       -- ^ Target (what the link points to)
    -> FilePath       -- ^ Link path (the new symlink)
    -> FileSystem (Either FileSystemError ())

  -- | Check if a file exists.
  FileExists
    :: FilePath
    -> FileSystem (Either FileSystemError Bool)

  -- | Check if a directory exists.
  DirectoryExists
    :: FilePath
    -> FileSystem (Either FileSystemError Bool)

  -- | Read text content from a file.
  ReadFileText
    :: FilePath
    -> FileSystem (Either FileSystemError Text)


-- ════════════════════════════════════════════════════════════════════════════
-- SMART CONSTRUCTORS
-- ════════════════════════════════════════════════════════════════════════════

-- | Create a directory (including parent directories).
--
-- @
-- result <- createDirectory "/path/to/new/dir"
-- case result of
--   Right () -> -- directory created
--   Left err -> handleError err
-- @
createDirectory
  :: Member FileSystem effs
  => FilePath
  -> Eff effs (Either FileSystemError ())
createDirectory = send . CreateDirectory

-- | Write text content to a file.
--
-- @
-- result <- writeFileText "/path/to/file.txt" "Hello, world!"
-- @
writeFileText
  :: Member FileSystem effs
  => FilePath
  -> Text
  -> Eff effs (Either FileSystemError ())
writeFileText path content = send (WriteFileText path content)

-- | Copy a file from source to destination.
--
-- @
-- result <- copyFile "/src/template.yaml" "/dest/config.yaml"
-- @
copyFile
  :: Member FileSystem effs
  => FilePath
  -> FilePath
  -> Eff effs (Either FileSystemError ())
copyFile src dest = send (CopyFile src dest)

-- | Create a symbolic link.
--
-- @
-- result <- createSymlink "/path/to/.env" "/worktree/.env"
-- @
createSymlink
  :: Member FileSystem effs
  => FilePath  -- ^ Target (what the link points to)
  -> FilePath  -- ^ Link path (the new symlink)
  -> Eff effs (Either FileSystemError ())
createSymlink target link = send (CreateSymlink target link)

-- | Check if a file exists.
--
-- @
-- result <- fileExists "/path/to/file.txt"
-- case result of
--   Right True -> -- file exists
--   Right False -> -- file does not exist
--   Left err -> handleError err
-- @
fileExists
  :: Member FileSystem effs
  => FilePath
  -> Eff effs (Either FileSystemError Bool)
fileExists = send . FileExists

-- | Check if a directory exists.
--
-- @
-- result <- directoryExists "/path/to/dir"
-- @
directoryExists
  :: Member FileSystem effs
  => FilePath
  -> Eff effs (Either FileSystemError Bool)
directoryExists = send . DirectoryExists

-- | Read text content from a file.
--
-- @
-- result <- readFileText "/path/to/file.txt"
-- case result of
--   Right content -> -- use content
--   Left err -> handleError err
-- @
readFileText
  :: Member FileSystem effs
  => FilePath
  -> Eff effs (Either FileSystemError Text)
readFileText = send . ReadFileText
