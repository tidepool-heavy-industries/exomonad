-- | FileSystem effect interpreter - basic file operations.
--
-- Implements FileSystem effect using standard Haskell IO operations.
-- Provides a testable abstraction over file system operations.
--
-- = Usage
--
-- @
-- import ExoMonad.FileSystem.Interpreter (runFileSystemIO)
-- import ExoMonad.Effects.FileSystem
--
-- main = runM $ runFileSystemIO $ do
--   result <- createDirectory "/path/to/dir"
--   case result of
--     Right () -> writeFileText "/path/to/file" "content"
--     Left err -> pure (Left err)
-- @
module ExoMonad.FileSystem.Interpreter
  ( -- * Interpreter
    runFileSystemIO,
  )
where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import ExoMonad.Effects.FileSystem
  ( FileSystem (..),
    FileSystemError (..),
  )
import Polysemy (Member, Sem, embed, interpret)
import Polysemy.Embed (Embed)
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
  )
import System.FilePath (takeDirectory)
import System.Posix.Files (createSymbolicLink)

-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run FileSystem effects using standard Haskell IO operations.
--
-- All operations are wrapped in try/catch to return explicit errors.
runFileSystemIO :: (Member (Embed IO) r) => Sem (FileSystem ': r) a -> Sem r a
runFileSystemIO = interpret $ \case
  CreateDirectory path -> embed $ createDirectoryIO path
  WriteFileText path content -> embed $ writeFileTextIO path content
  CopyFile src dest -> embed $ copyFileIO src dest
  CreateSymlink target link -> embed $ createSymlinkIO target link
  FileExists path -> embed $ fileExistsIO path
  DirectoryExists path -> embed $ directoryExistsIO path
  ReadFileText path -> embed $ readFileTextIO path

-- ════════════════════════════════════════════════════════════════════════════
-- IMPLEMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Create a directory (including parent directories).
createDirectoryIO :: FilePath -> IO (Either FileSystemError ())
createDirectoryIO path = do
  result <- try @SomeException $ createDirectoryIfMissing True path
  case result of
    Left e ->
      pure $
        Left
          FSIOError
            { fseOperation = "createDirectory",
              fsePath = path,
              fseReason = T.pack (show e)
            }
    Right () -> pure $ Right ()

-- | Write text content to a file.
writeFileTextIO :: FilePath -> Text -> IO (Either FileSystemError ())
writeFileTextIO path content = do
  -- Ensure parent directory exists
  let parentDir = takeDirectory path
  dirResult <- try @SomeException $ createDirectoryIfMissing True parentDir
  case dirResult of
    Left e ->
      pure $
        Left
          FSIOError
            { fseOperation = "writeFile (create parent)",
              fsePath = parentDir,
              fseReason = T.pack (show e)
            }
    Right () -> do
      writeResult <- try @SomeException $ TIO.writeFile path content
      case writeResult of
        Left e ->
          pure $
            Left
              FSIOError
                { fseOperation = "writeFile",
                  fsePath = path,
                  fseReason = T.pack (show e)
                }
        Right () -> pure $ Right ()

-- | Copy a file from source to destination.
copyFileIO :: FilePath -> FilePath -> IO (Either FileSystemError ())
copyFileIO src dest = do
  -- Ensure parent directory exists
  let parentDir = takeDirectory dest
  dirResult <- try @SomeException $ createDirectoryIfMissing True parentDir
  case dirResult of
    Left e ->
      pure $
        Left
          FSIOError
            { fseOperation = "copyFile (create parent)",
              fsePath = parentDir,
              fseReason = T.pack (show e)
            }
    Right () -> do
      copyResult <- try @SomeException $ copyFile src dest
      case copyResult of
        Left e ->
          pure $
            Left
              FSIOError
                { fseOperation = "copyFile",
                  fsePath = src <> " -> " <> dest,
                  fseReason = T.pack (show e)
                }
        Right () -> pure $ Right ()

-- | Create a symbolic link.
createSymlinkIO :: FilePath -> FilePath -> IO (Either FileSystemError ())
createSymlinkIO target link = do
  -- Ensure parent directory exists
  let parentDir = takeDirectory link
  dirResult <- try @SomeException $ createDirectoryIfMissing True parentDir
  case dirResult of
    Left e ->
      pure $
        Left
          FSIOError
            { fseOperation = "createSymlink (create parent)",
              fsePath = parentDir,
              fseReason = T.pack (show e)
            }
    Right () -> do
      linkResult <- try @SomeException $ createSymbolicLink target link
      case linkResult of
        Left e ->
          pure $
            Left
              FSIOError
                { fseOperation = "createSymlink",
                  fsePath = link <> " -> " <> target,
                  fseReason = T.pack (show e)
                }
        Right () -> pure $ Right ()

-- | Check if a file exists.
fileExistsIO :: FilePath -> IO (Either FileSystemError Bool)
fileExistsIO path = do
  result <- try @SomeException $ doesFileExist path
  case result of
    Left e ->
      pure $
        Left
          FSIOError
            { fseOperation = "fileExists",
              fsePath = path,
              fseReason = T.pack (show e)
            }
    Right exists -> pure $ Right exists

-- | Check if a directory exists.
directoryExistsIO :: FilePath -> IO (Either FileSystemError Bool)
directoryExistsIO path = do
  result <- try @SomeException $ doesDirectoryExist path
  case result of
    Left e ->
      pure $
        Left
          FSIOError
            { fseOperation = "directoryExists",
              fsePath = path,
              fseReason = T.pack (show e)
            }
    Right exists -> pure $ Right exists

-- | Read text content from a file.
readFileTextIO :: FilePath -> IO (Either FileSystemError Text)
readFileTextIO path = do
  result <- try @SomeException $ TIO.readFile path
  case result of
    Left e ->
      pure $
        Left
          FSIOError
            { fseOperation = "readFileText",
              fsePath = path,
              fseReason = T.pack (show e)
            }
    Right content -> pure $ Right content
