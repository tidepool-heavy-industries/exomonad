-- | FileSystem effect interpreter - basic file operations.
--
-- Implements FileSystem effect using standard Haskell IO operations.
-- Provides a testable abstraction over file system operations.
--
-- = Usage
--
-- @
-- import Tidepool.FileSystem.Interpreter (runFileSystemIO)
-- import Tidepool.Effects.FileSystem
--
-- main = runM $ runFileSystemIO $ do
--   result <- createDirectory "/path/to/dir"
--   case result of
--     Right () -> writeFileText "/path/to/file" "content"
--     Left err -> pure (Left err)
-- @
module Tidepool.FileSystem.Interpreter
  ( -- * Interpreter
    runFileSystemIO
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.Freer (Eff, LastMember, interpret, sendM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
  ( createDirectoryIfMissing
  , copyFile
  , doesFileExist
  , doesDirectoryExist
  )
import System.FilePath (takeDirectory)
import System.Posix.Files (createSymbolicLink)

import Tidepool.Effects.FileSystem
  ( FileSystem(..)
  , FileSystemError(..)
  )


-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run FileSystem effects using standard Haskell IO operations.
--
-- All operations are wrapped in try/catch to return explicit errors.
runFileSystemIO :: LastMember IO effs => Eff (FileSystem ': effs) a -> Eff effs a
runFileSystemIO = interpret $ \case
  CreateDirectory path -> sendM $ createDirectoryIO path
  WriteFileText path content -> sendM $ writeFileTextIO path content
  CopyFile src dest -> sendM $ copyFileIO src dest
  CreateSymlink target link -> sendM $ createSymlinkIO target link
  FileExists path -> sendM $ fileExistsIO path
  DirectoryExists path -> sendM $ directoryExistsIO path
  ReadFileText path -> sendM $ readFileTextIO path


-- ════════════════════════════════════════════════════════════════════════════
-- IMPLEMENTATION
-- ════════════════════════════════════════════════════════════════════════════

-- | Create a directory (including parent directories).
createDirectoryIO :: FilePath -> IO (Either FileSystemError ())
createDirectoryIO path = do
  result <- try @SomeException $ createDirectoryIfMissing True path
  case result of
    Left e -> pure $ Left FSIOError
      { fseOperation = "createDirectory"
      , fsePath = path
      , fseReason = T.pack (show e)
      }
    Right () -> pure $ Right ()

-- | Write text content to a file.
writeFileTextIO :: FilePath -> Text -> IO (Either FileSystemError ())
writeFileTextIO path content = do
  -- Ensure parent directory exists
  let parentDir = takeDirectory path
  dirResult <- try @SomeException $ createDirectoryIfMissing True parentDir
  case dirResult of
    Left e -> pure $ Left FSIOError
      { fseOperation = "writeFile (create parent)"
      , fsePath = parentDir
      , fseReason = T.pack (show e)
      }
    Right () -> do
      writeResult <- try @SomeException $ TIO.writeFile path content
      case writeResult of
        Left e -> pure $ Left FSIOError
          { fseOperation = "writeFile"
          , fsePath = path
          , fseReason = T.pack (show e)
          }
        Right () -> pure $ Right ()

-- | Copy a file from source to destination.
copyFileIO :: FilePath -> FilePath -> IO (Either FileSystemError ())
copyFileIO src dest = do
  -- Ensure parent directory exists
  let parentDir = takeDirectory dest
  dirResult <- try @SomeException $ createDirectoryIfMissing True parentDir
  case dirResult of
    Left e -> pure $ Left FSIOError
      { fseOperation = "copyFile (create parent)"
      , fsePath = parentDir
      , fseReason = T.pack (show e)
      }
    Right () -> do
      copyResult <- try @SomeException $ copyFile src dest
      case copyResult of
        Left e -> pure $ Left FSIOError
          { fseOperation = "copyFile"
          , fsePath = src <> " -> " <> dest
          , fseReason = T.pack (show e)
          }
        Right () -> pure $ Right ()

-- | Create a symbolic link.
createSymlinkIO :: FilePath -> FilePath -> IO (Either FileSystemError ())
createSymlinkIO target link = do
  -- Ensure parent directory exists
  let parentDir = takeDirectory link
  dirResult <- try @SomeException $ createDirectoryIfMissing True parentDir
  case dirResult of
    Left e -> pure $ Left FSIOError
      { fseOperation = "createSymlink (create parent)"
      , fsePath = parentDir
      , fseReason = T.pack (show e)
      }
    Right () -> do
      linkResult <- try @SomeException $ createSymbolicLink target link
      case linkResult of
        Left e -> pure $ Left FSIOError
          { fseOperation = "createSymlink"
          , fsePath = link <> " -> " <> target
          , fseReason = T.pack (show e)
          }
        Right () -> pure $ Right ()

-- | Check if a file exists.
fileExistsIO :: FilePath -> IO (Either FileSystemError Bool)
fileExistsIO path = do
  result <- try @SomeException $ doesFileExist path
  case result of
    Left e -> pure $ Left FSIOError
      { fseOperation = "fileExists"
      , fsePath = path
      , fseReason = T.pack (show e)
      }
    Right exists -> pure $ Right exists

-- | Check if a directory exists.
directoryExistsIO :: FilePath -> IO (Either FileSystemError Bool)
directoryExistsIO path = do
  result <- try @SomeException $ doesDirectoryExist path
  case result of
    Left e -> pure $ Left FSIOError
      { fseOperation = "directoryExists"
      , fsePath = path
      , fseReason = T.pack (show e)
      }
    Right exists -> pure $ Right exists

-- | Read text content from a file.
readFileTextIO :: FilePath -> IO (Either FileSystemError Text)
readFileTextIO path = do
  result <- try @SomeException $ TIO.readFile path
  case result of
    Left e -> pure $ Left FSIOError
      { fseOperation = "readFileText"
      , fsePath = path
      , fseReason = T.pack (show e)
      }
    Right content -> pure $ Right content
