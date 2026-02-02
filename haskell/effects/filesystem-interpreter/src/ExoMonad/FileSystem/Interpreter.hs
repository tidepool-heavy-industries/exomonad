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
import Polysemy (Sem, Member, interpret, embed)
import Polysemy.Embed (Embed)
import Data.Text (Text)
import Data.Text qualified as T
import ExoMonad.Effects.FileSystem
  ( FileSystem (..),
    FileSystemError (..),
  )
import ExoMonad.Path (Path, File, Dir, toFilePathText, toFilePath)
import Path.IO
  ( copyFile,
    createDirIfMissing,
    doesDirExist,
    doesFileExist,
    ensureDir,
    createFileLink,
  )
import Path.IO qualified as PathIO

-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run FileSystem effects using standard Haskell IO operations (via path-io).
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
createDirectoryIO :: Path b Dir -> IO (Either FileSystemError ())
createDirectoryIO path = do
  result <- try @SomeException $ createDirIfMissing True path
  case result of
    Left e ->
      pure $
        Left
          FSIOError
            { fseOperation = "createDirectory",
              fsePath = toFilePathText path,
              fseReason = T.pack (show e)
            }
    Right () -> pure $ Right ()

-- | Write text content to a file.
writeFileTextIO :: Path b File -> Text -> IO (Either FileSystemError ())
writeFileTextIO path content = do
  -- ensureDir creates parents for the file
  dirResult <- try @SomeException $ ensureDir path
  case dirResult of
    Left e ->
      pure $
        Left
          FSIOError
            { fseOperation = "writeFile (create parent)",
              fsePath = toFilePathText path,
              fseReason = T.pack (show e)
            }
    Right () -> do
      -- PathIO.readFileUtf8 exists, but writeFileUtf8 is usually just TIO.writeFile with toFilePath
      -- Actually path-io doesn't seem to export writeFileText/Utf8 directly?
      -- Checked path-io docs: it has generic IO helpers but strict text IO is usually via Data.Text.IO
      -- But we can use toFilePath to bridge.
      writeResult <- try @SomeException $ PathIO.writeFileUtf8 path content
      case writeResult of
        Left e ->
          pure $
            Left
              FSIOError
                { fseOperation = "writeFile",
                  fsePath = toFilePathText path,
                  fseReason = T.pack (show e)
                }
        Right () -> pure $ Right ()

-- | Copy a file from source to destination.
copyFileIO :: Path b1 File -> Path b2 File -> IO (Either FileSystemError ())
copyFileIO src dest = do
  dirResult <- try @SomeException $ ensureDir dest
  case dirResult of
    Left e ->
      pure $
        Left
          FSIOError
            { fseOperation = "copyFile (create parent)",
              fsePath = toFilePathText dest,
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
                  fsePath = toFilePathText src <> " -> " <> toFilePathText dest,
                  fseReason = T.pack (show e)
                }
        Right () -> pure $ Right ()

-- | Create a symbolic link.
createSymlinkIO :: Path b1 t1 -> Path b2 t2 -> IO (Either FileSystemError ())
createSymlinkIO target link = do
  -- ensureDir works on Path b t, extracting the parent dir
  dirResult <- try @SomeException $ ensureDir link
  case dirResult of
    Left e ->
      pure $
        Left
          FSIOError
            { fseOperation = "createSymlink (create parent)",
              fsePath = toFilePathText link,
              fseReason = T.pack (show e)
            }
    Right () -> do
      linkResult <- try @SomeException $ createFileLink target link
      case linkResult of
        Left e ->
          pure $
            Left
              FSIOError
                { fseOperation = "createSymlink",
                  fsePath = toFilePathText link <> " -> " <> toFilePathText target,
                  fseReason = T.pack (show e)
                }
        Right () -> pure $ Right ()

-- | Check if a file exists.
fileExistsIO :: Path b File -> IO (Either FileSystemError Bool)
fileExistsIO path = do
  result <- try @SomeException $ doesFileExist path
  case result of
    Left e ->
      pure $
        Left
          FSIOError
            { fseOperation = "fileExists",
              fsePath = toFilePathText path,
              fseReason = T.pack (show e)
            }
    Right exists -> pure $ Right exists

-- | Check if a directory exists.
directoryExistsIO :: Path b Dir -> IO (Either FileSystemError Bool)
directoryExistsIO path = do
  result <- try @SomeException $ doesDirExist path
  case result of
    Left e ->
      pure $
        Left
          FSIOError
            { fseOperation = "directoryExists",
              fsePath = toFilePathText path,
              fseReason = T.pack (show e)
            }
    Right exists -> pure $ Right exists

-- | Read text content from a file.
readFileTextIO :: Path b File -> IO (Either FileSystemError Text)
readFileTextIO path = do
  result <- try @SomeException $ PathIO.readFileUtf8 path
  case result of
    Left e ->
      pure $
        Left
          FSIOError
            { fseOperation = "readFileText",
              fsePath = toFilePathText path,
              fseReason = T.pack (show e)
            }
    Right content -> pure $ Right content
