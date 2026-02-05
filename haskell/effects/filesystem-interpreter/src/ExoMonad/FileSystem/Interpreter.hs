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
--   createDirectory $(mkRelDir "path/to/dir")
--   writeTextFile $(mkRelFile "path/to/file") "content"
-- @
module ExoMonad.FileSystem.Interpreter
  ( -- * Interpreter
    runFileSystemIO,
  )
where

import Data.Text (Text)
import Data.Text.IO qualified as TIO
import ExoMonad.Effects.FileSystem (FileSystem (..))
import Path (toFilePath)
import Polysemy (Member, Sem, embed, interpret)
import Polysemy.Embed (Embed)
import System.Directory qualified as Dir
import System.FilePath (takeDirectory)
import System.Posix.Files (createSymbolicLink)

-- ════════════════════════════════════════════════════════════════════════════
-- INTERPRETER
-- ════════════════════════════════════════════════════════════════════════════

-- | Run FileSystem effects using standard Haskell IO operations.
--
-- Errors are thrown as IO exceptions. Use try/catch if you need to handle them.
runFileSystemIO :: (Member (Embed IO) r) => Sem (FileSystem ': r) a -> Sem r a
runFileSystemIO = interpret $ \case
  CreateDirectory path -> embed $ Dir.createDirectoryIfMissing True (toFilePath path)
  WriteTextFile path content -> embed $ do
    let fp = toFilePath path
    Dir.createDirectoryIfMissing True (takeDirectory fp)
    TIO.writeFile fp content
  CopyFile src dest -> embed $ do
    let destFp = toFilePath dest
    Dir.createDirectoryIfMissing True (takeDirectory destFp)
    Dir.copyFile (toFilePath src) destFp
  CreateSymlink target link -> embed $ do
    let linkFp = toFilePath link
    Dir.createDirectoryIfMissing True (takeDirectory linkFp)
    createSymbolicLink (toFilePath target) linkFp
  FileExists path -> embed $ Dir.doesFileExist (toFilePath path)
  DirectoryExists path -> embed $ Dir.doesDirectoryExist (toFilePath path)
  ReadTextFile path -> embed $ TIO.readFile (toFilePath path)

