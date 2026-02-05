{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module ExoMonad.Path
  ( -- * Re-exports
    module Path,

    -- * Type Aliases for common paths
    AbsFile,
    RelFile,
    AbsDir,
    RelDir,

    -- * Helper types
    AnyPath,

    -- * Conversion helpers
    toFilePathText,
    parseAbsFileT,
    parseRelFileT,
    parseAbsDirT,
    parseRelDirT,
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.Text qualified as T
import Path

type AbsFile = Path Abs File

type RelFile = Path Rel File

type AbsDir = Path Abs Dir

type RelDir = Path Rel Dir

type AnyPath = Either (Path Abs File) (Either (Path Rel File) (Either (Path Abs Dir) (Path Rel Dir)))

-- | Convert a path to Text for logging or shell commands
toFilePathText :: Path b t -> Text
toFilePathText = T.pack . toFilePath

-- | Parse absolute file from Text
parseAbsFileT :: (MonadThrow m) => Text -> m AbsFile
parseAbsFileT = parseAbsFile . T.unpack

-- | Parse relative file from Text
parseRelFileT :: (MonadThrow m) => Text -> m RelFile
parseRelFileT = parseRelFile . T.unpack

-- | Parse absolute directory from Text
parseAbsDirT :: (MonadThrow m) => Text -> m AbsDir
parseAbsDirT = parseAbsDir . T.unpack

-- | Parse relative directory from Text
parseRelDirT :: (MonadThrow m) => Text -> m RelDir
parseRelDirT = parseRelDir . T.unpack
