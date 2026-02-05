{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Filesystem effects for reading and writing files.
--
-- All I/O is handled by the Rust host.
module ExoMonad.Guest.Effects.FileSystem
  ( -- * Effect type
    FileSystem (..),

    -- * Smart constructors
    readFile,
    writeFile,

    -- * Interpreter
    runFileSystem,

    -- * Types
    ReadFileInput (..),
    ReadFileOutput (..),
    WriteFileInput (..),
    WriteFileOutput (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Text (Text)
import ExoMonad.Guest.FFI (FFIBoundary)
import ExoMonad.Guest.HostCall (callHost, host_fs_read_file, host_fs_write_file)
import GHC.Generics (Generic)
import Polysemy (Member, Sem, embed, interpret, send)
import Polysemy.Embed (Embed)
import Prelude hiding (readFile, writeFile)

-- ============================================================================
-- Types (match Rust filesystem.rs)
-- ============================================================================

-- | Input for reading a file.
data ReadFileInput = ReadFileInput
  { rfiPath :: Text,
    rfiMaxBytes :: Int -- 0 = unlimited
  }
  deriving (Show, Eq, Generic)

instance ToJSON ReadFileInput where
  toJSON (ReadFileInput p m) =
    object
      [ "path" .= p,
        "max_bytes" .= m
      ]

instance FromJSON ReadFileInput where
  parseJSON = withObject "ReadFileInput" $ \v ->
    ReadFileInput
      <$> v .: "path"
      <*> v .: "max_bytes"

instance FFIBoundary ReadFileInput

-- | Result of reading a file.
data ReadFileOutput = ReadFileOutput
  { rfoContent :: Text,
    rfoBytesRead :: Int,
    rfoTruncated :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON ReadFileOutput where
  parseJSON = withObject "ReadFileOutput" $ \v ->
    ReadFileOutput
      <$> v .: "content"
      <*> v .: "bytes_read"
      <*> v .: "truncated"

instance ToJSON ReadFileOutput where
  toJSON (ReadFileOutput c b t) =
    object
      [ "content" .= c,
        "bytes_read" .= b,
        "truncated" .= t
      ]

instance FFIBoundary ReadFileOutput

-- | Input for writing a file.
data WriteFileInput = WriteFileInput
  { wfiPath :: Text,
    wfiContent :: Text,
    wfiCreateParents :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON WriteFileInput where
  toJSON (WriteFileInput p c cp) =
    object
      [ "path" .= p,
        "content" .= c,
        "create_parents" .= cp
      ]

instance FromJSON WriteFileInput where
  parseJSON = withObject "WriteFileInput" $ \v ->
    WriteFileInput
      <$> v .: "path"
      <*> v .: "content"
      <*> v .: "create_parents"

instance FFIBoundary WriteFileInput

-- | Result of writing a file.
data WriteFileOutput = WriteFileOutput
  { wfoBytesWritten :: Int,
    wfoPath :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON WriteFileOutput where
  parseJSON = withObject "WriteFileOutput" $ \v ->
    WriteFileOutput
      <$> v .: "bytes_written"
      <*> v .: "path"

instance ToJSON WriteFileOutput where
  toJSON (WriteFileOutput b p) =
    object
      [ "bytes_written" .= b,
        "path" .= p
      ]

instance FFIBoundary WriteFileOutput

-- ============================================================================
-- Effect type
-- ============================================================================

-- | Filesystem effect for file operations.
data FileSystem m a where
  -- | Read a file.
  ReadFileOp :: Text -> Int -> FileSystem m (Either Text ReadFileOutput)
  -- | Write a file.
  WriteFileOp :: Text -> Text -> Bool -> FileSystem m (Either Text WriteFileOutput)

-- Smart constructors (manually written - makeSem doesn't work with WASM cross-compilation)
readFile :: (Member FileSystem r) => Text -> Int -> Sem r (Either Text ReadFileOutput)
readFile path maxBytes = send (ReadFileOp path maxBytes)

writeFile :: (Member FileSystem r) => Text -> Text -> Bool -> Sem r (Either Text WriteFileOutput)
writeFile path content createParents = send (WriteFileOp path content createParents)

-- ============================================================================
-- Interpreter
-- ============================================================================

-- | Interpret FileSystem by calling Rust host functions.
runFileSystem :: (Member (Embed IO) r) => Sem (FileSystem ': r) a -> Sem r a
runFileSystem = interpret $ \case
  ReadFileOp path maxBytes -> embed $ do
    let input = ReadFileInput path maxBytes
    callHost host_fs_read_file input
  WriteFileOp path content createParents -> embed $ do
    let input = WriteFileInput path content createParents
    callHost host_fs_write_file input
