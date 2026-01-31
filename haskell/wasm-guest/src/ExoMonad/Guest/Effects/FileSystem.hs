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
    HostResult (..),
  )
where

import Control.Monad.Freer
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text
import ExoMonad.Guest.HostCall (callHost, host_fs_read_file, host_fs_write_file)
import GHC.Generics (Generic)
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

-- | Host result wrapper (matches Rust HostResult).
data HostResult a
  = Success a
  | HostError Text
  deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (HostResult a) where
  parseJSON = withObject "HostResult" $ \v -> do
    kind <- v .: "kind" :: Parser Text
    case kind of
      "Success" -> Success <$> v .: "payload"
      "Error" -> do
        errObj <- v .: "payload"
        HostError <$> (errObj .: "message")
      _ -> fail "Unknown HostResult kind"

-- ============================================================================
-- Effect type
-- ============================================================================

-- | Filesystem effect for file operations.
data FileSystem r where
  -- | Read a file.
  ReadFile :: Text -> Int -> FileSystem (Either Text ReadFileOutput)
  -- | Write a file.
  WriteFile :: Text -> Text -> Bool -> FileSystem (Either Text WriteFileOutput)

-- ============================================================================
-- Smart constructors
-- ============================================================================

-- | Read a file (path, max_bytes).
readFile :: (Member FileSystem effs) => Text -> Int -> Eff effs (Either Text ReadFileOutput)
readFile path maxBytes = send (ReadFile path maxBytes)

-- | Write a file (path, content, create_parents).
writeFile :: (Member FileSystem effs) => Text -> Text -> Bool -> Eff effs (Either Text WriteFileOutput)
writeFile path content createParents = send (WriteFile path content createParents)

-- ============================================================================
-- Interpreter
-- ============================================================================

-- | Interpret FileSystem by calling Rust host functions.
runFileSystem :: (LastMember IO effs) => Eff (FileSystem ': effs) a -> Eff effs a
runFileSystem = interpret $ \case
  ReadFile path maxBytes -> sendM $ do
    let input = ReadFileInput path maxBytes
    res <- callHost host_fs_read_file input
    pure $ case res of
      Left err -> Left (Data.Text.pack err)
      Right (Success r) -> Right r
      Right (HostError msg) -> Left msg
  WriteFile path content createParents -> sendM $ do
    let input = WriteFileInput path content createParents
    res <- callHost host_fs_write_file input
    pure $ case res of
      Left err -> Left (Data.Text.pack err)
      Right (Success r) -> Right r
      Right (HostError msg) -> Left msg
