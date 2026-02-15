{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Filesystem effects for reading and writing files.
--
-- All I/O is handled by the Rust host via the yield_effect mechanism.
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

import Control.Monad.Freer (Eff, LastMember, Member, interpret, send, sendM)
import Data.Text (Text)
import Data.Text qualified as T
import Effects.EffectError (EffectError)
import ExoMonad.Effects.Fs qualified as Fs
import ExoMonad.Guest.Proto (fromText, toText)
import GHC.Generics (Generic)
import Prelude hiding (readFile, writeFile)

-- ============================================================================
-- Types (maintained for backward compatibility with callers)
-- ============================================================================

-- | Input for reading a file.
data ReadFileInput = ReadFileInput
  { rfiPath :: Text,
    rfiMaxBytes :: Int -- 0 = unlimited
  }
  deriving (Show, Eq, Generic)

-- | Result of reading a file.
data ReadFileOutput = ReadFileOutput
  { rfoContent :: Text,
    rfoBytesRead :: Int,
    rfoTruncated :: Bool
  }
  deriving (Show, Eq, Generic)

-- | Input for writing a file.
data WriteFileInput = WriteFileInput
  { wfiPath :: Text,
    wfiContent :: Text,
    wfiCreateParents :: Bool
  }
  deriving (Show, Eq, Generic)

-- | Result of writing a file.
data WriteFileOutput = WriteFileOutput
  { wfoBytesWritten :: Int,
    wfoPath :: Text
  }
  deriving (Show, Eq, Generic)

-- ============================================================================
-- Effect type
-- ============================================================================

-- | Filesystem effect for file operations.
data FileSystem a where
  -- | Read a file.
  ReadFileOp :: Text -> Int -> FileSystem (Either Text ReadFileOutput)
  -- | Write a file.
  WriteFileOp :: Text -> Text -> Bool -> FileSystem (Either Text WriteFileOutput)

-- Smart constructors (manually written - makeSem doesn't work with WASM cross-compilation)
readFile :: (Member FileSystem r) => Text -> Int -> Eff r (Either Text ReadFileOutput)
readFile path maxBytes = send (ReadFileOp path maxBytes)

writeFile :: (Member FileSystem r) => Text -> Text -> Bool -> Eff r (Either Text WriteFileOutput)
writeFile path content createParents = send (WriteFileOp path content createParents)

-- ============================================================================
-- Interpreter (uses yield_effect via Effect typeclass)
-- ============================================================================

-- | Interpret FileSystem by calling Rust host via yield_effect.
runFileSystem :: (LastMember IO r) => Eff (FileSystem ': r) a -> Eff r a
runFileSystem = interpret $ \case
  ReadFileOp path maxBytes -> sendM $ do
    let req =
          Fs.ReadFileRequest
            { Fs.readFileRequestPath = fromText path,
              Fs.readFileRequestMaxBytes = fromIntegral maxBytes,
              Fs.readFileRequestOffset = 0
            }
    result <- Fs.readFile req
    pure $ case result of
      Left err -> Left (effectErrorToText err)
      Right resp ->
        Right
          ReadFileOutput
            { rfoContent = toText (Fs.readFileResponseContent resp),
              rfoBytesRead = fromIntegral (Fs.readFileResponseBytesRead resp),
              rfoTruncated = Fs.readFileResponseTruncated resp
            }
  WriteFileOp path content createParents -> sendM $ do
    let req =
          Fs.WriteFileRequest
            { Fs.writeFileRequestPath = fromText path,
              Fs.writeFileRequestContent = fromText content,
              Fs.writeFileRequestCreateParents = createParents,
              Fs.writeFileRequestAppend = False
            }
    result <- Fs.writeFile req
    pure $ case result of
      Left err -> Left (effectErrorToText err)
      Right resp ->
        Right
          WriteFileOutput
            { wfoBytesWritten = fromIntegral (Fs.writeFileResponseBytesWritten resp),
              wfoPath = toText (Fs.writeFileResponsePath resp)
            }

-- | Convert EffectError to Text for backward compatibility.
effectErrorToText :: EffectError -> Text
effectErrorToText err = T.pack (show err)
