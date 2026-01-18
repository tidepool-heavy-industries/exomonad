{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tidepool.Teaching.Record
  ( initRecording
  , recordExample
  , recordTurn
  , closeRecording
  , writeMetadata
  ) where

import Data.Aeson (encode, object, (.=), toJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (IOMode(..), hClose, hFlush, hPutChar, openFile)

import Tidepool.Teaching.Types

-- | Initialize a recording session.
--
-- Creates a session directory with the structure:
--   .tidepool/training/session-{uuid}/
--     ├── anthropic.jsonl
--     ├── gemma.jsonl
--     └── metadata.json
--
-- The JSONL files are opened in append mode with line buffering.
initRecording :: FilePath -> UUID -> IO RecordingHandles
initRecording baseDir sessionId = do
  let sessionDir = baseDir </> ("session-" <> UUID.toString sessionId)
  createDirectoryIfMissing True sessionDir

  -- Open file handles in append mode
  rawHandle <- openFile (sessionDir </> "anthropic.jsonl") AppendMode
  gemmaHandle <- openFile (sessionDir </> "gemma.jsonl") AppendMode

  pure RecordingHandles
    { rhRawHandle = rawHandle
    , rhGemmaHandle = gemmaHandle
    , rhSessionDir = sessionDir
    }

-- | Record a training example to both output files.
--
-- Writes:
-- - Raw Anthropic response to anthropic.jsonl (one JSON object per line)
-- - Converted FunctionGemma format to gemma.jsonl (one JSONL line)
--
-- Both files are flushed after each write for durability.
recordExample :: RecordingHandles -> TrainingExample -> IO ()
recordExample RecordingHandles{..} TrainingExample{..} = do
  -- Write raw Anthropic response (no pretty printing)
  BL.hPut rhRawHandle (encode teAnthropicRaw)
  hPutChar rhRawHandle '\n'
  hFlush rhRawHandle

  -- Write converted FunctionGemma JSONL
  TIO.hPutStrLn rhGemmaHandle teFunctionGemmaFormatted
  hFlush rhGemmaHandle

-- | Close recording handles.
--
-- Should be called when the teaching session ends to ensure all
-- data is flushed and handles are properly closed.
closeRecording :: RecordingHandles -> IO ()
closeRecording RecordingHandles{..} = do
  hClose rhRawHandle
  hClose rhGemmaHandle


-- | Record a teaching turn (LLM-level capture).
--
-- This is the new LLM-level recording that captures full turns with
-- node context. Writes to anthropic.jsonl with node metadata.
recordTurn :: RecordingHandles -> TeachingTurn -> IO ()
recordTurn RecordingHandles{..} turn = do
  -- Write turn as JSONL (includes node metadata)
  BL.hPut rhRawHandle (encode turn)
  hPutChar rhRawHandle '\n'
  hFlush rhRawHandle


-- | Write session metadata to metadata.json.
--
-- Should be called after session initialization to record:
-- - Session ID
-- - Configuration
-- - Start timestamp
-- - Version info
writeMetadata :: FilePath -> TeachingConfig -> IO ()
writeMetadata sessionDir TeachingConfig{..} = do
  now <- getCurrentTime
  let metadata = object
        [ "sessionId" .= UUID.toString tcSessionId
        , "outputDir" .= tcOutputDir
        , "enabled" .= tcEnabled
        , "startTime" .= now
        , "version" .= ("0.1.0" :: Text)
        ]
  BL.writeFile (sessionDir </> "metadata.json") (encode metadata)
