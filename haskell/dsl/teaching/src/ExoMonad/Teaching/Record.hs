{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Recording infrastructure for teaching mode.
--
-- Handles file I/O for training data capture:
-- - Session directory creation
-- - JSONL recording of LLM turns
-- - Session metadata
module ExoMonad.Teaching.Record
  ( initRecording,
    recordTurn,
    closeRecording,
    writeMetadata,
  )
where

import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import ExoMonad.Teaching.Types
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (IOMode (..), hClose, hFlush, hPutChar, openFile)

-- | Initialize a recording session.
--
-- Creates a session directory with the structure:
--   .exomonad/training/session-{uuid}/
--     ├── anthropic.jsonl  -- TeachingTurn records (node context + full LLM turns)
--     ├── gemma.jsonl      -- Reserved for FunctionGemma format (not yet implemented)
--     └── metadata.json    -- Session configuration and timestamp
--
-- Both JSONL files are opened in append mode.
initRecording :: FilePath -> UUID -> IO RecordingHandles
initRecording baseDir sessionId = do
  let sessionDir = baseDir </> ("session-" <> UUID.toString sessionId)
  createDirectoryIfMissing True sessionDir

  -- Open file handles in append mode
  rawHandle <- openFile (sessionDir </> "anthropic.jsonl") AppendMode
  gemmaHandle <- openFile (sessionDir </> "gemma.jsonl") AppendMode

  pure
    RecordingHandles
      { rhRawHandle = rawHandle,
        rhGemmaHandle = gemmaHandle,
        rhSessionDir = sessionDir
      }

-- | Record a teaching turn (LLM-level capture).
--
-- Writes the full TeachingTurn to anthropic.jsonl as a single JSON line.
-- Includes node metadata (node name, graph name) plus full request/response.
--
-- Note: gemma.jsonl conversion is not yet implemented. The handle is opened
-- but currently unused, reserved for future FunctionGemma format conversion.
recordTurn :: RecordingHandles -> TeachingTurn -> IO ()
recordTurn RecordingHandles {..} turn = do
  BL.hPut rhRawHandle (encode turn)
  hPutChar rhRawHandle '\n'
  hFlush rhRawHandle

-- | Close recording handles.
--
-- Should be called when the teaching session ends to ensure all
-- data is flushed and handles are properly closed.
closeRecording :: RecordingHandles -> IO ()
closeRecording RecordingHandles {..} = do
  hClose rhRawHandle
  hClose rhGemmaHandle

-- | Write session metadata to metadata.json.
--
-- Should be called after session initialization to record:
-- - Session ID
-- - Configuration
-- - Start timestamp
-- - Version info
writeMetadata :: FilePath -> TeachingConfig -> IO ()
writeMetadata sessionDir TeachingConfig {..} = do
  now <- getCurrentTime
  let metadata =
        object
          [ "sessionId" .= UUID.toString tcSessionId,
            "outputDir" .= tcOutputDir,
            "enabled" .= tcEnabled,
            "startTime" .= now,
            "version" .= ("0.1.0" :: Text)
          ]
  BL.writeFile (sessionDir </> "metadata.json") (encode metadata)
