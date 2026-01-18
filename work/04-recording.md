# Task 04: Recording Infrastructure

**Status:** Pending
**Assignee:** TBD
**Dependencies:** 01-foundation
**Blocks:** 05-execute-wrapper

## Objective

Create dual-output recording infrastructure (raw Anthropic JSON + converted FunctionGemma JSONL).

## Deliverables

### 1. Record.hs Module

Create `haskell/dsl/teaching/src/Tidepool/Teaching/Record.hs`

### 2. Key Functions

```haskell
-- Initialize recording session
initRecording :: FilePath -> UUID -> IO RecordingHandles
-- Creates: .tidepool/training/{sessionId}/
--   ├── anthropic.jsonl
--   ├── gemma.jsonl
--   └── metadata.json

-- Record single example (both formats)
recordExample :: RecordingHandles -> TrainingExample -> IO ()

-- Cleanup (close handles)
closeRecording :: RecordingHandles -> IO ()

-- Write session metadata
writeMetadata :: FilePath -> TeachingConfig -> IO ()
```

### 3. Implementation

```haskell
initRecording :: FilePath -> UUID -> IO RecordingHandles
initRecording baseDir sessionId = do
  let sessionDir = baseDir </> ("session-" <> UUID.toString sessionId)
  createDirectoryIfMissing True sessionDir

  -- Open file handles
  rawHandle <- openFile (sessionDir </> "anthropic.jsonl") AppendMode
  gemmaHandle <- openFile (sessionDir </> "gemma.jsonl") AppendMode

  -- Write metadata
  writeMetadata sessionDir (TeachingConfig {..})

  pure RecordingHandles
    { rhRawHandle = rawHandle
    , rhGemmaHandle = gemmaHandle
    , rhSessionDir = sessionDir
    }

recordExample :: RecordingHandles -> TrainingExample -> IO ()
recordExample handles ex = do
  -- Write raw Anthropic response
  BL.hPutStrLn handles.rhRawHandle (encode ex.teAnthropicRaw)
  hFlush handles.rhRawHandle

  -- Write converted FunctionGemma JSONL
  TIO.hPutStrLn handles.rhGemmaHandle ex.teFunctionGemmaFormatted
  hFlush handles.rhGemmaHandle

closeRecording :: RecordingHandles -> IO ()
closeRecording handles = do
  hClose handles.rhRawHandle
  hClose handles.rhGemmaHandle
```

### 4. Metadata Format

```haskell
writeMetadata :: FilePath -> TeachingConfig -> IO ()
writeMetadata dir config = do
  now <- getCurrentTime
  let meta = object
        [ "sessionId" .= show config.tcSessionId
        , "timestamp" .= now
        , "outputDir" .= config.tcOutputDir
        , "version" .= ("0.1.0" :: Text)
        ]
  BL.writeFile (dir </> "metadata.json") (encode meta)
```

### 5. Directory Structure

```
.tidepool/training/
├── session-abc-123-def/
│   ├── anthropic.jsonl   # Raw Anthropic responses (one per line)
│   ├── gemma.jsonl       # Converted FunctionGemma training data
│   └── metadata.json     # Session info
└── session-xyz-789-ghi/
    ├── ...
```

## Acceptance Criteria

- [ ] Builds: `cabal build tidepool-teaching`
- [ ] `initRecording` creates directory + files
- [ ] `recordExample` writes to both files
- [ ] Files are line-buffered (flushed after each write)
- [ ] `closeRecording` closes handles cleanly
- [ ] Metadata includes session ID and timestamp
- [ ] Unit tests pass (see below)

## Testing

Create `test/Tidepool/Teaching/RecordSpec.hs`:

```haskell
spec :: Spec
spec = do
  describe "initRecording" $ do
    it "creates session directory and files" $ do
      withSystemTempDirectory "test-recording" $ \tmpDir -> do
        sessionId <- UUID.nextRandom
        handles <- initRecording tmpDir sessionId

        let sessionDir = tmpDir </> ("session-" <> UUID.toString sessionId)
        doesDirectoryExist sessionDir `shouldReturn` True
        doesFileExist (sessionDir </> "anthropic.jsonl") `shouldReturn` True
        doesFileExist (sessionDir </> "gemma.jsonl") `shouldReturn` True
        doesFileExist (sessionDir </> "metadata.json") `shouldReturn` True

        closeRecording handles

  describe "recordExample" $ do
    it "writes to both files" $ do
      withSystemTempDirectory "test-recording" $ \tmpDir -> do
        sessionId <- UUID.nextRandom
        handles <- initRecording tmpDir sessionId

        now <- getCurrentTime
        let example = TrainingExample
              { teAnthropicRaw = object ["test" .= ("value" :: Text)]
              , teFunctionGemmaFormatted = "gemma line"
              , teTeacherGuidance = Just "guidance"
              , teTimestamp = now
              , teToolName = "test_tool"
              }

        recordExample handles example

        closeRecording handles

        -- Verify files contain data
        let sessionDir = tmpDir </> ("session-" <> UUID.toString sessionId)
        anthropicContent <- readFile (sessionDir </> "anthropic.jsonl")
        anthropicContent `shouldContain` "\"test\""

        gemmaContent <- readFile (sessionDir </> "gemma.jsonl")
        gemmaContent `shouldContain` "gemma line"
```

## Dependencies

Add to `tidepool-teaching.cabal`:
```
build-depends:
  , directory
  , filepath
```

Add to test suite:
```
build-depends:
  , temporary    -- withSystemTempDirectory
```

## Integration Point

Consumed by Task 05 (Execute.hs):
```haskell
-- Initialize once per session
handles <- sendM $ initRecording config.tcOutputDir config.tcSessionId

-- Record each example
sendM $ recordExample handles example

-- Cleanup at end
sendM $ closeRecording handles
```

## Estimated Effort

2-3 hours (file I/O + tests)

## Notes

- Use `hFlush` after each write (durability)
- JSONL = one JSON object per line (no pretty printing)
- Session IDs are UUIDs (globally unique)
- Metadata helps with debugging/auditing sessions
- Consider adding `--latest` symlink pointing to most recent session
