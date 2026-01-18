# Task 07: CLI Integration

**Status:** Complete
**Assignee:** Claude Sonnet 4.5
**Dependencies:** 06-scout-integration
**Blocks:** None (final task)

## Objective

Add `teach-with-haiku` CLI subcommand to control-server and wire teaching config through MCP handler.

## Deliverables

### 1. Update Main.hs

Modify `haskell/control-server/src/Main.hs`

Add command type:
```haskell
data Command
  = Serve ServerConfig
  | TeachWithHaiku TeachConfig  -- NEW

data TeachConfig = TeachConfig
  { teachOutputDir :: FilePath
  , teachAnthropicKey :: Text
  } deriving (Show, Eq)
```

Add parser:
```haskell
parseCommand :: Parser Command
parseCommand = hsubparser
  (  command "serve"
       (info (Serve <$> serverOpts) (progDesc "Run production server"))
  <> command "teach-with-haiku"
       (info (TeachWithHaiku <$> teachOpts) (progDesc "Run in teaching mode (records Haiku responses)"))
  )

teachOpts :: Parser TeachConfig
teachOpts = TeachConfig
  <$> strOption
      (  long "output-dir"
      <> metavar "DIR"
      <> value ".tidepool/training"
      <> showDefault
      <> help "Output directory for training data"
      )
  <*> strOption
      (  long "anthropic-key"
      <> metavar "KEY"
      <> help "Anthropic API key (or set ANTHROPIC_API_KEY env var)"
      )
```

Update main:
```haskell
main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Serve config -> runServer config
    TeachWithHaiku teachCfg -> runServerWithTeaching teachCfg

runServerWithTeaching :: TeachConfig -> IO ()
runServerWithTeaching teachCfg = do
  -- Similar to runServer but pass teaching config through
  ...
```

### 2. Update Handler/MCP.hs

Modify `haskell/control-server/src/Tidepool/Control/Handler/MCP.hs`

Thread teaching config:
```haskell
-- Update signature
handleMcpTool :: LSPSession -> Text -> Text -> Value -> Maybe TeachingConfig -> IO ControlResponse

-- Update teach handler
handleTeachTool :: LSPSession -> Text -> Value -> Maybe TeachingConfig -> IO ControlResponse
handleTeachTool lspSession reqId args maybeTeachConfig = do
  case fromJSON args of
    Error err -> pure $ mcpToolError reqId $ "Invalid teach arguments: " <> T.pack err
    Success query -> do
      -- Initialize teaching session if enabled
      (config, maybeHandles) <- case maybeTeachConfig of
        Nothing -> do
          -- Production mode
          let config = TeachingConfig
                { tcEnabled = False
                , tcOutputDir = ""
                , tcSessionId = nil
                , tcAnthropicKey = ""
                }
          pure (config, Nothing)

        Just teachCfg -> do
          -- Teaching mode
          sessionId <- UUID.nextRandom
          handles <- initRecording teachCfg.teachOutputDir sessionId
          let config = TeachingConfig
                { tcEnabled = True
                , tcOutputDir = teachCfg.teachOutputDir
                , tcSessionId = sessionId
                , tcAnthropicKey = teachCfg.teachAnthropicKey
                }
          pure (config, Just handles)

      -- Run exploration (teaching wrapper is implicit in tool execution)
      maybeEndpoint <- lookupEnv "GEMMA_ENDPOINT"
      case maybeEndpoint of
        Nothing -> pure $ mcpToolError reqId "GEMMA_ENDPOINT not set"
        Just ep -> do
          result <- try $ runM $ runLog Debug $ runTeachGemmaHTTP (T.pack ep) $ runLSP lspSession $
            teach defaultTeachConfig query

          -- Cleanup
          case maybeHandles of
            Just handles -> closeRecording handles
            Nothing -> pure ()

          case result of
            Left (e :: SomeException) -> pure $ mcpToolError reqId $ T.pack $ show e
            Right doc -> pure $ mcpToolSuccess reqId (toJSON doc)
```

### 3. Wire Teaching Through Server

Update server initialization to carry teaching config:
```haskell
-- In Server.hs or wherever the server loop is
runServer :: Maybe TeachingConfig -> IO ()
runServer maybeTeachConfig = do
  -- ... existing setup ...

  -- Pass teaching config to handler
  forever $ do
    (conn, _) <- accept sock
    forkIO $ handleConnection lspSession maybeTeachConfig conn
```

## Acceptance Criteria

- [ ] Builds: `cabal build tidepool-control-server`
- [ ] `--help` shows both commands
- [ ] `serve` command works (production mode)
- [ ] `teach-with-haiku` command accepts flags
- [ ] Teaching config threads through to tool execution
- [ ] E2E integration test passes (see below)

## Testing

### Manual E2E Test

```bash
# 1. Start control-server in teaching mode
cd /home/inanna/tidepool-heavy-industries/haiku-training-data-synthesis-woktree
export ANTHROPIC_API_KEY=sk-ant-...
cabal run tidepool-control-server -- teach-with-haiku \
  --output-dir ./test-training \
  --anthropic-key $ANTHROPIC_API_KEY &

# 2. Send teach request via MCP
echo '{"type":"MCPToolCall","id":"1","tool_name":"teach","arguments":{"topic":"test","seeds":["compositeScore"],"budget":5}}' | \
  socat - UNIX-CONNECT:.tidepool/control.sock

# 3. Verify output files created
ls -la ./test-training/session-*/
cat ./test-training/session-*/anthropic.jsonl | jq .
cat ./test-training/session-*/gemma.jsonl | jq -r .text

# 4. Validate FunctionGemma format
grep "<start_of_turn>model" ./test-training/session-*/gemma.jsonl
grep "call:select_symbols" ./test-training/session-*/gemma.jsonl

# 5. Check reasoning preserved
jq -r '.content[] | select(.type == "text") | .text' \
  ./test-training/session-*/anthropic.jsonl
```

### Automated Integration Test

Create `test/Integration/TeachingSpec.hs`:
```haskell
spec :: Spec
spec = do
  describe "teach-with-haiku E2E" $ do
    it "records Haiku responses during exploration" $ do
      withSystemTempDirectory "test-teaching" $ \tmpDir -> do
        -- Start server in background
        -- Send MCP request
        -- Verify outputs
        -- (Implementation requires spawning subprocess)
        pending "Requires subprocess spawning"
```

## Documentation Updates

### 1. Update control-server/CLAUDE.md

Add section on teaching mode:
```markdown
## Teaching Mode

Generate training data for FunctionGemma fine-tuning:

```bash
cabal run tidepool-control-server -- teach-with-haiku \
  --output-dir .tidepool/training \
  --anthropic-key $ANTHROPIC_API_KEY
```

All tool executions will:
1. Call Haiku instead of FunctionGemma
2. Record raw Anthropic responses to `anthropic.jsonl`
3. Convert to FunctionGemma format in `gemma.jsonl`
4. Continue execution normally (for testing)

Output: `.tidepool/training/session-{uuid}/`
```

### 2. Update haskell/dsl/teaching/CLAUDE.md

Add usage section showing CLI integration.

## Estimated Effort

2-3 hours (CLI wiring + testing)

## Notes

- Teaching config should be optional (Maybe TeachingConfig)
- Production mode = Nothing
- Teaching mode = Just config
- Thread config through all relevant handlers
- Consider global IORef for config (alternative to threading)
- Make sure API key is not logged/printed
