# tidepool-teaching

LLM-level teaching infrastructure for generating FunctionGemma training data via Haiku.

## Status: Implemented, Not Wired

The teaching infrastructure is complete but **not yet wired into any server**. To use it, a server would need to:
1. Call `loadTeachingConfig` to check for env vars
2. Use `runLLMWithTeaching` instead of production LLM interpreter when enabled

## Architecture Overview

This package implements **knowledge distillation** from Claude 3.5 Haiku to FunctionGemma 270M. Teaching happens at the **LLM effect level** (not individual tools), capturing full multi-turn conversations with node context.

```
Graph Node (e.g., gClassify)
    │
    ▼ (runTurn called with NodeMeta context)
RunTurnOp meta systemPrompt userContent schema tools
    │
    ▼ (runLLMWithTeaching intercepts)
┌────────────────────────────────────────────┐
│ Teaching Interpreter                        │
│  1. Wrap system prompt with guidance        │
│  2. Call Haiku via Anthropic API            │
│  3. Record TeachingTurn (full context)      │
│  4. Return TurnOutcome                      │
└────────────────────────────────────────────┘
    │
    ▼
anthropic.jsonl  (TeachingTurn records)
```

## Module Overview

### `Tidepool.Teaching.Types`

Core data types:

- **`TeachingConfig`** - Global teaching session configuration:
  ```haskell
  data TeachingConfig = TeachingConfig
    { tcEnabled :: Bool           -- Master switch
    , tcOutputDir :: FilePath     -- Where to write training data
    , tcSessionId :: UUID         -- Unique session identifier
    , tcAnthropicKey :: Text      -- Anthropic API key for Haiku calls
    }
  ```

- **`TeachingEnv`** - Runtime environment for teaching sessions:
  ```haskell
  data TeachingEnv = TeachingEnv
    { teConfig :: TeachingConfig  -- Session configuration
    , teHandles :: RecordingHandles -- Open file handles
    , teGuidance :: Text          -- Teacher guidance to prepend
    }
  ```

- **`TeachingTurn`** - A captured LLM turn for training data:
  ```haskell
  data TeachingTurn = TeachingTurn
    { ttNodeName :: Text       -- e.g., "gClassify"
    , ttGraphName :: Text      -- e.g., "SupportGraph"
    , ttSystemPrompt :: Text   -- Original system prompt
    , ttUserContent :: Value   -- User content blocks (JSON)
    , ttOutputSchema :: Value  -- Structured output schema
    , ttToolDefs :: [Value]    -- Tool definitions provided
    , ttResponse :: Value      -- Full Haiku response (JSON)
    , ttTimestamp :: UTCTime   -- When executed
    }
  ```

- **`RecordingHandles`** - Dual-output file handles:
  ```haskell
  data RecordingHandles = RecordingHandles
    { rhRawHandle :: Handle    -- anthropic.jsonl
    , rhGemmaHandle :: Handle  -- gemma.jsonl (reserved, not yet written)
    , rhSessionDir :: FilePath -- Session directory path
    }
  ```

### `Tidepool.Teaching.Teacher`

Minimal typeclass for effect-specific guidance:

```haskell
class FineTrainingTeacher effect where
  teacherGuidance :: Text

-- Base prompt for all teaching sessions
baseSystemPrompt :: Text
```

### `Tidepool.Teaching.Record`

File I/O for training data:

- `initRecording` - Create session directory, open handles
- `recordTurn` - Write TeachingTurn to anthropic.jsonl
- `closeRecording` - Flush and close handles
- `writeMetadata` - Write session metadata.json

### `Tidepool.Teaching.LLM`

Teaching LLM interpreter:

- `runLLMWithTeaching` - Interpreter that intercepts `RunTurnOp`, calls Haiku, records turns
- `withTeaching` - Bracket-style session management
- `loadTeachingConfig` - Load config from environment variables
- `initTeachingEnv` / `closeTeachingEnv` - Manual lifecycle management

## Usage

### Environment Variables

```bash
export TEACHING_ENABLED=true
export ANTHROPIC_API_KEY=sk-ant-...
export TEACHING_OUTPUT_DIR=.tidepool/training  # optional, default shown
```

### Programmatic Usage

```haskell
import Tidepool.Teaching.LLM

main :: IO ()
main = do
  mConfig <- loadTeachingConfig
  case mConfig of
    Nothing -> runProductionMode  -- Normal execution
    Just config -> do
      let guidance = teacherGuidance @MyEffect
      withTeaching config guidance $ \env -> do
        -- Use runLLMWithTeaching instead of production interpreter
        result <- runM $ runLLMWithTeaching env $ myGraph
        pure result
```

## Output Format

Teaching sessions write to:
```
.tidepool/training/
└── session-{uuid}/
    ├── anthropic.jsonl   # TeachingTurn records (one JSON per line)
    ├── gemma.jsonl       # Reserved for FunctionGemma format (not yet implemented)
    └── metadata.json     # Session info (timestamp, config, version)
```

## Known Limitations

1. **Not wired to server** - No server currently calls `runLLMWithTeaching`
2. **gemma.jsonl not written** - Handle is opened but FunctionGemma conversion not implemented
3. **Thinking content lost** - `ThinkingEnabled 1024` is set but `trThinking = ""` in output
4. **Single-threaded** - Interpreter is not thread-safe

## Dependencies

- `tidepool-core` - Effect types, NodeMeta
- `tidepool-llm-interpreter` - Anthropic API client

## Integration Points

**Consumed by:**
- Future server wiring (would replace production LLM interpreter)

**Depends on:**
- `Tidepool.Effect.NodeMeta` - Provides node/graph context at dispatch time
- `Tidepool.Effects.LLMProvider` - Anthropic API types
- `Tidepool.LLM.Interpreter` - HTTP client for Haiku calls
