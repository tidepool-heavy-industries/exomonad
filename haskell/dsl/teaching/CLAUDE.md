# tidepool-teaching

Generic teaching infrastructure for generating FunctionGemma training data via Haiku.

## Architecture Overview

This package implements **knowledge distillation** from Claude 3.5 Haiku (200B params) to FunctionGemma 270M. Instead of synthesizing training data, we run actual production workflows with Haiku as the execution backend, recording its reasoning and tool use.

See [ADR 001](../../../decisions/001-haiku-driven-functiongemma-teaching.md) for the full rationale.

## Core Principles

1. **Task-oriented, not roleplay** - Haiku executes real tasks, not simulations
2. **One tool, two backends** - Production uses FunctionGemma (fast, local), teaching uses Haiku (smart, recorded)
3. **Reasoning as first-class data** - Preserve Haiku's chain-of-thought in training examples
4. **All-or-nothing teaching mode** - Global flag controls whether all tools use teaching mode

## Module Overview

### `Tidepool.Teaching.Types`

Core data types:

- **`TrainingExample`** - A training example pair:
  - Raw Anthropic Messages API response (for offline iteration)
  - Converted FunctionGemma JSONL line (ready for training)
  - Teacher guidance + metadata

- **`TeachingConfig`** - Global teaching session configuration:
  - `tcEnabled :: Bool` - Master switch (True = Haiku, False = FunctionGemma)
  - `tcOutputDir :: FilePath` - Where to write training data
  - `tcSessionId :: UUID` - Unique session identifier
  - `tcAnthropicKey :: Text` - Anthropic API key

- **`RecordingHandles`** - File handles for dual-output recording:
  - `anthropic.jsonl` - Raw Anthropic responses
  - `gemma.jsonl` - Converted FunctionGemma training data

### `Tidepool.Teaching.Anthropic`

Re-exports the production Anthropic client with teaching-specific helpers:

- **Re-exports**: `AnthropicTool`, `AnthropicConfig`, `AnthropicResponse`, `ContentBlock`
- **Helper**: `extractTeachingTurn` - Parse response into (reasoning, toolName, toolArgs)

Uses existing `tidepool-llm-interpreter` (already supports Haiku + tool use). Example:

```haskell
import Tidepool.Teaching.Anthropic
import Tidepool.LLM.Interpreter (runLLMComplete, mkLLMEnv)
import Tidepool.Effects.LLMProvider (complete, SAnthropic)

-- Build config with teacher guidance
let cfg = AnthropicConfig
      { acModel = "claude-3-5-haiku-20241022"
      , acMaxTokens = 1024
      , acThinking = ThinkingDisabled
      , acSystemPrompt = Just (baseSystemPrompt <> "\n\n" <> teacherGuidance @TeachGemma)
      }

-- Call Haiku with tools
response <- runM $ runLLMComplete env $ complete SAnthropic cfg prompt (Just tools)

-- Extract turn for conversion
case extractTeachingTurn response of
  Right (reasoning, toolName, args) -> ...
```

### `Tidepool.Teaching.Teacher`

Minimal typeclass for effect-specific guidance:

```haskell
class FineTrainingTeacher effect where
  teacherGuidance :: Text
```

Each effect that supports teaching must provide domain-specific guidance that helps Haiku understand the task. This is appended to `baseSystemPrompt` when calling Haiku.

Example:
```haskell
instance FineTrainingTeacher TeachGemma where
  teacherGuidance = T.unlines
    [ "# Symbol Selection Strategy"
    , ""
    , "When selecting symbols for understanding a topic:"
    , "- Prioritize symbols that break on changes (exhaustive matches, type families)"
    , "- Include core dependencies that explain behavior"
    , "- Avoid primitive types (Int, Text, Maybe, etc.)"
    ]
```

## Usage Pattern

Teaching infrastructure is consumed by other packages in phases:

1. **Anthropic Client** (`Tidepool.Teaching.Anthropic`) - Re-exports production client (Task 02 ✓)
2. **Format Conversion** (`Tidepool.Teaching.Convert`) - Anthropic → FunctionGemma (Task 03)
3. **Recording** (`Tidepool.Teaching.Record`) - Write dual-format output (Task 04)
4. **Execution Wrapper** (`Tidepool.Teaching.Execute`) - Orchestrate 1-3 (Task 05)

This package builds on the existing `tidepool-llm-interpreter` client.

## Integration Points

**Consumed by:**
- `Tidepool.Teaching.Anthropic` - Uses `TeachingConfig` for API calls
- `Tidepool.Teaching.Convert` - Produces `TrainingExample`
- `Tidepool.Teaching.Record` - Uses `RecordingHandles` for I/O
- `Tidepool.Teaching.Execute` - Uses `FineTrainingTeacher` constraint

**Used in:**
- `tidepool-control-server` - CLI integration (`teach-with-haiku` command)
- Effect wrappers - Add `FineTrainingTeacher` instances

## Directory Structure

Teaching sessions write to:
```
.tidepool/training/
├── session-abc123/
│   ├── anthropic.jsonl   # Raw Haiku responses (one JSON per line)
│   ├── gemma.jsonl       # Converted training data (one conversation per line)
│   └── metadata.json     # Session info (timestamp, config)
└── session-def456/
    └── ...
```

## Design Rationale

### Why Dual Output?

Recording both raw Anthropic responses AND converted FunctionGemma format enables:
- **Offline iteration** - Tweak conversion logic without re-running expensive Haiku calls
- **Quality validation** - Compare raw reasoning to converted format
- **Debugging** - Trace issues to specific API responses

### Why Global Teaching Mode?

Instead of per-tool or per-query teaching mode, we use a global flag because:
- **Consistency** - All tools in a session use the same backend
- **Simplicity** - No complex routing or partial teaching
- **Real workflows** - Captures complete multi-tool interactions

### Why Minimal Typeclass?

`FineTrainingTeacher` only requires `teacherGuidance` text because:
- **Separation of concerns** - Guidance is effect-specific, execution is generic
- **No boilerplate** - Tools auto-derive from existing `ToolDef` instances
- **Flexibility** - Easy to add more methods later if needed

## Next Steps

After implementing this package, see:
- `work/02-anthropic-client.md` - HTTP client for Haiku
- `work/03-format-conversion.md` - Anthropic → FunctionGemma converter
- `work/04-recording.md` - Dual-output file I/O
- `work/05-execute-wrapper.md` - Integration orchestration
