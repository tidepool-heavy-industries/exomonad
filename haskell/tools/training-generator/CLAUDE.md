# Training Generator - FunctionGemma Fine-Tuning Data

Generates JSONL training data for fine-tuning FunctionGemma 270M as a semantic edge scorer. Used by agents and consuming repos to replace heuristics with learned models.

## When to Read This

Read this if you're:
- Generating datasets for FunctionGemma fine-tuning
- Working on agent training data generation
- Understanding the heuristics → ML migration path

## What It Does

Generates training examples in **2-turn minimal format** for edge scoring:

```
Turn 1 (user):  Edge context (query, source, target, hover info)
Turn 2 (model): Function call with scoring rubric
```

Schema is "baked" into model weights via fine-tuning - no schema turn needed.

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ training-generator 1000 > training.jsonl                            │
│   • Generates synthetic edge contexts via QuickCheck                │
│   • Scores each edge with heuristics                                │
│   • Outputs JSONL in 2-turn format                                  │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ JSONL
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ Training Pipeline (external)                                        │
│   • Converts JSONL → TRL/Unsloth format                             │
│   • Fine-tunes FunctionGemma 270M                                   │
│   • Bakes schema into weights                                       │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ Model
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ Agent (Semantic Scout Module)                                       │
│   • HTTP to Ollama (port 11434) with OpenAI-style tools array       │
│   • Ollama auto-translates → FunctionGemma token format             │
│   • Response in message.tool_calls[0].function.arguments            │
└─────────────────────────────────────────────────────────────────────┘
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `Types.hs` | Core data types (ScoreEdgeInput, ScoreEdgeOutput, EdgeTrainingExample) |
| `Arbitrary.hs` | QuickCheck generators + heuristic scoring |
| `Format.hs` | 2-turn minimal wire format |

## Data Model

### ScoreEdgeInput (Flat)
```haskell
data ScoreEdgeInput = ScoreEdgeInput
  { seiQuery       :: Text      -- Natural language query
  , seiSourceFile  :: Text      -- Source location file
  , seiSourceLine  :: Int       -- Source location line
  , seiSourceHover :: Text      -- Hover info at source
  , seiTargetFile  :: Text      -- Target location file
  , seiTargetLine  :: Int       -- Target location line
  , seiTargetHover :: Text      -- Hover info at target
  , seiEdgeType    :: EdgeType  -- Definition, Reference, etc.
  }
```

### ScoreEdgeOutput (Flat)
```haskell
data ScoreEdgeOutput = ScoreEdgeOutput
  { seoRelevance    :: Int   -- 1-5: How relevant to query
  , seoRisk         :: Int   -- 1-5: How risky to modify
  , seoReasoning    :: Text  -- Natural language justification
  , seoIsExhaustive :: Bool  -- Pattern match exhaustiveness
  , seoIsTypeFamily :: Bool  -- Type-level computation
  , seoIsExported   :: Bool  -- Public API surface
  }
```

**Note:** Tags flattened to individual booleans for 270M model accuracy.

### EdgeType Enum
```haskell
data EdgeType
  = Definition      -- Go-to-definition edge
  | Reference       -- Find-references edge
  | Usage           -- Usage/call site
  | Instance        -- Typeclass instance implementation
  | TypeConstraint  -- Typeclass constraint
```

## Usage

```bash
# Generate 1000 training examples
training-generator 1000 > training.jsonl

# Show help
training-generator --help
```

## Wire Format (2-Turn Minimal)

Each JSONL line contains a single "text" field with the conversation:

```json
{"text":"<start_of_turn>user\nScore this edge:\nQuery: <escape>What breaks if I add a variant?<escape>\nSource: <escape>Types.hs:42<escape>\nSource hover: <escape>data LLMKind = ...<escape>\nTarget: <escape>Handler.hs:87<escape>\nTarget hover: <escape>case x of...<escape>\nEdge type: <escape>reference<escape>\n<end_of_turn>\n<start_of_turn>model\n<start_function_call>call:score_edge{relevance:5,risk:4,reasoning:<escape>Pattern match...<escape>,is_exhaustive:true,is_type_family:false,is_exported:true}<end_function_call>\n<end_of_turn>"}
```

### Control Tokens

| Token | Required | Purpose |
|-------|----------|---------|
| `<start_of_turn>` / `<end_of_turn>` | Yes | Role boundaries, stop signals |
| `<start_function_call>` / `<end_function_call>` | Yes | Parser locates structured output |
| `<escape>` | Yes | String delimiters (Haskell code has brackets/commas) |

## Heuristic Scoring

The `heuristicScoreEdge` function in Arbitrary.hs scores edges based on:
- Query keyword matching ("breaks", "uses", "type family")
- Edge type correlation (Definition for "where is X")
- Pattern match detection in hover info
- Type family detection
- Export heuristics (file path, underscore prefix)

This bootstraps training data - the fine-tuned model learns to generalize.

## Shared Types

**Edge types** (used by agents for semantic edge scoring):
- `ScoreEdgeInput` - Input to Ollama: query + source/target locations + hover info
- `ScoreEdgeOutput` - Output from Ollama: relevance, risk, reasoning, boolean flags
- `EdgeType` - Edge classification (Definition, Reference, Usage, Instance, TypeConstraint)

**Legacy types** (still exported for compatibility):
- `Tag`, `Rubric`, `QueryContext`, `NodeContext`, `TrainingExample`

**JSON field mapping:** `ScoreEdgeOutput` uses custom JSON instances to map Haskell camelCase (`seoRelevance`) to Ollama's snake_case (`relevance`).

## Related Documentation

- [tools/CLAUDE.md](../CLAUDE.md) - Tools overview
- [Root CLAUDE.md](../../../CLAUDE.md) - Project overview
