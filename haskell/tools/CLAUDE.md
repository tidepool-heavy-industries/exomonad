# Standalone Tools

Developer tools that run as separate processes.

## Packages

| Package | Purpose |
|---------|---------|
| `training-generator/` | JSONL training data for FunctionGemma |

## training-generator

Generates JSONL training data for FunctionGemma 270M fine-tuning:

```bash
# Generate 1000 training examples
cabal run training-generator -- 1000 > training.jsonl
```

**Purpose**: Generates edge scoring training data in 2-turn minimal format:
- `ScoreEdgeInput` - Edge context (query, source, target, hover info)
- `ScoreEdgeOutput` - Scoring rubric (relevance, risk, boolean flags)
- `EdgeTrainingExample` - Input/output pairs for supervised learning

See `training-generator/CLAUDE.md` for data model details.
