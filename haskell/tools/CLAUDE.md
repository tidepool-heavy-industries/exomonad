# Standalone Tools

Developer tools that run as separate processes, not linked into main Tidepool binaries.

## When to Read Which CLAUDE.md

| I want to... | Read this |
|--------------|-----------|
| Query Haskell types from an agent | `ghci-oracle/CLAUDE.md` |
| Analyze agent logs for evolution | `sleeptime/CLAUDE.md` |
| Generate training data for FunctionGemma | `training-generator/CLAUDE.md` |

## Documentation Tree

```
tools/CLAUDE.md  ← YOU ARE HERE (router)
├── ghci-oracle/CLAUDE.md     ← GHCi subprocess server (detailed)
├── sleeptime/CLAUDE.md       ← Log analysis for agent evolution
└── training-generator/CLAUDE.md ← FunctionGemma JSONL training data
```

## Structure

| Package | Purpose | Runs as |
|---------|---------|---------|
| `ghci-oracle/` | Persistent GHCi session server | Standalone binary (socket server) |
| `sleeptime/` | Log analysis for agent improvement | Library + CLI |
| `training-generator/` | JSONL training data for FunctionGemma | Library + CLI |

## ghci-oracle

Maintains a persistent GHCi REPL session, exposing it via TCP socket:

```bash
# Start server
cd tools/ghci-oracle
cabal run ghci-oracle -- --port 9999 --project /path/to/project

# Clients use tidepool-ghci-interpreter effect to query types
runGHCiIO conn $ queryType "fmap"  -- Returns "(a -> b) -> f a -> f b"
```

**Why standalone?** GHCi dependencies are heavy. Keeping this separate:
- Avoids polluting main build with GHC internals
- Isolates crashes from the main application
- Enables independent deployment

See `ghci-oracle/CLAUDE.md` for protocol details.

## sleeptime

Log analysis for agent evolution (the "cron job" pattern):

```bash
# Analyze agent run logs
sleeptime analyze --log-dir ./logs --output report.json
```

**Purpose**: Observes agent runs, identifies improvement opportunities, files issues/PRs.

**Note**: The actual cron jobs live in consuming repos (anemone, urchin), not here.

See `sleeptime/CLAUDE.md` for analysis patterns.

## training-generator

Generates JSONL training data for FunctionGemma 270M fine-tuning:

```bash
# Generate 1000 training examples
training-generator 1000 > training.jsonl
```

**Purpose**: Generates edge scoring training data in 2-turn minimal format:
- `ScoreEdgeInput` - Edge context (query, source, target, hover info)
- `ScoreEdgeOutput` - Scoring rubric (relevance, risk, boolean flags)
- `EdgeTrainingExample` - Input/output pairs for supervised learning

**Wire format**: 2-turn minimal (user context → model call). Schema baked into weights.

**Also exports** legacy types used by semantic-scout:
- `Tag`, `Rubric`, `QueryContext`, `NodeContext`, `TrainingExample`

See `training-generator/CLAUDE.md` for data model details.

## Why Not in Main Build?

These tools have different dependency profiles:
- **ghci-oracle**: Heavy GHC API dependencies
- **sleeptime**: Analysis libraries, different optimization needs

Keeping them separate avoids dependency conflicts and bloat in the main build.

## Related Documentation

- [effects/ghci-interpreter/CLAUDE.md](../effects/ghci-interpreter/CLAUDE.md) - Client that connects to ghci-oracle
- [Root CLAUDE.md](../../CLAUDE.md) - Project overview with sleeptime context
