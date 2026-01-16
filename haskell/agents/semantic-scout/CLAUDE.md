# Semantic Scout - Code Exploration Engine

**IMPORTANT: This package has been merged into `haskell/control-server/`.**

The semantic-scout agent is now exposed as the `scout` MCP tool via the control server. The code in this directory is a reference/archive.

## Current Location

See `haskell/control-server/CLAUDE.md` for the active implementation.

The scout tool is implemented as:
- **Handler**: `haskell/control-server/src/Tidepool/Control/Handler/MCP.hs`
- **Exploration**: `haskell/control-server/src/Tidepool/Control/Scout/Explore.hs`
- **Scoring**: `haskell/control-server/src/Tidepool/Control/Scout/Gemma.hs`
- **Types**: `haskell/control-server/src/Tidepool/Control/Scout/Types.hs`

## What It Does

Semantic Scout answers natural language questions about codebases using LSP (Language Server Protocol) and heuristic/ML-based scoring.

**Example query:**
```
"What breaks if I add a variant to LLMKind?"
→ Returns:
  - Pattern match locations (risk: 5, relevance: 4)
  - Type family definitions (risk: 4, relevance: 5)
  - Exhaustive case analysis sites (risk: 5, relevance: 3)
  - Suggested actions for each location
```

## Architecture

```
Claude Code (human TTY)
    │
    └─ MCP tool "scout" ──► mantle-agent mcp ──► control-server (Haskell)
                                                       │
                                                       ├─ LSP (HLS)
                                                       ├─ Gemma scoring
                                                       └─ BFS exploration
```

## Key Components

| Component | Location | Purpose |
|-----------|----------|---------|
| **MCP Handler** | `control-server/Handler/MCP.hs` | Receives scout calls, runs exploration |
| **Exploration Loop** | `control-server/Scout/Explore.hs` | BFS with LSP + Gemma scoring |
| **LSP Integration** | `effects/lsp-interpreter/` | HLS communication via lsp-test |
| **Scoring** | `control-server/Scout/Gemma.hs` | Pluggable scorer (heuristic/HTTP) |
| **Training Data** | `tools/training-generator/` | Types + JSONL formatting |

## Data Flow

1. **User query** → Claude Code MCP call
2. **mantle-agent mcp** → TCP to control-server
3. **control-server** → handleMcpTool "scout"
4. **Exploration**:
   - Find entry points via LSP workspace/symbol
   - BFS expansion via LSP references
   - Score each node with Gemma effect
   - Collect high-risk/relevance pointers
5. **Response** → ScoutResponse with pointers + training examples

## Scoring: Gemma Effect

The `Gemma` effect decouples scoring from exploration:

```haskell
data Gemma a where
  RateEdge :: Text -> EdgeContext -> Gemma Rubric
  RateNode :: QueryContext -> NodeContext -> Gemma Rubric
```

### Interpreters

| Interpreter | Status | Purpose |
|-------------|--------|---------|
| `runGemmaHeuristic` | ✅ Active | Pattern-based rules (baseline) |
| `runGemmaHTTP` | ✅ Implemented | Call mistralrs-server for FunctionGemma 270M |
| `runGemmaMock` | ✅ Available | Hardcoded rubrics (testing) |

The HTTP interpreter calls a local FunctionGemma model via `GEMMA_ENDPOINT` environment variable.

## Training Data

Exploration generates training examples during each run:

```haskell
data TrainingExample = TrainingExample
  { teQuery  :: QueryContext
  , teNode   :: NodeContext
  , teRubric :: Rubric
  }
```

These are included in `ScoutResponse.srTrainingExamples` and can be exported for fine-tuning.

See `haskell/tools/training-generator/CLAUDE.md` for JSONL format details.

## Running

The scout tool runs inside the control-server:

```bash
# Start control server (includes scout MCP tool)
cd /path/to/project
cabal run tidepool-control-server

# Claude Code calls it via:
# Tool: scout
# Arguments: { query: "...", symbols: [...], depth: "medium" }
```

## Related Documentation

- **[haskell/control-server/CLAUDE.md](../../control-server/CLAUDE.md)** - Main implementation (current)
- **[haskell/effects/lsp-interpreter/CLAUDE.md](../../effects/lsp-interpreter/CLAUDE.md)** - LSP integration
- **[haskell/tools/training-generator/CLAUDE.md](../../tools/training-generator/CLAUDE.md)** - Training data format
- **[rust/mantle-agent/CLAUDE.md](../../../rust/mantle-agent/CLAUDE.md)** - MCP forwarding
- **[Root CLAUDE.md](../../../CLAUDE.md)** - Project overview

## Migration Notes

**Why merged into control-server?**

1. **Session sharing**: Both hook handling and scout need long-lived LSP sessions
2. **Simplified deployment**: One binary instead of two
3. **Shared types**: Scout types used by both MCP tools and hooks
4. **Reduced IPC**: No need for separate MCP server process

The standalone semantic-scout binary has been removed. All scout functionality is now part of `tidepool-control-server`.
