# Semantic Scout - Code Exploration MCP Tool

A Haskell agent that answers semantic questions about codebases using LSP (Language Server Protocol). The first production implementation of **a Tidepool LLMNode exposed as an MCP tool**.

## When to Read This

Read this if you're:
- Understanding how semantic code exploration works
- Working on the semantic-scout MCP integration
- Implementing new MCP tools from Tidepool agents
- Debugging LSP-based code analysis
- Working with FunctionGemma scoring/training

## What It Does

Semantic Scout takes a natural language query about code and returns actionable pointers to relevant locations:

```bash
# As MCP tool (for Claude Code)
semantic-scout --mcp

# Demo mode with mock data
semantic-scout --explore

# Generate training examples for FunctionGemma
semantic-scout --gen-training 100
```

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
┌─────────────────────────────────────────────────────────────────────┐
│ Claude Code                                                          │
│   "What breaks if I add a variant to LLMKind?"                      │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ MCP tools/call
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ semantic-scout --mcp (Main.hs)                                      │
│   • MCP stdio server                                                │
│   • Wraps exploreEff as MCP tool via makeMcpTool                    │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │
                ┌──────────────────────┼──────────────────────┐
                ▼                      ▼                      ▼
       ┌────────────────┐    ┌────────────────┐    ┌────────────────┐
       │ LSP Effect     │    │ Gemma Effect   │    │ Heuristics     │
       │ (Interpreter)  │    │ (Scorer)       │    │ (Decision)     │
       └────────┬───────┘    └────────┬───────┘    └────────┬───────┘
                │                     │                     │
                ▼                     ▼                     ▼
       HLS (workspace/symbol,  FunctionGemma 270M    shouldExpand
            hover, references)  (or heuristics)      (prune/expand)
                │                     │                     │
                └──────────────┬──────┴─────────────────────┘
                               ▼
                    ┌──────────────────────┐
                    │ Explore.exploreEff   │
                    │ (BFS with budget)    │
                    └──────────────────────┘
                               │
                               ▼
                    ┌──────────────────────┐
                    │ ScoutResponse        │
                    │ • Summary            │
                    │ • Pointers           │
                    │ • Training examples  │
                    └──────────────────────┘
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `Main.hs` | MCP server entry point, wires LSP session into tool |
| `Explore.hs` | BFS exploration loop (pure + effectful versions) |
| `Types.hs` | ScoutQuery, ScoutResponse, Pointer data types |
| `Gemma.hs` | Pluggable scorer effect + interpreters |
| `Heuristics.hs` | Decision rules for expansion and scoring |
| `LSP.hs` | LSP integration helpers (makeNodeContext, etc.) |

## Data Model

### Input: ScoutQuery
```haskell
data ScoutQuery = ScoutQuery
  { query  :: Text           -- Natural language question
  , tags   :: [Tag]          -- What kind of changes break code
  , budget :: Maybe Int      -- Max nodes to explore (default: 20)
  }

data Tag
  = Exhaustive      -- Changes break exhaustive pattern matches
  | PatternMatch    -- Changes break specific pattern matches
  | TypeFamily      -- Changes affect type families
  | Constraint      -- Changes affect typeclass constraints
  | ...
```

### Output: ScoutResponse
```haskell
data ScoutResponse = ScoutResponse
  { summary          :: Text
  , pointers         :: [Pointer]
  , nodesVisited     :: Int
  , trainingExamples :: [TrainingExample]
  }

data Pointer = Pointer
  { location  :: Text           -- "File.hs:45"
  , what      :: Text           -- Hover snippet
  , risk      :: Int            -- 1-5: Impact of breaking change
  , relevance :: Int            -- 1-5: Relevance to query
  , action    :: Text           -- Suggested next step
  }
```

## Exploration Algorithm

### Pure Version (explore)
```haskell
explore
  :: Monad m
  => Scorer m           -- Abstract scoring function
  -> QueryContext       -- User query + tags
  -> [NodeId]           -- Entry points
  -> Budget             -- Node exploration limit
  -> m [ScoredNode]     -- Results with scores
```

Uses abstract `Scorer` type for testing with mock data.

### Effectful Version (exploreEff)
```haskell
exploreEff
  :: (Member LSP effs, Member Gemma effs)
  => ScoutQuery
  -> Eff effs ScoutResponse
```

Implements the BFS loop with real effects:

1. **Find entry points**: `workspaceSymbol` search based on query
2. **Fetch context**: For each node, `hover` + file snippet
3. **Score node**: `Gemma` effect rates relevance + risk
4. **Decide expansion**: `shouldExpand` checks rubric + budget + depth
5. **Queue children**: `references` for BFS expansion
6. **Collect results**: Sort by relevance, return top pointers

### Budget Management
- **Node limit**: Max nodes to visit (default: 20)
- **Depth limit**: Max BFS depth (default: 5)
- **Breadth limit**: Max children per node (default: 10)

## Scoring: Gemma Effect

The `Gemma` effect decouples scoring from exploration logic:

```haskell
data Gemma m a where
  RateNode :: QueryContext -> NodeContext -> Gemma m Rubric

data Rubric = Rubric
  { relevance   :: Int  -- 1-5
  , risk        :: Int  -- 1-5
  , confidence  :: Int  -- 1-5
  , reasoning   :: Text
  }
```

### Interpreters

| Interpreter | Purpose |
|-------------|---------|
| `runGemmaMock` | Hardcoded rubrics for testing |
| `runGemmaHeuristic` | Pattern-based rules (current) |
| Future: `runGemmaModel` | HTTP to FunctionGemma 270M |

**Why pluggable?** Allows testing without model dependency, enables gradual migration from heuristics to ML.

## Heuristics

`Heuristics.hs` contains pattern-based scoring rules:

### scoreNode
Analyzes hover text + code snippet:
```haskell
scoreNode :: QueryContext -> NodeContext -> Rubric
scoreNode query node
  | "pattern" `isInfixOf` hover && Exhaustive `elem` tags query =
      Rubric 5 5 4 "Exhaustive pattern match"
  | "type family" `isInfixOf` hover && TypeFamily `elem` tags query =
      Rubric 5 4 4 "Type family definition"
  | "data" `isInfixOf` hover =
      Rubric 3 2 5 "Data type definition"
  | otherwise =
      Rubric 1 1 3 "Generic code location"
```

### shouldExpand
Decides whether to follow references:
```haskell
shouldExpand :: Rubric -> QueryContext -> Depth -> Breadth -> Bool
shouldExpand rubric query depth breadth
  | relevance rubric >= 4 = True   -- High relevance: always expand
  | depth > 5 = False              -- Too deep: prune
  | breadth > 10 = False           -- Too wide: prune
  | otherwise = relevance rubric >= 3
```

## MCP Integration

`Main.hs` exposes the agent as an MCP tool:

```haskell
main :: IO ()
main = do
  mode <- parseArgs
  case mode of
    MCP -> withLSPSession "." $ \session ->
      runMcpServer $ McpConfig
        { mcName = "semantic-scout"
        , mcVersion = "0.1"
        , mcTools =
            [ makeMcpTool
                (Proxy @ScoutQuery)
                "scout"
                "Answer semantic questions about code"
                (executeScout session)
            ]
        }
```

### MCP Tool Definition
```json
{
  "name": "scout",
  "description": "Answer semantic questions about code",
  "inputSchema": {
    "type": "object",
    "properties": {
      "query": { "type": "string" },
      "tags": { "type": "string", "description": "Comma-separated tags" },
      "budget": { "type": "integer", "default": 20 }
    },
    "required": ["query"]
  }
}
```

**Note**: Tags serialized as comma-separated string for MCP compatibility (Aeson instance handles conversion).

## Training Data Generation

`--gen-training N` mode generates examples for fine-tuning FunctionGemma:

```haskell
data TrainingExample = TrainingExample
  { teQuery     :: QueryContext
  , teNode      :: NodeContext
  , teRubric    :: Rubric
  , teTimestamp :: UTCTime
  }
```

Flow:
1. Run exploration with heuristic scorer
2. Collect (query, node, rubric) triples
3. Export as JSON for FunctionGemma fine-tuning
4. Future: Replace heuristics with trained model

See `haskell/tools/training-generator/CLAUDE.md` for dataset format.

## LSP Integration

Uses `tidepool-lsp-interpreter` for code intelligence:

```haskell
import Tidepool.Effects.LSP
import Tidepool.LSP.Interpreter (withLSPSession, runLSP)

withLSPSession "/path/to/project" $ \session -> do
  result <- runM $ runLSP session $ exploreEff query
  pure result
```

**Session lifecycle:**
- One session per MCP server instance (long-lived)
- Amortizes HLS startup cost (~5s) across many queries
- Thread-safe via Chan + worker pattern

See `haskell/effects/lsp-interpreter/CLAUDE.md` for LSP details.

## Running Modes

### MCP Server (Production)
```bash
semantic-scout --mcp
# Reads JSON-RPC 2.0 from stdin, outputs to stdout
# Used by Claude Code via mcpServers config
```

### Demo Mode (Development)
```bash
semantic-scout --explore
# Uses mock data for entry points, node contexts, children
# No HLS required
```

### Training Generation
```bash
semantic-scout --gen-training 100 > training.json
# Explores codebase, outputs training examples
# Requires HLS + project with working LSP setup
```

## Requirements

- `haskell-language-server-wrapper` on PATH
- Project with valid `hie.yaml` or cabal config
- Initial HLS load may take 5-10 seconds

## Dependencies

| Package | Purpose |
|---------|---------|
| `tidepool-lsp-interpreter` | LSP effect interpreter |
| `tidepool-mcp-server` | MCP tool wrapping |
| `training-generator` | Training data types |
| `freer-simple` | Effect system |
| `aeson` | JSON serialization |

## Design Decisions

| Decision | Rationale |
|----------|-----------|
| Effect-based LSP | Testable without HLS, supports WASM future |
| Pluggable scorer | Allows heuristics → ML migration without refactoring |
| BFS with budget | Prevents runaway exploration, bounded latency |
| Tags as strings | MCP doesn't support enum types well |
| Long-lived session | Amortizes HLS startup across queries |
| Training data collection | Enables supervised learning from heuristics |

## Related Documentation

- [effects/lsp-interpreter/CLAUDE.md](../../effects/lsp-interpreter/CLAUDE.md) - LSP integration
- [effects/mcp-server/CLAUDE.md](../../effects/mcp-server/CLAUDE.md) - MCP tool wrapping
- [tools/training-generator/CLAUDE.md](../../tools/training-generator/CLAUDE.md) - Training data format
- [Root CLAUDE.md](../../../CLAUDE.md) - Project overview

## Future Work

- Replace heuristics with trained FunctionGemma model
- Add more LSP operations (definition, implementation)
- Support multiple language servers (not just HLS)
- Incremental exploration (resume from previous query)
- Query history and caching
