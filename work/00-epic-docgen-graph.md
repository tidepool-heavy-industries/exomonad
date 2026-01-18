# Epic: DocGen as Graph DSL Agent

## Vision

Transform DocGen from a hand-rolled effect-based implementation into a proper graph DSL agent. This becomes the **canonical example** of how to build agents using the tidepool graph DSL, demonstrating:

- Graph-based LLM orchestration
- Teaching/training data capture via tidepool-teaching
- Claude Code++ MCP integration
- End-to-end testing workflow

## Current State

```
DocGen.hs (hand-rolled BFS loop)
    → ScoutGemma effect (custom freer-simple effect)
    → runScoutGemmaHTTP (direct HTTP to Ollama)
    → No teaching capture, no graph DSL
```

## Target State

```
DocGenGraph (graph DSL)
    → entry :: Entry TeachQuery
    → resolve :: LogicNode (LSP symbol resolution)
    → select :: LLMNode :@ Template SelectTpl :@ Schema SelectedSymbols
    → expand :: LogicNode (frontier management)
    → exit :: Exit TeachingDoc

Effect Stack:
    → runLLMWithTeaching (intercepts LLM calls, records JSONL)
    → runLSP (existing LSP session)
    → Haiku API (via teaching interpreter)
```

## Success Criteria (Epic Level)

1. **Claude Code can call `teach` MCP tool** and receive structured results
2. **JSONL training data is captured** for every Haiku call during exploration
3. **Logs show graph execution** with node transitions visible
4. **Example is self-documenting** - serves as reference for future graph agents

## Task Breakdown

| Task | Description | Depends On |
|------|-------------|------------|
| [01-docgen-graph-design](01-docgen-graph-design.md) | Design graph structure, node types, data flow | - |
| [02-templates-and-schemas](02-templates-and-schemas.md) | Create LLM node templates and output schemas | 01 |
| [03-wire-graph-execution](03-wire-graph-execution.md) | Integrate graph runner into control-server | 01, 02 |
| [04-integrate-teaching](04-integrate-teaching.md) | Wire tidepool-teaching for JSONL capture | 03 |
| [05-e2e-mcp-testing](05-e2e-mcp-testing.md) | Test with Claude Code as MCP client | 04 |
| [06-cleanup-prototype](06-cleanup-prototype.md) | Remove/deprecate ScoutGemma prototype | 05 |

## Key Files

### Existing (to understand)
- `control-server/src/Tidepool/Control/Scout/DocGen.hs` - Current BFS implementation
- `control-server/src/Tidepool/Control/Scout/DocGen/Gemma.hs` - ScoutGemma prototype
- `control-server/src/Tidepool/Control/Handler/MCP.hs` - MCP tool routing
- `dsl/teaching/src/Tidepool/Teaching/LLM.hs` - Teaching interpreter

### To Create
- `control-server/src/Tidepool/Control/Scout/Graph.hs` - DocGen graph definition
- `control-server/src/Tidepool/Control/Scout/Graph/Types.hs` - Graph-specific types
- `control-server/src/Tidepool/Control/Scout/Graph/Templates.hs` - LLM templates
- `control-server/src/Tidepool/Control/Scout/Graph/Handlers.hs` - Node handlers

## Architecture Notes

### Why Graph DSL?

1. **Teaching capture** - Graph execution goes through `LLM` effect, which tidepool-teaching intercepts
2. **Observability** - Graph transitions are traceable, debuggable
3. **Composability** - Can add nodes (e.g., caching, fallback) without restructuring
4. **Type safety** - Compile-time validation of node connections

### Graph vs Current BFS

The current BFS loop is imperative:
```haskell
exploreLoop :: TeachState -> Eff effs TeachingDoc
exploreLoop state = do
  case tsFrontier state of
    [] -> buildDoc state
    ((sym, depth):rest) -> do
      info <- lookupSymbol sym
      candidates <- extractCandidates info
      selected <- selectRelevantSymbols topic info candidates  -- LLM call here
      resolved <- resolveTokens selected
      exploreLoop (addToFrontier resolved state)
```

The graph version separates concerns:
- **Entry** receives query
- **Resolve** handles LSP lookup (LogicNode)
- **Select** handles LLM classification (LLMNode - captured by teaching)
- **Expand** manages frontier (LogicNode with Goto Self for looping)
- **Exit** builds final document

### Self-Loop Pattern

The exploration loop uses `Goto Self` for BFS iteration:
```haskell
expand :: mode :- LogicNode
    :@ Input ExpandInput
    :@ UsesEffects '[Goto Self ExpandInput, Goto "exit" TeachingDoc]
```

Handler returns `gotoSelf` while frontier non-empty, `gotoChoice @"exit"` when done.

## Testing Strategy

1. **Unit tests** for templates/schemas (do they render correctly?)
2. **Integration test** with mock LSP (does graph execute?)
3. **E2E test** with Claude Code (does MCP flow work?)
4. **JSONL validation** (are training examples well-formed?)

## Open Questions

- [ ] Should we support both Haiku and local Ollama? (env var switch?)
- [ ] How much of the BFS state goes in Memory vs passed through nodes?
- [ ] Do we want streaming results back to Claude Code?
