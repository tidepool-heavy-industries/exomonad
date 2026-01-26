# ExoMonad DSL

Core graph DSL implementation and teaching infrastructure.

## Packages

| Package | Purpose |
|---------|---------|
| `core/` | Graph DSL, effects, templates, schemas |
| `teaching/` | LLM-level teaching infrastructure for FunctionGemma training |

## Package: core/

**exomonad-core** (~7000 LOC, 53 modules)

### Key Modules

- `Graph/Types.hs` - DSL syntax (:@, Input, Schema, Goto)
- `Graph/Generic.hs` - Mode system, handler computation
- `Graph/Interpret.hs` - Dispatch and interpretation
- `Effect/Types.hs` - Core effect types (LLM, State, Log, Emit, etc.)
- `Effect/NodeMeta.hs` - Node/graph context for teaching (NodeMetadata, GraphMetadata)
- `Effects/*.hs` - Integration effects (BD, GitHub, Habitica, etc.)
- `Schema.hs` - JSON schema generation
- `StructuredOutput/` - Structured LLM output parsing

### Adding New Graph Annotations

Edit `Graph/Types.hs` and add to the annotation type families.

### Native-Only Effects

Some effects are excluded from WASM builds (LSP, GHCi, ClaudeCode, DevLog).
See exomonad-core.cabal `if !os(wasi)` blocks.

## Package: teaching/

**exomonad-teaching** - LLM-level teaching for FunctionGemma training data

See `teaching/CLAUDE.md` for details.

### Status

Implemented but **not wired into any server**. Provides:
- `runLLMWithTeaching` - Intercepts LLM calls, records training data
- `TeachingTurn` - Full LLM turn with node context
- Dual-output recording (anthropic.jsonl + gemma.jsonl)

### Dependencies

- `exomonad-core` - Effect types, NodeMeta
- `exomonad-llm-interpreter` - Anthropic API client
