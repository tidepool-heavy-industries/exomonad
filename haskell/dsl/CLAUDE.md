# Tidepool DSL

Core graph DSL implementation. Defines graph types, validation, templates, schemas.

## Package: core/

**tidepool-core** (~7000 LOC, 53 modules)

### Key Modules

- `Graph/Types.hs` - DSL syntax (:@, Input, Schema, Goto)
- `Graph/Generic.hs` - Mode system, handler computation
- `Graph/Execute.hs` - Dispatch and execution
- `Effect/Types.hs` - Core effect types (LLM, State, Log, Emit, etc.)
- `Effects/*.hs` - Integration effects (BD, GitHub, Habitica, etc.)
- `Schema.hs` - JSON schema generation
- `StructuredOutput/` - Structured LLM output parsing

### Adding New Graph Annotations

Edit `Graph/Types.hs` and add to the annotation type families.

### Native-Only Effects

Some effects are excluded from WASM builds (LSP, GHCi, ClaudeCode, DevLog).
See tidepool-core.cabal `if !os(wasi)` blocks.

## Dependencies

None (foundation package)
