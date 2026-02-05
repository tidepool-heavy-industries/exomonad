# ExoMonad DSL

Core effect types, structured output, and LLM infrastructure.

## Packages

| Package | Purpose |
|---------|---------|
| `core/` | Effects, structured output, LLM types, templates, schemas |

## Package: core/

**exomonad-core**

See `core/CLAUDE.md` for detailed documentation.

### Key Modules

- `Effect/Types.hs` - Core effect types (LLM, State, Log, Emit, TUI, etc.)
- `Effects/*.hs` - Integration effects (Git, GitHub, Zellij, etc.)
- `StructuredOutput/` - Compile-time validated LLM output parsing
- `Schema.hs` - JSON schema generation
- `LLM/Types.hs` - Model selection, prompt wrappers
- `Template/Render.hs` - Jinja template rendering

### Native-Only Modules

Some modules are excluded from WASM builds.
See `exomonad-core.cabal` `if !os(wasi)` blocks.
