# ExoMonad

> Type-safe LLM agent framework with structured state and templates

## What Is This?

A library for building LLM agents using **polysemy** for sandboxed effects, **ginger** (Jinja) templates validated at compile time, and **structured output** for LLM → state mutations.

The key insight: LLMs don't need raw IO access. They need:
1. **Typed state** they can read (via templates)
2. **Typed mutations** they can express (via structured output)
3. **Typed tools** for mid-turn capabilities

The Haskell code controls what's possible. The LLM controls what happens.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Agent Turn Loop                         │
│                                                             │
│  1. Build context (Haskell: State → Context)                │
│  2. Render template (Jinja: Context → prompt)               │
│  3. Call LLM (runTurn: prompt + schema + tools → result)    │
│  4. Apply structured output (Output → State')               │
│  5. Handle user input (RequestInput for choices)            │
│  6. Return response                                         │
└─────────────────────────────────────────────────────────────┘
```

### Effects (what agents can do)

```haskell
type AgentEffects s event =
  '[ State s          -- read/write agent state
   , Random           -- dice rolls, weighted choices
   , LLM              -- template-based LLM calls
   , Emit event       -- observable events
   , RequestInput     -- user input
   ]

-- Notably absent: IO, Network, FileSystem
-- The interpreter handles all real-world interaction
```

### Templates (what the LLM sees)

Ginger (Jinja) templates validated at compile time:

```haskell
$(typedTemplateFile ''MyContext "templates/turn.jinja")
-- Compile error if template references nonexistent fields
```

### Structured Output (what the LLM can express)

Every mutation includes `because` for training data:

```haskell
data StateDelta = StateDelta
  { deltaValue :: Int
  , deltaBecause :: Text  -- "User requested change"
  }
```

## Quick Start

```bash
# Build all packages
cabal build all

# Run pre-build for Docker (generates SSH keys)
./scripts/docker-prebuild.sh

# Run native server with SimpleAgent example
just native  # Starts at localhost:8080
```

## Project Structure

```
haskell/dsl/core/        # Core library (effects, templates)
haskell/effects/         # Native effect interpreters
haskell/protocol/        # Wire types and FFI
```

For detailed documentation, see `CLAUDE.md`.

## Example Agents

Agents live in separate repos to keep the library clean:
- `~/exomonad-labs/anemone` - DM agent, Tidying agent, etc.

## See Also

- [polysemy](https://hackage.haskell.org/package/polysemy) - Higher-power, low-boilerplate effect system
- [ginger](https://github.com/inanna-malick/ginger) - Jinja template engine for Haskell (typed fork)
- [Anthropic Tool Use](https://docs.anthropic.com/en/docs/tool-use) - LLM tool calling

## Testing

### FFI Property Tests

We use property-based testing to verify JSON serialization across the WASM FFI boundary.

**Rust:**
```bash
cd rust && cargo test --test proptest_ffi
```

**Haskell:**
```bash
cd haskell/wasm-guest && cabal test ffi-serialization-tests
```

**Integration (Round-trip):**
```bash
cd rust && cargo test --test integration_ffi
```
