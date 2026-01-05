# Tidepool

> Type-safe LLM agent loops with structured state and templates

## What Is This?

A library for building LLM agents as typed state machines using **freer-simple** for sandboxed effects with reified continuations, **ginger** (Jinja) templates validated at compile time, and **structured output** for LLM → state mutations.

The key insight: LLMs don't need raw IO access. They need:
1. **Typed state** they can read (via templates)
2. **Typed mutations** they can express (via structured output)
3. **Typed tools** for mid-turn capabilities

The Haskell code controls what's possible. The LLM controls what happens.

## Architecture

```
┌─────────────────────────────────────────────────────┐
│                    Agent Turn Loop                   │
│                                                     │
│  1. Build context (Haskell: state → context)        │
│  2. Render template (ginger: context → prompt)      │
│  3. Call LLM (API: prompt + tools → response)       │
│  4. Parse output (JSON: response → output)          │
│  5. Apply changes (Haskell: output → state')        │
│  6. Repeat                                          │
└─────────────────────────────────────────────────────┘
```

### Effects (what agents can do)

```haskell
type AgentEffects s event =
  '[ State s          -- read/write agent state
   , Random           -- dice rolls, weighted choices
   , LLM              -- template-based LLM calls
   , Emit event       -- observable events
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

Every mutation includes `because` for sleeptime learning:

```haskell
data StatusChange = StatusChange
  { newStatus :: Status
  , changeBecause :: Text  -- "User completed the task"
  }
```

## Quick Start

```bash
# Build all packages
cabal build all

# Run native server (example agent)
just native  # Starts at localhost:8080

# Run tests
cabal test all
```

## Project Structure

```
tidepool-core/           # Core library
├── src/Tidepool/
│   ├── Effect/          # Effect types
│   ├── Graph/           # Type-level DSL for agent graphs
│   ├── Template.hs      # Template type + TH validation
│   └── Tool.hs          # Tool typeclass

tidepool-native-gui/     # Native executors
├── server/              # Servant + WebSocket server
├── llm-executor/        # Anthropic API executor
├── ui-executor/         # UI effect executor
├── bd-executor/         # Beads task tracking executor
├── claude-code-executor/  # Claude Code subprocess executor
└── lsp-executor/        # LSP effect executor

deploy/                  # Cloudflare Worker harness
```

## Documentation

See `CLAUDE.md` for full architecture documentation and effect system details.

## See Also

- [freer-simple](https://hackage.haskell.org/package/freer-simple) - Effect system with reified continuations
- [ginger](https://hackage.haskell.org/package/ginger) - Jinja template engine for Haskell
