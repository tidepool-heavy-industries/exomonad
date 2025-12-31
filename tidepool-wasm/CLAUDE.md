# tidepool-wasm/ - WASM Deployment Scaffolding

Haskell package that wraps tidepool-core graphs for WASM cross-compilation.

## What This Is

A library package that:
1. Defines a minimal test graph (`TestGraph.hs`) to prove WASM execution works
2. Provides wire types (`WireTypes.hs`) for WASM ↔ TypeScript JSON communication
3. Exports FFI entry points (`Ffi.hs`) that TypeScript calls

The key insight: **tidepool-core stays WASM-agnostic**. This package handles all WASM-specific concerns (serialization, FFI exports, build configuration).

## Current Status

**Scaffolding: Complete** - Package structure and types defined.

| File | Status | Purpose |
|------|--------|---------|
| `TestGraph.hs` | ✅ Complete | Minimal graph (Int → Int+1 with Log effect) |
| `WireTypes.hs` | ✅ Complete | SerializableEffect, EffectResult, StepOutput |
| `Ffi.hs` | ✅ Stubs | FFI exports (implementations are `error "TODO"`) |

**Runner implementation: Out of scope** - Handled by larger LLM swarm.

## Architecture

```
tidepool-core (pure graph DSL)
       │
       ▼
tidepool-wasm (this package)
       │
       ├─→ Native build: Tests, development
       │
       └─→ WASM build: wasm32-wasi-ghc → .wasm blob
                │
                ▼
          deploy/ (TypeScript harness consumes blob)
```

## Files

### TestGraph.hs

Purpose-built graph for proving WASM works. Uses record-based (Servant-style) syntax:

```haskell
data TestGraph mode = TestGraph
  { entry   :: mode :- Entry Int
  , compute :: mode :- LogicNode :@ Needs '[Int] :@ UsesEffects '[Log, Goto Exit Int]
  , exit    :: mode :- Exit Int
  }
```

The same type serves dual purposes:
- **AsGraph mode**: Type-level specification (structure, edges, effect requirements)
- **AsHandler mode**: Value-level handlers (actual implementations)

### WireTypes.hs

Types that cross the WASM/JSON boundary. Matches `deploy/src/protocol.ts`:

| Type | Direction | Purpose |
|------|-----------|---------|
| `SerializableEffect` | WASM → TS | Effect to execute (LogInfo, future: LlmComplete) |
| `EffectResult` | TS → WASM | Result of effect execution (success/error) |
| `StepOutput` | WASM → TS | Per-step output (effect, done flag, result) |

### Ffi.hs

FFI exports with CPP conditionals:

```haskell
#if defined(wasm32_HOST_ARCH)
foreign export javascript "initialize" initialize :: JSString -> IO JSString
foreign export javascript "step" step :: JSString -> IO JSString
#endif
```

Native builds get the same interface without FFI exports, enabling testing.

## Building

### Native (development/testing)

```bash
# From repo root
cabal build tidepool-wasm
cabal test tidepool-wasm  # Once tests exist
```

### WASM (cross-compilation)

```bash
# Enter WASM shell
nix develop .#wasm

# Build WASM blob
wasm32-wasi-cabal build tidepool-wasm

# Output location
# dist-newstyle/build/wasm32-wasi/ghc-9.10.*/tidepool-wasm-*/...
```

## Incremental Milestones

| Step | Graph Shape | Proves | Status |
|------|-------------|--------|--------|
| **1** | Single logic node + Log effect | WASM runs, effect loop works | Scaffolding done |
| 2 | Multiple logic nodes + routing | Graph structure, Goto transitions | Future |
| 3 | LLM node (CF AI) | Real effect interpretation | Future |

## What's Needed (Swarm Implements)

1. **Runner implementation** in `Ffi.hs`:
   - `initializeImpl`: Parse entry value, run graph until first effect yield
   - `stepImpl`: Resume with effect result, run until next yield or completion
   - `getGraphInfoImpl`: Reify graph structure to JSON
   - `getGraphStateImpl`: Serialize current execution state

2. **Effect interpretation loop** that:
   - Runs graph handlers
   - Catches effect operations
   - Serializes them as `StepOutput`
   - Resumes when `step` is called with result

## Protocol Flow

```
TypeScript                              WASM (Haskell)
    │                                        │
    │── initialize("5") ────────────────────►│
    │                                        │ Parse 5
    │                                        │ Run compute handler
    │                                        │ Handler: logInfo "Computing: 5"
    │◄── StepOutput ─────────────────────────│
    │    { effect: {type: "LogInfo",         │
    │               eff_message: "Computing: 5"}
    │      done: false }                     │
    │                                        │
    │ [execute: console.log]                 │
    │                                        │
    │── step({type: "success"}) ────────────►│
    │                                        │ Resume handler
    │                                        │ Handler: goto @Exit 6
    │◄── StepOutput ─────────────────────────│
    │    { effect: null,                     │
    │      done: true,                       │
    │      stepResult: 6 }                   │
```

## Dependencies

- `tidepool-core` - Graph DSL, effects, validation
- `effectful-core` - Effect system
- `aeson` - JSON encoding/decoding
- `text` - Text type
- `ghc-experimental` - WASM FFI primitives (WASM builds only)

## Cabal Configuration

Key WASM-specific options in `tidepool-wasm.cabal`:

```cabal
if os(wasi)
  build-depends: ghc-experimental
  ghc-options:
    -no-hs-main                    -- No main(), it's a library
    -optl-mexec-model=reactor      -- WASI reactor model
    -optl-Wl,--export=hs_init      -- Export RTS initialization
    -optl-Wl,--export=initialize   -- Export our functions
    -optl-Wl,--export=step
    -optl-Wl,--export=getGraphInfo
    -optl-Wl,--export=getGraphState
```
