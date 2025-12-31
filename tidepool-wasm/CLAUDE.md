# tidepool-wasm/ - WASM Deployment Scaffolding

Haskell package that wraps tidepool-core graphs for WASM cross-compilation.

## What This Is

A library package that:
1. Defines a minimal test graph (`TestGraph.hs`) to prove WASM execution works
2. Provides wire types (`WireTypes.hs`) for WASM ↔ TypeScript JSON communication
3. Exports FFI entry points (`Ffi.hs`) that TypeScript calls

The key insight: **tidepool-core stays WASM-agnostic**. This package handles all WASM-specific concerns (serialization, FFI exports, build configuration).

## Current Status

**Complete** - Full FFI implementation with integration tests.

| File | Status | Purpose |
|------|--------|---------|
| `TestGraph.hs` | ✅ Complete | Minimal graph (Int → Int+1 with Log effect) |
| `ExampleGraph.hs` | ✅ Complete | Multi-node graph with branching (message classifier) |
| `WireTypes.hs` | ✅ Complete | SerializableEffect, EffectResult, StepOutput |
| `Runner.hs` | ✅ Complete | Explicit continuation encoding for yield/resume |
| `Ffi.hs` | ✅ Complete | FFI exports with global IORef state management |

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

### ExampleGraph.hs

A non-trivial multi-node graph that demonstrates branching with 3+ targets:

```
Entry(UserMessage) → classify → handleGreeting  ─┐
                            │                    │
                            ├→ handleQuestion   ─┼→ Exit(Response)
                            │                    │
                            └→ handleStatement ─┘
```

**Domain Types** (newtypes for type safety):
- `UserMessage` - Newtype around Text for input
- `Classification` - ADT: Greeting | Question | Statement
- `Response` - Newtype around Text for output

**Nodes** (6 total):
1. `entry` - Entry point with UserMessage
2. `classify` - Classifies messages, yields Log effects, returns 3-way GotoChoice
3. `handleGreeting` - Handles greetings, yields Log, exits with Response
4. `handleQuestion` - Handles questions, yields Log + LlmComplete, exits (also has Self for retry demo)
5. `handleStatement` - Handles statements, yields Log, exits with Response
6. `exit` - Exit point with Response

**Key Features Demonstrated**:
- `GotoChoice` with 3+ targets at classify node
- Multiple effect types: Log (all handlers) + LlmComplete (question handler)
- Self-loop capability in question handler
- Deterministic classification for testability

**FFI Exports** (separate from TestGraph):
- `initializeExample(json)` - Start with JSON-encoded Text message
- `stepExample(json)` - Continue with EffectResult
- `getExampleGraphInfo()` - Get graph structure
- `getExampleGraphState()` - Get runtime state

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
| **1** | Single logic node + Log effect | WASM runs, effect loop works | ✅ Complete |
| **2** | Multiple logic nodes + routing | Graph structure, Goto transitions, GotoChoice with 3+ targets | ✅ Complete |
| 3 | LLM node (CF AI) | Real effect interpretation | Future |

## Implementation Details

### Runner.hs - Continuation Encoding

The runner uses explicit continuations to work around GHC WASM's inability to block mid-computation:

```haskell
data GraphContinuation
  = ContAfterLog Int  -- Waiting for Log effect result, resume with n+1

data GraphYield
  = YieldEffect SerializableEffect GraphContinuation
  | YieldComplete Value
  | YieldError Text
```

### Ffi.hs - Global State

Uses a global `IORef` to persist state across FFI calls:

```haskell
{-# NOINLINE globalState #-}
globalState :: IORef (Maybe RunnerState)
```

This is safe in WASM because each module instance is isolated (one per Durable Object).

### Testing

- `RunnerSpec.hs` - Tests pure continuation logic
- `FfiSpec.hs` - Integration tests through JSON interface
- `TestGraphSpec.hs` - Tests TestGraph handler behavior
- `ExampleGraphSpec.hs` - Tests ExampleGraph (21 tests covering all branch paths)
- Uses `resetState` / `resetExampleState` for test isolation

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
    # TestGraph exports
    -optl-Wl,--export=initialize
    -optl-Wl,--export=step
    -optl-Wl,--export=getGraphInfo
    -optl-Wl,--export=getGraphState
    # ExampleGraph exports
    -optl-Wl,--export=initializeExample
    -optl-Wl,--export=stepExample
    -optl-Wl,--export=getExampleGraphInfo
    -optl-Wl,--export=getExampleGraphState
```
