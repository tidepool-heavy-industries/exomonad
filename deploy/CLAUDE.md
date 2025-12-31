# deploy/ - Cloudflare Worker Durable Object Harness

TypeScript harness for running Tidepool WASM graphs on Cloudflare's edge network.

## What This Is

A Cloudflare Worker that:
1. Hosts a Durable Object (`StateMachineDO`)
2. Loads a WASM module compiled from Haskell (tidepool-wasm)
3. Executes the graph via WebSocket, interpreting effects

The key insight: **TypeScript is a graph-aware effect executor**. Haskell owns the graph structure and execution logic; TypeScript handles IO (LLM calls, HTTP, logging).

## Current Status

**TypeScript side: Complete** - Ready to consume WASM blobs.

| File | Status | Purpose |
|------|--------|---------|
| `protocol.ts` | ✅ Complete | Wire types matching Haskell `WireTypes.hs` |
| `index.ts` | ✅ Complete | Durable Object with WebSocket handling, effect loop |
| `loader.ts` | ✅ Complete | WASM loader with GHC RTS + JSFFI setup |
| `jsffi.ts` | ✅ Complete | JavaScript FFI for GHC WASM |
| `tidepool.wasm.d.ts` | ✅ Stub | TypeScript declaration for WASM exports |

**WASM blob: Missing** - Needs `tidepool-wasm` compiled with `wasm32-wasi-ghc`.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  Cloudflare Durable Object                                  │
│                                                             │
│  ┌─────────────────────────────────────────────────────┐   │
│  │  TypeScript Harness (this directory)                │   │
│  │  - WebSocket connection management                  │   │
│  │  - Effect interpreter (CF AI, fetch, logging)       │   │
│  │  - WASM instance lifecycle                          │   │
│  └──────────────────────┬──────────────────────────────┘   │
│                         │ FFI (JSON)                        │
│  ┌──────────────────────▼──────────────────────────────┐   │
│  │  WASM Module (from tidepool-wasm)                   │   │
│  │  - Graph execution (Runner.hs - TODO)               │   │
│  │  - Effect yield/resume protocol                     │   │
│  │  - Pure business logic, no IO                       │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

## Protocol Flow

```
Client                    TypeScript                    WASM (Haskell)
  │                           │                              │
  │── WS: {init: input} ─────►│                              │
  │                           │── initialize(json) ─────────►│
  │                           │◄── StepOutput {effect} ──────│
  │                           │                              │
  │                           │ [execute effect: LLM/log]    │
  │                           │                              │
  │                           │── step(result) ─────────────►│
  │                           │◄── StepOutput {done: true} ──│
  │                           │                              │
  │◄── WS: {done: result} ────│                              │
```

## Effect Types

Currently defined in `protocol.ts`:

| Effect | Status | Handler |
|--------|--------|---------|
| `LogInfo` | ✅ Ready | Console log |
| `LogError` | ✅ Ready | Console error |
| `LlmComplete` | ✅ Ready | Cloudflare AI binding |
| `HttpFetch` | ✅ Ready | `fetch()` |

## Running

```bash
cd deploy
pnpm install
pnpm dev          # Local dev server (needs WASM blob)
pnpm typecheck    # Type check
pnpm lint         # ESLint
pnpm deploy       # Deploy to Cloudflare
```

## What's Needed

1. **WASM blob** - Build `tidepool-wasm` with:
   ```bash
   nix develop .#wasm
   wasm32-wasi-cabal build tidepool-wasm
   ```
   Copy output to `deploy/tidepool.wasm`

2. **Runner implementation** - `tidepool-wasm/Ffi.hs` stubs need real implementations

## Configuration

`wrangler.toml`:
- Durable Object binding: `STATE_MACHINE`
- AI binding: `AI` (for LLM effects)
- Migration v1 with `StateMachineDO` class
