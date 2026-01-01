---
applyTo: "deploy/**/*.ts"
---

# TypeScript Review Instructions (Cloudflare Worker)

This is a Cloudflare Worker Durable Object harness for WASM state machines.

## Critical Issues

**Protocol Conformance**
- Types in `protocol.ts` must exactly match Haskell `Serializable.hs`
- Any protocol change requires updating both sides
- WebSocket message types are the contract between client and WASM

**Durable Object Constraints**
- State must be JSON-serializable
- Don't hold references across await points
- Use `blockConcurrencyWhile` for initialization

**WASM/JSFFI**
- `loader.ts` and `jsffi.ts` implement GHC WASM runtime
- These are delicate - don't modify without understanding GHC WASM internals
- WASI polyfills are minimal, only what's needed

## Less Important

- General TypeScript style (ESLint handles this)
- Test coverage (critical paths only)

## Key Files

- `src/index.ts` - Durable Object, WebSocket handling, effect loop
- `src/protocol.ts` - Must match Haskell types exactly
- `src/loader.ts` - GHC WASM loader (careful!)
- `src/jsffi.ts` - JavaScript FFI for GHC WASM (careful!)
