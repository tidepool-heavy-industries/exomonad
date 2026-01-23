# deploy/ - Cloudflare Worker Durable Object Harness

**STATUS: FROZEN** - WASM/Cloudflare deployment is not currently maintained. The blessed development workflow is `./start-augmented.sh` for Claude Code++ integration.

TypeScript harness for running Tidepool WASM graphs on Cloudflare's edge network.

## What This Is

A Cloudflare Worker that:
1. Hosts a Durable Object (`StateMachineDO`)
2. Loads a WASM module compiled from Haskell (tidepool-wasm)
3. Executes the graph via WebSocket, interpreting effects

The key insight: **TypeScript is a graph-aware effect interpreter**. Haskell owns the graph structure and execution logic; TypeScript handles IO (LLM calls, HTTP, logging).

## Current Status

**TypeScript side: Complete** - Ready to consume WASM blobs.

| File | Status | Purpose |
|------|--------|---------|
| `protocol.ts` | ✅ Complete | Wire types matching Haskell `WireTypes.hs` |
| `index.ts` | ✅ Complete | Durable Object with WebSocket handling, effect loop |
| `loader.ts` | ✅ Complete | WASM loader with GHC RTS + JSFFI setup |
| `jsffi.ts` | ✅ Complete | JavaScript FFI for GHC WASM |
| `tidepool.wasm.d.ts` | ✅ Stub | TypeScript declaration for WASM exports |
| `handlers/` | ✅ Complete | Effect handler registry with tests |

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

See [docs/PROTOCOL.md](docs/PROTOCOL.md) for the full WebSocket protocol specification.

### Message Types

**Client → Server:**
- `init` - Start graph execution: `{ type: 'init', graphId: string, input: unknown }`
- `resume` - Provide effect result: `{ type: 'resume', result: EffectResult }`
- `reconnect` - Resume suspended session: `{ type: 'reconnect', sessionId: string }`
- `ping` - Keepalive heartbeat

**Server → Client:**
- `progress` - Effect executing server-side: `{ type: 'progress', effect, status }`
- `yield` - Effect for client handling: `{ type: 'yield', effect, sessionId }`
- `done` - Graph completed: `{ type: 'done', result }`
- `error` - Error occurred: `{ type: 'error', message, recoverable, sessionId? }`
- `pong` - Keepalive response

### Sequence Diagram

```
Client                    TypeScript                    WASM (Haskell)
  │                           │                              │
  │── WS: {init: input} ─────►│                              │
  │                           │── initialize(json) ─────────►│
  │                           │◄── StepOutput {effect} ──────│
  │                           │                              │
  │◄── {progress} ────────────│ [execute effect: LLM/log]    │
  │                           │                              │
  │                           │── step(result) ─────────────►│
  │                           │◄── StepOutput {done: true} ──│
  │                           │                              │
  │◄── WS: {done: result} ────│                              │
```

### Session Management

- Sessions are stored in Durable Object storage for reconnection
- Sessions expire after 5 minutes of inactivity
- `sessionId` is included in `yield` and recoverable `error` messages
- Clients can reconnect with `reconnect` message to resume from last yield point

## Effect Types

Effects are divided into two categories:

### Internal Effects (handled by StateMachineDO)

| Effect | Status | Handler | Semantics |
|--------|--------|---------|-----------|
| `LogInfo` | ✅ Ready | Console log | Fire-and-forget |
| `LogError` | ✅ Ready | Console error | Fire-and-forget |
| `LlmComplete` | ✅ Ready | Cloudflare AI | Blocking |
| `LlmCall` | ✅ Ready | Cloudflare AI (multi-turn) | Blocking |
| `Habitica` | ✅ Ready | Habitica API | Blocking |
| `GetState` | ✅ Ready | DO storage read | Blocking |
| `SetState` | ✅ Ready | DO storage write | Fire-and-forget |
| `RandomInt` | ✅ Ready | Math.random | Blocking |
| `GetTime` | ✅ Ready | Date.now | Blocking |

### Yielded Effects (returned to caller for handling)

| Effect | Status | Handler | Semantics |
|--------|--------|---------|-----------|
| `TelegramSend` | ✅ Ready | Caller-provided | Fire-and-forget |
| `TelegramAsk` | ✅ Ready | Caller-provided | Blocking |
| `TelegramConfirm` | ✅ Ready | Caller-provided | Blocking |
| `EmitEvent` | ✅ Ready | Caller-provided | Fire-and-forget |

### Effect Routing

Effects are routed based on `EffectCategory`:
- **Internal**: Executed by StateMachineDO, result returned to WASM
- **Yielded**: Sent to WebSocket client for external handling

And `EffectSemantics`:
- **Blocking**: WASM waits for result before continuing
- **Fire-and-forget**: WASM continues immediately (result is `null`)

> **Types**: `tidepool-generated-ts/src/protocol.ts`, `tidepool-wasm/src/Tidepool/Wasm/WireTypes.hs`

## Effect Handlers

Effect handlers are implemented in `src/handlers/`:

```
src/handlers/
├── index.ts      # Registry: executeEffect() dispatches to handlers
├── log.ts        # LogInfo, LogError → console output
├── llm.ts        # LlmComplete, LlmCall → Cloudflare AI
├── habitica.ts   # Habitica API operations
├── state.ts      # GetState, SetState → Durable Object storage
├── random.ts     # RandomInt → crypto.randomInt
├── time.ts       # GetTime → Date.now (ISO8601)
├── telegram.ts   # TelegramSend, TelegramAsk, TelegramConfirm (yielded to caller)
└── __tests__/    # Vitest tests for each handler
```

### Handler Details

#### state.ts - State Persistence

| Function | Effect | Description |
|----------|--------|-------------|
| `handleGetState` | `GetState` | Read state by key, returns `null` if not found |
| `handleSetState` | `SetState` | Write state by key (fire-and-forget) |

**Implementation**: Currently uses in-memory Map (dev/testing). Production TODO: use DO storage.

```typescript
// Testing utilities
clearState()           // Reset all state
getStateSnapshot()     // Debug: get all state as Record
```

#### random.ts - Random Numbers

| Function | Effect | Description |
|----------|--------|-------------|
| `handleRandomInt` | `RandomInt` | Random int in [min, max] inclusive |

**Implementation**: Uses `crypto.randomInt` for unbiased, cryptographically secure randomness.
Requires `nodejs_compat` flag in wrangler.toml.

#### time.ts - Current Time

| Function | Effect | Description |
|----------|--------|-------------|
| `handleGetTime` | `GetTime` | Current UTC time as ISO8601 string |

Returns format: `"2024-01-15T10:30:00.000Z"`

#### telegram.ts - Telegram Bot Integration

| Function | Effect | Description |
|----------|--------|-------------|
| `handleTelegramSend` | `telegram_send` | Send message (fire-and-forget) |
| `handleTelegramReceive` | `telegram_receive` | Block until messages arrive |
| `handleTelegramTryReceive` | `telegram_try_receive` | Return pending messages (non-blocking) |
| `handleTelegramAsk` | `TelegramAsk` | Present buttons, wait for response |

**TelegramAsk Protocol**:
1. First call sends buttons with a nonce, returns `{ type: "yield", nonce, buttonMapping, messageId }`
2. Caller stores nonce and waits for user interaction
3. Resume call checks pending messages against stored nonce
4. Returns `TelegramAskResult`: `{ type: "button", response }` | `{ type: "text", text }` | `{ type: "stale_button" }`

**Button ID Mapping**: Short IDs (`btn_0`, `btn_1`) are used in callback_data to stay within Telegram's 64-byte limit. Original data is stored in `buttonMapping` and resolved on click.

### Handler Interface

All handlers follow this pattern:

```typescript
async function handleEffect(effect: EffectType, env?: Env): Promise<EffectResult>
```

Results are always `{ type: "success", value: T }` or `{ type: "error", message: string }`.

### Error Handling

Handlers return typed errors rather than throwing:
- **Rate limits**: `LLM rate limited: ...`
- **Timeouts**: `HTTP timeout after 25000ms: ...`
- **Network errors**: `HTTP network error: ...`
- **Unknown effects**: `Unknown effect type: ...`

The `executeEffect()` wrapper catches any uncaught exceptions and converts them to error results.

### Adding a New Handler

1. Create `src/handlers/myeffect.ts` with handler function
2. Add effect type to `protocol.ts` (must match Haskell side)
3. Add case to `executeEffect()` in `src/handlers/index.ts`
4. Add tests in `src/handlers/__tests__/myeffect.test.ts`

## Related Documentation

- [haskell/dsl/core/CLAUDE.md](../haskell/dsl/core/CLAUDE.md) - Graph DSL and effect definitions
- [haskell/runtime/wasm/CLAUDE.md](../haskell/runtime/wasm/CLAUDE.md) - WASM compilation and FFI
- [haskell/protocol/generated-ts/CLAUDE.md](../haskell/protocol/generated-ts/CLAUDE.md) - Generated TypeScript types

## Running

```bash
cd deploy
pnpm install
pnpm dev          # Local dev server (needs WASM blob)
pnpm typecheck    # Type check
pnpm test         # Run vitest tests
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
