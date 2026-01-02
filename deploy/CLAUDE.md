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

Currently defined in `protocol.ts`:

| Effect | Status | Handler |
|--------|--------|---------|
| `LogInfo` | ✅ Ready | Console log |
| `LogError` | ✅ Ready | Console error |
| `LlmComplete` | ✅ Ready | Cloudflare AI binding |
| `Habitica` | ✅ Ready | Habitica API |

## Effect Handlers

Effect handlers are implemented in `src/handlers/`:

```
src/handlers/
├── index.ts      # Registry: executeEffect() dispatches to handlers
├── log.ts        # LogInfo, LogError → console output
├── llm.ts        # LlmComplete → Cloudflare AI (@cf/meta/llama-3.3-70b-instruct-fp8-fast)
├── habitica.ts   # Habitica API operations
└── __tests__/    # Vitest tests for each handler
```

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

### Habitica Handler (`habitica.ts`)

Handles `EffHabitica` effects with 6 operations. The Haskell side uses a typed GADT (`tidepool-wasm/src/Tidepool/Wasm/Habitica.hs`) but the wire format is `{ eff_hab_op: string, eff_hab_payload: object }`.

| Operation | Payload | Response |
|-----------|---------|----------|
| `GetUser` | `{}` | `{ userId, userName, userStats: { usHp, usMp, usExp, usGp } }` |
| `GetTasks` | `{ taskType: "habits"\|"dailys"\|"todos"\|"rewards" }` | `[{ taskId, taskText, taskType, taskCompleted }]` |
| `FetchTodos` | `{}` | `[{ todoId, todoTitle, todoChecklist: [{checklistId, checklistText, checklistDone}], todoCompleted }]` |
| `ScoreTask` | `{ taskId, direction: "up"\|"down" }` | `{ srDelta, srDrop? }` |
| `CreateTodo` | `{ title }` | `{ unTodoId }` |
| `AddChecklistItem` | `{ todoId, item }` | `string` (the new item ID) |

**Adding a new Habitica operation**:
1. Add constructor to `HabiticaOp` GADT in `Habitica.hs`
2. Add case to `encodeOp` (operation name + payload)
3. Add case to `decodeResult` (parse response)
4. Add case to `handleHabitica` switch in `habitica.ts`
5. Response types must match exactly between Haskell `FromJSON` and TypeScript handler

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
