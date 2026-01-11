# Runtime Backends

Execution backends for Tidepool graphs. Each backend provides a different execution model.

## Structure

| Package | Purpose | Key Exports |
|---------|---------|-------------|
| `actor/` | Actor model with message passing → see actor/CLAUDE.md | `withActorSystem`, `spawnActor`, `Router` |
| `wasm/` | WASM compilation target → see wasm/CLAUDE.md | `WasmM`, yield/resume protocol |

## Choosing a Backend

| Backend | Use Case | Tradeoffs |
|---------|----------|-----------|
| **Actor** | Native server, development, debugging | Simple, concurrent, good for fan-out/barrier |
| **WASM** | Cloudflare Workers, edge deployment | Portable, yield/resume, requires serialization |

## Actor Model (Primary)

The actor backend is the recommended choice for native execution:

```haskell
import Tidepool.Actor (withActorSystem, spawnActor)

result <- withActorSystem initialPayload $ \scope router -> do
  compute <- spawnActor scope "compute" $ \msg -> do
    let output = process msg
    router "exit" (toJSON output)
  pure $ Map.singleton "compute" compute
```

**Key features:**
- **Structured concurrency via ki** - Automatic cleanup on scope exit
- **Message passing** - Actors communicate via JSON values
- **Subgraph support** - Spawn child graph instances recursively
- **Fork/Barrier pattern** - Fan-out with result collection

## WASM Backend

For edge deployment on Cloudflare Workers:

```haskell
import Tidepool.Wasm (WasmM, stepGraph)

-- Effects yield to TypeScript harness
computeHandler :: Int -> WasmM (GotoChoice '[To Exit Int])
computeHandler n = do
  logInfo "Computing..."  -- Yields Log effect
  pure $ gotoExit (n + 1)
```

**Key features:**
- **Yield/resume protocol** - Effects cross FFI boundary
- **Serializable state** - GraphState survives across invocations
- **TypeScript harness** - Interprets effects in Cloudflare Worker

## Adding a New Backend

1. Create package at `runtime/{name}/`
2. Implement effect interpreter for target environment
3. Provide graph execution entry point
4. Wire into build (cabal.project)
