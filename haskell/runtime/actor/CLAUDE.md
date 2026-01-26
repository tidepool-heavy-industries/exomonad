# Actor Runtime

Concurrent graph execution via actor model with message passing.

## Package: exomonad-actor

**Size**: ~76k LOC across 11 modules

### Key Modules

| Module | Purpose | Key Exports |
|--------|---------|-------------|
| `Types.hs` | Core types | `Actor`, `ActorId` |
| `Mailbox.hs` | Message queues | `Mailbox`, `send`, `receive` |
| `Runtime.hs` | System lifecycle | `withActorSystem`, `Router` |
| `Spawn.hs` | Actor creation | `spawnActor`, `spawnActorWith` |
| `Graph.hs` | Graph execution | `runGraphAsActors`, `NodeHandler` |
| `Dispatch.hs` | Message routing | `dispatchMessage` |
| `Fork.hs` | Parallel fan-out | `forkHandler`, `SpawnTargets` |
| `Barrier.hs` | Result collection | `barrierHandler`, `Arrive` |
| `Subgraph.hs` | Recursive graphs | `spawnSelf`, `awaitAny`, `collectAll` |
| `Retry.hs` | Retry logic | `withRetry`, `RetryConfig` |
| `Merge.hs` | Result merging | `MergeStrategy`, `collectResults` |

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    Ki Scope                              │
│  (structured concurrency - automatic cleanup on exit)    │
│                                                          │
│  ┌────────┐    ┌────────┐    ┌────────┐    ┌────────┐  │
│  │ entry  │───▶│compute │───▶│ route  │───▶│ exit   │  │
│  │ actor  │    │ actor  │    │ actor  │    │ actor  │  │
│  └────────┘    └────────┘    └────────┘    └────────┘  │
│       ▲              │                           │      │
│       │              │  Mailbox                  │      │
│       │              ▼  (TQueue)                 ▼      │
│  entryPayload   Message flow              exitMVar      │
│                                                          │
└─────────────────────────────────────────────────────────┘
```

## Core Concepts

### Actor

An actor is a running computation with a mailbox:

```haskell
data Actor = Actor
  { actorId      :: !ActorId      -- Unique identifier (node name)
  , actorMailbox :: !(Mailbox Value)  -- Incoming message queue
  }
```

**Note**: No thread field - ki's scope manages lifecycle.

### Router

A router sends messages to actors by ID:

```haskell
type Router = ActorId -> Value -> IO ()

-- Special targets:
-- "entry" - Initial message destination
-- "exit"  - Final result delivery
```

### Mailbox

Typed queue for actor communication:

```haskell
data Mailbox a = Mailbox !(TQueue a)

send :: Mailbox a -> a -> IO ()
receive :: Mailbox a -> IO a
tryReceive :: Mailbox a -> IO (Maybe a)
```

## Usage Patterns

### Basic Graph Execution

```haskell
import ExoMonad.Actor.Graph (runGraphAsActors, pureHandler)

handlers :: Map ActorId NodeHandler
handlers = Map.fromList
  [ ("entry", pureHandler entryLogic)
  , ("compute", pureHandler computeLogic)
  ]

result <- runGraphAsActors handlers initialInput
```

### Fork/Barrier Pattern

For parallel fan-out with result collection:

```haskell
-- Fork spawns to multiple workers
forkHandler :: Input -> Eff es (HList '[Worker1Ctx, Worker2Ctx])
forkHandler input = do
  let ctx1 = buildWorker1Context input
  let ctx2 = buildWorker2Context input
  pure (ctx1 ::: ctx2 ::: HNil)

-- Barrier collects from all workers
barrierHandler :: [Result] -> Eff es FinalOutput
barrierHandler results = do
  let merged = combineResults results
  pure $ gotoExit merged
```

### Subgraph (Recursive Execution)

For tree decomposition where children run full graph cycles:

```haskell
import ExoMonad.Actor.Subgraph

-- Handler spawns child graphs
decomposeHandler :: Spec -> Eff (Subgraph Spec Result ': es) (GotoChoice targets)
decomposeHandler spec = do
  let childSpecs = partition spec
  handles <- traverse spawnSelf childSpecs
  results <- collectAll handles
  pure $ gotoExit (combine results)

-- collectAll blocks until all children complete
collectAll :: [ChildHandle] -> Eff (Subgraph Spec Result ': es) [Result]
collectAll handles = go handles []
  where
    go [] acc = pure (reverse acc)
    go pending acc = do
      (childId, result) <- awaitAny  -- Blocks until any child done
      go (filter (\h -> handleId h /= childId) pending) (result : acc)
```

### Self-Reference Pattern

For recursive subgraph execution, the runner passes itself:

```haskell
runV2Graph :: Spec -> IO V2Result
runV2Graph spec = do
  state <- newSubgraphState runV2Graph  -- Self-reference!
  runGraphWithSubgraph state spec
```

## Key Implementation Details

### Structured Concurrency (ki)

All actors run within a ki scope:
- Scope exit automatically cancels all actors
- No orphan threads
- Exception propagation through scope

```haskell
withActorSystem entryPayload $ \scope router -> do
  actor1 <- spawnActor scope "compute" handler1
  actor2 <- spawnActor scope "route" handler2
  pure $ Map.fromList [("compute", actor1), ("route", actor2)]
-- Scope exit cleans up both actors
```

### JSON Message Format

Actors communicate via `Value` (Aeson JSON):
- Cross-backend compatible (same format as WASM)
- Serializable for debugging/logging
- Schema-flexible

### Completion Notification

Subgraph uses TQueue for completion events:

```haskell
data CompletionEvent = CompletionEvent
  { ceChildId :: ChildId
  , ceResult  :: result
  }

-- Parent blocks on TQueue, wakes when any child finishes
awaitAny :: Eff (Subgraph spec result ': es) (ChildId, result)
```

## Testing

```bash
cabal test exomonad-actor
```

Test modules:
- `GraphSpec.hs` - Graph execution
- `MailboxSpec.hs` - Message passing
- `SubgraphSpec.hs` - Recursive execution
- `SpawnSpec.hs` - Actor creation
- `RuntimeSpec.hs` - System lifecycle
- `IntegrationSpec.hs` - End-to-end flows

## Dependencies

- `ki` - Structured concurrency
- `stm` - TQueue for mailboxes
- `aeson` - JSON values
- `freer-simple` - Effect system
- `exomonad-core` - Graph types
