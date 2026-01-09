# tidepool-parallel

Parallel execution backend for tidepool graphs using ki for structured concurrency.

## Core Concepts

### Fan-out with GotoAll

`GotoAll` represents parallel dispatch to multiple targets (product type semantics):

```haskell
-- Type-safe parallel targets
let payReq = PaymentReq orderId 100.0
    invReq = InventoryReq orderId ["item"]
    targets :: GotoAll '[To "payment" PaymentReq, To "inventory" InventoryReq]
    targets = gotoAll (payReq ::: invReq ::: HNil)

-- Extract for dispatch
let extracted = extractTargets targets  -- [(Text, Value)]
```

### Fan-in with Merge

`Merge` collects results from parallel workers, grouped by correlation key:

```haskell
-- Create accumulator expecting specific sources
acc <- newMergeAccumulator @'[From "payment" PayResult, From "inventory" InvResult] @OrderId

-- Add results as they arrive
_ <- addResult acc orderId "payment" (toJSON payResult)
completed <- addResult acc orderId "inventory" (toJSON invResult)
-- completed == Just (HS.fromList ["payment", "inventory"])

-- Extract typed results
Just (Right (pay ::: inv ::: HNil)) <- getCompletedResults @'[...] acc orderId
```

### Correlation Keys

Results are grouped by correlation key, enabling multiple concurrent fan-outs:

```haskell
instance CorrelateBy OrderId PayResult where
  correlationKey = (.orderId)

instance CorrelateBy OrderId InvResult where
  correlationKey = (.orderId)
```

## Usage Pattern

```haskell
-- 1. Create fan-out with GotoAll
let targets = gotoAll (payReq ::: invReq ::: HNil)
    extractedTargets = extractTargets targets

-- 2. Dispatch workers in parallel
results <- Ki.scoped $ \scope ->
  dispatchAll defaultParallelConfig scope workerDispatch extractedTargets

-- 3. Collect results in accumulator
acc <- newMergeAccumulator @'[From "payment" PayResult, From "inventory" InvResult]
forM_ results $ \wr -> addResult acc correlationKey wr.wrSource wr.wrPayload

-- 4. Extract typed results when complete
Just (Right (payResult ::: invResult ::: HNil)) <-
  getCompletedResults @'[From "payment" PayResult, From "inventory" InvResult] acc key
```

## Modules

| Module | Purpose |
|--------|---------|
| `Tidepool.Parallel` | Re-exports, main entry point |
| `Tidepool.Parallel.Dispatch` | Worker spawning with ki, parallel execution |
| `Tidepool.Parallel.Merge` | Accumulator, typed extraction |
| `Tidepool.Parallel.Retry` | Configurable retry logic for workers |

## Key Types

- `GotoAll targets` - Product type for parallel fan-out
- `Merge sources` - Type annotation for fan-in nodes
- `From name payload` - Source marker in Merge
- `MergeAccumulator key` - STM-backed result collector
- `WorkerResult` - Result from a dispatched worker
- `ParallelConfig` - Configuration (retry policy, etc.)

## Design Notes

- **Product vs Sum**: `GotoAll` is product (all targets), `GotoChoice` is sum (one target)
- **Structured concurrency**: Uses ki for automatic cleanup on exceptions
- **Type-safe extraction**: `ExtractMergeResults` typeclass parses JSON to typed HList
- **Duplicate handling**: Later results overwrite earlier (supports retry)
