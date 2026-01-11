# V3 Hylomorphism Implementation Plan

## Overview

This plan covers the implementation of full recursive TDD execution where:
- Scaffold analyzes specs and spawns child graphs for subsystems
- Children run TDD in parallel
- Results merge up the tree
- Parent completes after all children

## Architecture

```
┌───────────────────────────────────────────────────────────────────┐
│ TDDGraph (recursive)                                              │
│                                                                   │
│  v3Entry(Spec)                                                    │
│       ↓                                                           │
│  v3Scaffold ─────[if decomposable]─────► spawnSelf(childSpec)    │
│       │                                        │                  │
│       │                                        └──► child graph   │
│       ↓ InitWork                                        ↓         │
│  v3Fork ────────► v3TDDWriteTests ─────► TestsReady    ↓         │
│       │                                       │        ↓         │
│       └─────────► v3ImplBarrier ◄─────────────┘        ↓         │
│                       │                                ↓         │
│                       │ ◄───────── MergeComplete ◄─────┘         │
│                       ↓ (all dependencies satisfied)              │
│                  v3Impl ──[retry loop]                            │
│                       ↓                                           │
│                  v3TDDReviewImpl ──[decision tools]               │
│                       │                                           │
│                       ├── Approved ──► v3Merger                   │
│                       └── MoreTests ─► v3TDDWriteTests            │
│                                               ↓                   │
│                                        v3Exit(MergeComplete)      │
└───────────────────────────────────────────────────────────────────┘
```

## Phase 1: V3 Effect Stack

**Goal:** Define the effect stack for V3 handlers.

**File:** `types-first-dev/src/TypesFirstDev/Effects.hs`

```haskell
type V3Effects =
  '[ Session                        -- Claude Code via mantle
   , Subgraph Spec MergeComplete    -- Child spawning
   , Worktree                       -- Git worktree management
   , Build                          -- Cabal build operations
   , Memory TDDMem                  -- TDD shared memory
   , Memory ImplMem                 -- Impl shared memory
   , IO
   ]

runV3Effects :: SubgraphState Spec MergeComplete -> Eff V3Effects a -> IO a
```

**Dependencies:**
- tidepool-session-executor (Session effect)
- tidepool-worktree-executor (Worktree effect)
- tidepool-cabal-executor (Build effect)
- tidepool-actor (Memory interpreter)

---

## Phase 2: V3 Handlers

**Goal:** Implement handler functions for each node.

**File:** `types-first-dev/src/TypesFirstDev/Handlers/V3.hs`

### 2.1 Scaffold Handler

```haskell
scaffoldHandler :: ScaffoldInput -> Eff V3Effects ScaffoldOutput
scaffoldHandler input = do
  -- 1. Build template context
  let ctx = ScaffoldTemplateCtx { spec = input.siSpec, parent = input.siParent }

  -- 2. Call Claude (via Session effect)
  result <- runSession sessionConfig (renderTemplate scaffoldCompiled ctx)

  -- 3. Parse structured output
  case parseScaffoldExit result of
    ClarificationNeeded c -> pure $ gotoExit (ClarificationNeededExit c)
    InitWork work -> do
      -- 4. If children needed, spawn them
      forM_ work.childSpecs $ \childSpec ->
        spawnSelf @Spec @MergeComplete childSpec

      -- 5. Route to Fork
      pure $ gotoChoice @"v3Fork" work
```

### 2.2 TDD Write Tests Handler

Uses decision tools for structured output:

```haskell
tddWriteTestsHandler :: TDDWriteTestsInput -> Eff V3Effects TDDWriteTestsOutput
tddWriteTestsHandler input = do
  mem <- getMem @TDDMem

  -- Configure decision tools from the output type
  let decisionTools = toDecisionTools @TDDWriteTestsExit
  let config = sessionConfig { soDecisionTools = Just decisionTools }

  -- Call Claude with decision tools
  result <- runSession config prompt

  -- Parse the tool call back to sum type
  case parseToolCall result.toolCall of
    Right (TestsReady payload) -> do
      updateMem @TDDMem (\m -> m { tddTestsWritten = payload.tests })
      pure $ gotoArrive @"v3ImplBarrier" payload
    Right (InvalidScaffold reason) ->
      pure $ gotoChoice @"v3Scaffold" (input with reason)
```

### 2.3 Impl Barrier Handler

Waits for both TDDWriteTests AND all children:

```haskell
implBarrierHandler :: HList '[TestsReadyPayload] -> Eff V3Effects ImplBarrierOutput
implBarrierHandler (testsReady ::: HNil) = do
  -- Collect all pending children
  pending <- getPending @Spec @MergeComplete

  -- Wait for each child to complete
  childResults <- collectAll pending

  -- All dependencies satisfied - route to Impl
  pure $ gotoChoice @"v3Impl" ImplInput
    { iiTests = testsReady
    , iiChildrenMerged = childResults
    }

collectAll :: [ChildHandle] -> Eff V3Effects [MergeComplete]
collectAll handles = go handles []
  where
    go [] acc = pure (reverse acc)
    go _ acc = do
      (_, result) <- awaitAny @Spec @MergeComplete
      go (tail handles) (result : acc)  -- simplified
```

### 2.4 Impl Handler (Self-Loop)

```haskell
implHandler :: ImplInput -> Eff V3Effects ImplOutput
implHandler input = do
  mem <- getMem @ImplMem
  let attempt = mem.imAttempt

  -- Check retry limit
  when (attempt >= maxRetryAttempts) $
    pure $ gotoExit (Stuck "Max retries exceeded")

  -- Call Claude to implement
  result <- runSession config prompt

  -- Run build + tests
  buildResult <- runBuild

  case buildResult of
    TestsPassed ->
      pure $ gotoChoice @"v3TDDReviewImpl" (TDDReviewImplInput result)
    TestsFailed err -> do
      updateMem @ImplMem (\m -> m { imAttempt = attempt + 1, imLastError = err })
      pure $ gotoChoice @"v3Impl" (input with err)  -- self-loop
```

### 2.5 TDD Review Impl Handler (Decision Tools)

```haskell
tddReviewImplHandler :: TDDReviewImplInput -> Eff V3Effects TDDReviewImplOutput
tddReviewImplHandler input = do
  -- Use decision tools for structured Approve/MoreTests/Reject
  let decisionTools = toDecisionTools @TDDReviewDecision

  result <- runSession config { soDecisionTools = Just decisionTools } prompt

  case parseToolCall result.toolCall of
    Right (Approved notes) ->
      pure $ gotoChoice @"v3Merger" (MergerInput notes)
    Right (MoreTests reasons) ->
      pure $ gotoChoice @"v3TDDWriteTests" (TDDWriteTestsInput reasons)
    Right (Reject reason) ->
      pure $ gotoExit (Rejected reason)
```

### 2.6 Merger Handler

```haskell
mergerHandler :: MergerInput -> Eff V3Effects MergerOutput
mergerHandler input = do
  -- Create commit + PR
  _ <- runSession config "Create merge request..."

  -- On success, return MergeComplete
  pure $ gotoExit MergeComplete
    { mcCommit = ...
    , mcBranch = ...
    }
```

---

## Phase 3: Actor Wiring

**Goal:** Wire handlers to actor runtime with proper effect interpretation.

**File:** `types-first-dev/src/TypesFirstDev/Handlers/ActorWiring.hs`

```haskell
buildV3ActorHandlers :: IO (Map Text HandlerBuilder)
buildV3ActorHandlers = withRecursiveGraph @Spec @MergeComplete $ \subgraphState wire -> do

  -- Build effect interpreter
  let interpret :: Eff V3Effects a -> IO a
      interpret = runV3Effects subgraphState

  -- Build handler map
  let handlers = Map.fromList
        [ ("v3Entry", pureHandler (Proxy @ScaffoldInput))
        , ("v3Scaffold", effHandler interpret scaffoldHandler)
        , ("v3Fork", forkHandler @'[To "v3TDDWriteTests" ..., To "v3ImplBarrier" ...]
                                 interpret forkH)
        , ("v3TDDWriteTests", effHandler interpret tddWriteTestsHandler)
        , ("v3ImplBarrier", barrierHandler @'[From "v3TDDWriteTests" TestsReadyPayload]
                                           interpret implBarrierHandler)
        , ("v3Impl", effHandler interpret implHandler)
        , ("v3TDDReviewImpl", effHandler interpret tddReviewImplHandler)
        , ("v3Merger", effHandler interpret mergerHandler)
        , ("v3Rebaser", effHandler interpret rebaserHandler)
        , ("v3Exit", pureHandler (Proxy @MergeComplete))
        ]

  -- Wire the recursion (CRITICAL: before running!)
  wire $ \childSpec -> runGraphAsActors handlers (toJSON childSpec)

  pure handlers
```

---

## Phase 4: Hub Integration

**Goal:** Register sessions and nodes with mantle-hub for visualization.

### 4.1 Hub Client

```haskell
-- In tidepool-session-executor or new package
data HubClient = HubClient { hcBaseUrl :: Text }

registerSession :: HubClient -> Spec -> IO SessionInfo
registerSession client spec = do
  resp <- HTTP.post (hcBaseUrl client <> "/api/sessions")
                    (toJSON SessionRegister { srBranch = ..., srPrompt = ... })
  pure (decode resp)

registerNode :: HubClient -> Text -> Text -> Spec -> IO NodeInfo
registerNode client sessionId parentNodeId childSpec = do
  resp <- HTTP.post (hcBaseUrl client <> "/api/sessions/" <> sessionId <> "/nodes")
                    (toJSON NodeRegister { nrParent = parentNodeId, nrPrompt = ... })
  pure (decode resp)
```

### 4.2 Integration in Scaffold

```haskell
scaffoldHandler input = do
  -- ... existing logic ...

  -- When spawning children, register with hub
  forM_ work.childSpecs $ \childSpec -> do
    -- Register node in hub
    nodeInfo <- sendM $ registerNode hubClient sessionId parentId childSpec

    -- Spawn the child
    spawnSelf @Spec @MergeComplete childSpec
```

---

## Phase 5: E2E Test

**Goal:** Run a full hylomorphism with a multi-criterion spec.

**Test Spec:**
```yaml
# specs/url-shortener-decomposed.yaml
description: |
  URL shortener with decomposable subsystems.

acceptanceCriteria:
  - id: url-1
    text: Shorten URLs and return short codes
  - id: url-2
    text: Look up original URL from short code
  - id: persist-1
    text: Persist mappings to storage
  - id: persist-2
    text: Retrieve mappings from storage
  - id: api-1
    text: HTTP POST /shorten endpoint
  - id: api-2
    text: HTTP GET /{code} redirect

# Expected decomposition:
# Scaffold → [UrlService, Persistence, Server]
# Each child runs full TDD
```

**Test Runner:**
```haskell
spec :: Spec
spec = describe "V3 Hylomorphism" $ do
  it "decomposes and parallelizes TDD for multi-criterion specs" $ do
    handlers <- buildV3ActorHandlers

    let spec = Spec
          { sId = "url-shortener-decomposed"
          , sDescription = "URL shortener..."
          , sAcceptanceCriteria = [...]
          }

    result <- runGraphAsActors handlers (toJSON spec)

    result `shouldSatisfy` \case
      MergeComplete {} -> True
      _ -> False
```

---

## Implementation Order

1. **V3 Effect Stack** (Phase 1) - ~1 day
   - Define effect type list
   - Write interpreter stack

2. **Scaffold + TDD Handlers** (Phase 2.1-2.2) - ~2 days
   - Scaffold with child spawning
   - TDDWriteTests with decision tools

3. **ImplBarrier + Impl** (Phase 2.3-2.4) - ~2 days
   - Barrier with child awaiting
   - Impl with self-loop retry

4. **Review + Merger** (Phase 2.5-2.6) - ~1 day
   - TDDReviewImpl with decision tools
   - Merger with exit

5. **Actor Wiring** (Phase 3) - ~1 day
   - Build handler map
   - Wire recursion

6. **Hub Integration** (Phase 4) - ~1 day
   - Register sessions/nodes
   - Stream events to hub

7. **E2E Test** (Phase 5) - ~1 day
   - Multi-criterion spec
   - Full tree execution

**Total: ~9 days of implementation work**

---

## Open Questions

1. **Rebaser trigger**: How does a sibling's MergeComplete trigger Rebaser in other siblings?
   - Option A: Broadcast via hub (push notification)
   - Option B: Poll hub for sibling state
   - Option C: Parent orchestrates rebase cascade

2. **Worktree isolation**: Each child needs its own git worktree.
   - Create worktree at spawn time?
   - Pass worktree path in Spec?

3. **Resource limits**: How many concurrent children?
   - Semaphore in Subgraph interpreter?
   - Global limit vs per-parent limit?

4. **Failure propagation**: What happens when a child fails?
   - Fail the parent immediately?
   - Continue with other children?
   - Retry the failed child?

---

## Dependencies

| Package | Purpose | Status |
|---------|---------|--------|
| tidepool-session-executor | Session effect + mantle integration | Done |
| tidepool-worktree-executor | Git worktree management | Done |
| tidepool-cabal-executor | Cabal build operations | Done |
| tidepool-actor | Fork/Barrier/Subgraph interpreters | Done |
| mantle-hub | Session visualization | Done |
| types-first-dev | V3 graph + types | Done |

All dependencies are in place. Only handlers need implementation.
