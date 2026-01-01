# Plan: Tidying Agent V2 Graph Implementation

## Goal

1. **Document the existing v2 graph DSL** (in `tidepool-core/src/Tidepool/Graph/`)
2. **Convert tidying agent** from manual OODA loop to v2 graph-based

## Current State

### V2 Graph DSL (already implemented)

The v2 graph infrastructure exists in `tidepool-core/src/Tidepool/Graph/`:

- **Generic.hs** - Record-based graph definition with `GraphMode`, `AsHandler`, `NodeHandler`
- **Generic/Core.hs** - Node types: `Entry`, `Exit`, `LLMNode`, `LogicNode`
- **Execute.hs** - `runGraph`, `DispatchGoto` for automatic typed dispatch
- **Goto.hs** - `GotoChoice`, `OneOf`, `To` for typed transitions
- **Validate.hs** - Compile-time validation constraints

Reference implementation: `tidepool-wasm/src/Tidepool/Wasm/HabiticaRoutingGraph.hs`

### Tidying Agent (to convert)

Current manual OODA loop in `tidepool-tidying/src/Tidying/Loop.hs`:

```
tidyingTurn :: UserInput -> Eff es Response
  1. OBSERVE: analyzePhotos (LLM vision)
  2. EXTRACT: extractFromInput (LLM) → Extract
  3. DECIDE: decideFromExtract (pure) → (Action, Phase)
  4. ACT: actResponse (LLM) → Text
```

## Implementation Steps

### Step 1: Document V2 DSL patterns

Add documentation to existing modules or create a guide showing:
- How to define a graph record
- How to write handlers (LLM vs Logic nodes)
- How `runGraph` dispatches automatically
- Common patterns and gotchas

### Step 2: Define TidyingGraph type

```haskell
data TidyingGraph mode = TidyingGraph
  { entry          :: mode :- Entry UserInput
  , analyzePhotos  :: mode :- LLMNode :@ Needs '[UserInput] :@ Template PhotoTpl :@ Schema PhotoAnalysis
  , extractIntent  :: mode :- LLMNode :@ Needs '[UserInput, Maybe PhotoAnalysis] :@ Template ExtractTpl :@ Schema Extract
  , routeByIntent  :: mode :- LogicNode :@ Needs '[Extract, SessionState]
                           :@ UsesEffects '[Goto "actDecided" ..., Goto "actHelp" ..., ...]
  , actDecided     :: mode :- LLMNode :@ Needs '[TidyingContext] :@ Template ActTpl :@ Schema Response
  , actHelp        :: mode :- LLMNode :@ Needs '[TidyingContext] :@ Template HelpTpl :@ Schema Response
  , exit           :: mode :- Exit Response
  }
```

### Step 3: Implement handlers

Convert the functions in `Tidying/Loop.hs` to graph handlers:
- `analyzePhotos` → `analyzePhotosHandler`
- `extractFromInput` → `extractIntentHandler`
- `decideFromExtract` → `routeByIntentHandler` (Logic, returns GotoChoice)
- `actResponse` → multiple act handlers by action type

### Step 4: Wire up and test

- Create `Tidying/Graph.hs` with the new graph definition
- Update `Tidying/Agent.hs` to use `runGraph` instead of manual loop
- Verify identical behavior with existing tests

## Files to Create/Modify

### New files
- `tidepool-tidying/src/Tidying/Graph.hs` - Graph type + handlers
- `tidepool-core/docs/graph-v2-guide.md` - Documentation

### Modify
- `tidepool-tidying/src/Tidying/Agent.hs` - Use runGraph
- `tidepool-tidying/tidepool-tidying.cabal` - Add new module

## Success Criteria

1. Tidying agent works identically with graph-based implementation
2. V2 DSL patterns are clearly documented
3. Graph definition is type-safe (invalid graphs fail at compile time)
