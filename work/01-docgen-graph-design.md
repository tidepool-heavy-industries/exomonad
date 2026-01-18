# Task 01: DocGen Graph Structure Design

**Epic:** [00-epic-docgen-graph.md](00-epic-docgen-graph.md)
**Depends On:** None
**Blocks:** 02, 03

## Goal

Design the graph DSL representation of DocGen, defining:
- Node types and their annotations
- Data flow between nodes
- The self-loop pattern for BFS iteration
- State management (Memory vs passed data)

## Context

### Current Implementation

`DocGen.hs` implements BFS exploration as an imperative loop:

```haskell
-- Current: hand-rolled state threading
exploreLoop :: TeachState -> Eff '[ScoutGemma, LSP, Log, IO] TeachingDoc
exploreLoop state = case tsFrontier state of
  [] -> pure $ buildTeachingDoc state
  ((symbolKey, depth):rest) -> do
    symbol <- lookupSymbol symbolKey
    candidates <- pure $ extractCandidates (lsSignature symbol)
    selected <- selectRelevantSymbols (tsTopic state) symbol candidates
    resolved <- resolveTokens selected
    let state' = state
          { tsFrontier = rest ++ [(k, depth+1) | k <- resolved]
          , tsVisited = Set.insert symbolKey (tsVisited state)
          , tsGraph = Map.insert symbolKey (depth, symbol) (tsGraph state)
          }
    exploreLoop state'
```

### Target: Graph DSL

```haskell
data DocGenGraph mode = DocGenGraph
  { dgEntry    :: mode :- Entry TeachQuery
  , dgInit     :: mode :- LogicNode :@ Input TeachQuery :@ UsesEffects '[Goto "process" ProcessInput]
  , dgProcess  :: mode :- LogicNode :@ Input ProcessInput :@ Memory ExploreState
                      :@ UsesEffects '[Goto "select" SelectInput, Goto "finalize" FinalizeInput]
  , dgSelect   :: mode :- LLMNode :@ Input SelectInput :@ Template SelectTpl :@ Schema SelectOutput
  , dgExpand   :: mode :- LogicNode :@ Input ExpandInput :@ Memory ExploreState
                      :@ UsesEffects '[Goto "process" ProcessInput, Goto "finalize" FinalizeInput]
  , dgFinalize :: mode :- LogicNode :@ Input FinalizeInput :@ Memory ExploreState
                      :@ UsesEffects '[Goto Exit TeachingDoc]
  , dgExit     :: mode :- Exit TeachingDoc
  }
```

## Acceptance Criteria

- [ ] **Graph type compiles** - `DocGenGraph` type-checks with all annotations
- [ ] **Nodes have correct input/output types** - Data flows make sense
- [ ] **Self-loop is valid** - `Goto "process"` from `dgExpand` is correctly typed
- [ ] **Memory annotation works** - `ExploreState` accessible from process/expand/finalize
- [ ] **LLM node is properly annotated** - Template and Schema types defined
- [ ] **Design doc reviewed** - Graph structure makes sense before implementation

## Subtasks

### 1.1 Define Input/Output Types

Create types for data flowing between nodes:

```haskell
-- Entry input
data TeachQuery = TeachQuery
  { tqTopic :: Text
  , tqSeeds :: [Text]
  , tqBudget :: Int
  }

-- Process node decides: select next symbol or finalize
data ProcessInput = ProcessInput
  { piSymbolKey :: SymbolKey
  , piDepth :: Int
  }

-- Select node input (goes to LLM)
data SelectInput = SelectInput
  { siTopic :: Text
  , siSymbol :: LSPSymbol
  , siCandidates :: [Text]
  }

-- Select node output (from LLM)
data SelectOutput = SelectOutput
  { soSelected :: [Text]
  }

-- Expand node input (after LLM selection)
data ExpandInput = ExpandInput
  { eiSelected :: [Text]
  , eiCurrentDepth :: Int
  }

-- Finalize input (when frontier empty or budget exhausted)
data FinalizeInput = FinalizeInput
  { fiReason :: FinalizeReason
  }

data FinalizeReason = BudgetExhausted | FrontierEmpty
```

### 1.2 Define Memory State

```haskell
-- Shared state across process/expand/finalize nodes
data ExploreState = ExploreState
  { esGraph :: Map SymbolKey (Int, LSPSymbol)
  , esFrontier :: [(SymbolKey, Int)]
  , esVisited :: Set SymbolKey
  , esBudget :: Int
  , esTopic :: Text
  }
```

### 1.3 Define Graph Type

Write the full graph type with all annotations. Verify it compiles.

### 1.4 Document Node Responsibilities

| Node | Type | Responsibility |
|------|------|----------------|
| `dgEntry` | Entry | Receive TeachQuery |
| `dgInit` | LogicNode | Initialize ExploreState, seed frontier, goto process |
| `dgProcess` | LogicNode | Pop from frontier, lookup via LSP, prepare for select or finalize |
| `dgSelect` | LLMNode | Call Haiku to select relevant symbols from candidates |
| `dgExpand` | LogicNode | Resolve selected symbols via LSP, add to frontier, loop or finalize |
| `dgFinalize` | LogicNode | Build TeachingDoc from accumulated state |
| `dgExit` | Exit | Return TeachingDoc |

### 1.5 Handle LSP Effects

LSP calls (workspace/symbol, hover) happen in LogicNodes. The `LSP` effect must be in the effect stack:

```haskell
type DocGenEffects = '[Memory ExploreState, LSP, LLM, Log]

-- In dgProcess handler:
processHandler :: ProcessInput -> Eff DocGenEffects (GotoChoice '[...])
processHandler input = do
  symbol <- lspHover (piSymbolKey input)  -- LSP effect
  let candidates = extractCandidates (lsSignature symbol)
  pure $ gotoChoice @"dgSelect" SelectInput { ... }
```

## Files to Create

| File | Purpose |
|------|---------|
| `control-server/src/Tidepool/Control/Scout/Graph/Types.hs` | Input/output types, ExploreState |
| `control-server/src/Tidepool/Control/Scout/Graph.hs` | DocGenGraph type definition |

## Open Questions

1. **LSP effect threading** - How does LSP session get into the effect stack?
2. **Budget tracking** - Decrement in which node?
3. **Error handling** - What if LSP lookup fails? Graceful skip?

## Verification

```bash
# Graph type compiles
cabal build tidepool-control-server

# Can instantiate graph (even with stub handlers)
cabal repl tidepool-control-server
>>> :t DocGenGraph
```
