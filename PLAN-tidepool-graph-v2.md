# Plan: tidepool-graph-v2 Library

## Overview

Create a new cabal library `tidepool-graph-v2` that provides an ergonomic, production-ready API for the typed graph DSL. This library will:

1. **Simplify the current v2 pattern** from `tidepool-core/src/Tidepool/Graph/`
2. **Provide migration path** for converting the legacy tidying OODA loop to graph-based
3. **Export a clean API surface** without exposing internal type machinery

## Current State Analysis

### What Exists (tidepool-core)

The v2 infrastructure already exists but is complex:

```
tidepool-core/src/Tidepool/Graph/
├── Generic.hs       # 868 lines - GraphMode, AsHandler, NodeHandler type family
├── Generic/Core.hs  # Core types: Entry, Exit, LLMNode, LogicNode
├── Execute.hs       # 496 lines - DispatchGoto, runGraph, CallHandler
├── Goto.hs          # 517 lines - Goto effect, OneOf GADT, GotoChoice
├── Edges.hs         # Type families for edge derivation
├── Validate.hs      # Compile-time validation constraints
├── Validate/RecordStructure.hs  # Reachability validation
├── Reify.hs         # Runtime graph info extraction
└── Mermaid.hs       # Diagram generation
```

### Current Pain Points

1. **Manual dispatch boilerplate** (from HabiticaRoutingGraph.hs:397-437):
   ```haskell
   runHabiticaRoutingGraph input = do
     extractResult <- extractTaskHandler input
     let GotoChoice (Here task) = extractResult  -- manual unwrap
     fetchResult <- fetchExistingHandler task
     let GotoChoice (Here existing) = fetchResult  -- manual unwrap
     ...
   ```

2. **Complex type errors** - The `TypeError` messages in Generic.hs are helpful but the underlying types are still intimidating

3. **Two handler patterns** - `LLMHandler` with `LLMBefore`/`LLMAfter`/`LLMBoth` vs logic handlers

4. **No migration guide** - Tidying uses manual OODA loop (Tidying/Loop.hs) that should be a graph

### What the Tidying App Does (to convert)

From `Tidying/Loop.hs`:
```
OBSERVE: User input (photos + text)
    → analyzePhotos (LLM vision)

ORIENT/EXTRACT: extractFromInput (LLM)
    → Returns: Extract { intent, item, choice, place, function, anchors }

DECIDE: decideFromExtract (PURE - no LLM)
    → Returns: (Action, Phase)

ACT: actResponse (LLM)
    → Generates natural language response
```

This is exactly a graph:
```
Entry(UserInput)
  → observePhotos (LLM vision)
  → extractIntent (LLM)
  → routeByIntent (Logic - pure routing)
    ├─→ handleDecided (LLM) → Exit
    ├─→ handleHelp (LLM) → Exit
    ├─→ handleContinue (LLM) → routeByIntent (self-loop)
    └─→ handleStop → Exit
```

## Proposed Package Structure

```
tidepool-graph-v2/
├── src/
│   └── Tidepool/
│       └── Graph/
│           └── V2/
│               ├── Graph.hs           # Main re-export (clean API)
│               ├── Define.hs          # Graph definition helpers
│               ├── Handler.hs         # Handler construction helpers
│               ├── Run.hs             # runGraph with better ergonomics
│               └── Convert.hs         # OODA → Graph conversion helpers
├── examples/
│   └── TidyingGraph.hs                # Tidying as a graph (reference impl)
├── test/
│   └── Spec.hs
└── tidepool-graph-v2.cabal
```

## API Design

### 1. Graph Definition (Define.hs)

```haskell
-- Simplified node definition syntax
module Tidepool.Graph.V2.Define
  ( -- * Node types
    entry, exit
  , llmNode, logicNode

    -- * Annotations (re-exports)
  , (:@), Needs, Template, Schema, UsesEffects

    -- * Goto targets
  , To, ToExit
  ) where

-- Type synonyms for common patterns
type LLMNode' needs tpl schema =
  LLMNode :@ Needs needs :@ Template tpl :@ Schema schema

type LogicNode' needs targets =
  LogicNode :@ Needs needs :@ UsesEffects targets

-- Smart constructors (documentation helpers, not needed at type level)
```

### 2. Handler Construction (Handler.hs)

```haskell
module Tidepool.Graph.V2.Handler
  ( -- * LLM handler builders
    llmHandler
  , llmHandlerWithRouting

    -- * Logic handler (just functions)
  , logicHandler  -- = id, but documents intent

    -- * Goto helpers
  , goto, gotoExit, gotoSelf
  , gotoChoice  -- TypeApplications version
  ) where

-- Simplified LLM handler construction
llmHandler
  :: (GingerContext tpl, FromJSON schema, HasJSONSchema schema)
  => TypedTemplate tpl SourcePos          -- user template
  -> (needs -> Eff es tpl)                -- build context
  -> LLMHandler needs schema '[] es tpl

llmHandlerWithRouting
  :: (GingerContext tpl, FromJSON schema, HasJSONSchema schema)
  => TypedTemplate tpl SourcePos          -- user template
  -> (needs -> Eff es tpl)                -- before: build context
  -> (schema -> Eff es (GotoChoice targets))  -- after: route
  -> LLMHandler needs schema targets es tpl
```

### 3. Execution (Run.hs)

```haskell
module Tidepool.Graph.V2.Run
  ( -- * Run graph to completion
    runGraph
  , runGraphFrom

    -- * Constraints (re-export for when needed)
  , DispatchGoto
  , CallHandler
  ) where

-- The existing runGraph from Execute.hs works, just re-export
-- with better documentation
```

### 4. OODA Conversion Helpers (Convert.hs)

```haskell
module Tidepool.Graph.V2.Convert
  ( -- * OODA pattern helpers
    OODAGraph
  , OODANode(..)

    -- * Phase machine helpers
  , PhaseGraph
  , phaseNode
  , phaseTransition
  ) where

-- A graph pattern for OODA loops
type OODAGraph input observe orient decide act output mode = ...

-- Helpers for phase-based state machines (like Tidying)
-- Encode phases as graph nodes with typed transitions
```

### 5. Main Export (Graph.hs)

```haskell
module Tidepool.Graph.V2
  ( -- * Define graphs
    module Tidepool.Graph.V2.Define

    -- * Build handlers
  , module Tidepool.Graph.V2.Handler

    -- * Run graphs
  , module Tidepool.Graph.V2.Run

    -- * Core types
  , GotoChoice(..)
  , OneOf(..)
  , LLMHandler(..)

    -- * Generate documentation
  , graphToMermaid
  ) where
```

## Implementation Steps

### Step 1: Create package structure
- Create `tidepool-graph-v2/` directory
- Create `tidepool-graph-v2.cabal` with dependency on `tidepool-core`
- Add to `cabal.project`

### Step 2: Implement core modules
- `Define.hs` - Type synonyms, re-exports
- `Handler.hs` - Simplified handler construction
- `Run.hs` - Re-export and document execution functions
- `Graph.hs` - Main export module

### Step 3: Convert tidying to graph (example)
- Create `examples/TidyingGraph.hs`
- Define `TidyingGraph` record type
- Implement handlers using new API
- Verify it compiles and produces same behavior

### Step 4: Add tests
- Graph definition tests
- Handler construction tests
- Execution tests
- Migration test (OODA loop vs graph produce same results)

### Step 5: Documentation
- Document the v2 pattern with examples
- Migration guide from OODA to graph
- Comparison with v1 type-level list syntax

## Dependencies

```cabal
build-depends:
    base >= 4.17 && < 5
  , tidepool-core          -- Re-use existing infrastructure
  , effectful-core >= 2.3
  , aeson >= 2.1
  , text >= 2.0
  , ginger >= 0.10         -- For templates
```

## Success Criteria

1. **Compile-time safety** - Invalid graphs fail to compile with clear errors
2. **Less boilerplate** - Handler definition requires fewer lines than current pattern
3. **Automatic dispatch** - No manual `GotoChoice (Here x)` pattern matching
4. **Tidying works** - The converted tidying graph produces identical behavior
5. **Documentation** - Clear examples and migration guide

## Questions to Resolve

1. **Package name**: `tidepool-graph-v2` vs `tidepool-dsl` vs just enhancing `tidepool-core`?
   - Recommendation: New package for clean API surface

2. **Backward compatibility**: Should we deprecate the type-level list syntax in tidepool-core?
   - Recommendation: Keep it, document record-based as preferred

3. **Template integration**: Should templates be part of graph definition or separate?
   - Recommendation: Keep templates external, graph just references them

4. **Phase state machines**: Special support or just use graph nodes?
   - Recommendation: Document pattern, maybe add helpers if tidying conversion reveals common needs
