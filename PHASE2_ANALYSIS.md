# Phase 2 Analysis: Compositional Node System

## Problem Statement

The plan aims to eliminate Entry/Exit handler boilerplate from simple Entry → Logic → Exit graphs.

**Current state (from LSPTools.hs:173-180):**
```haskell
findCallersHandlers :: FindCallersGraph (AsHandler es)
findCallersHandlers = FindCallersGraph
  { fcEntry = ()                  -- 33% ceremony
  , fcRun = findCallersLogic      -- 33% logic
  , fcExit = ()                   -- 33% ceremony
  }
```

**Target (from plan):**
```haskell
findCallersHandlers = simpleGraph @FindCallersGraph findCallersLogic
-- 100% logic, zero ceremony
```

## Critical Discovery: MCP Export Dependency

The plan's Node wrapper approach has a fundamental issue:

**MCP tool metadata lives in graph record annotations:**
```haskell
data FindCallersGraph mode = FindCallersGraph
  { fcEntry :: mode :- EntryNode FindCallersArgs
      :@ MCPExport  -- ← Tool export marker
      :@ MCPToolDef '("find_callers", "description")  -- ← Metadata
  , ...
  }
```

**How MCP discovery works (Export.hs:928-938):**
```haskell
exportMCPTools = do
  let allTools = concat
        [ reifyMCPTools (Proxy @FindCallersGraph)  -- Walks Generic Rep
        , reifyMCPTools (Proxy @ShowFieldsGraph)
        , reifyMCPTools (Proxy @DocGenGraph)
        ]
  pure $ map reifyToToolDef allTools
```

`reifyMCPTools` uses GHC.Generics to traverse the graph record structure and extract:
1. Tool name (from MCPToolDef annotation)
2. Tool description (from MCPToolDef annotation)
3. Input schema (from HasJSONSchema on entry type)
4. Entry field name (for dispatch routing)

**If we eliminate graph records, this entire system breaks.**

## Implementation Options

### Option A: Extend Node with Metadata (Plan's Approach)

```haskell
-- Add metadata to smart constructors
logic :: forall name desc input output.
         (KnownSymbol name, KnownSymbol desc)
      => (input -> Eff es (GotoChoice '[To Exit output]))
      -> Node Identity (AsHandler es)

-- Usage
findCallersTool = logic @"find_callers" @"description..." findCallersLogic
```

**Pros:**
- Achieves zero-ceremony goal
- Compositional design

**Cons:**
- Requires significant type-level metadata plumbing
- MCPReify needs complete rewrite to extract from Node instead of record
- Type inference challenges (need manual type applications)
- Breaks existing validation machinery

**Effort:** ~8-12 hours

### Option B: Helper Function for Graph Records

```haskell
-- Generic helper that fills Entry/Exit with ()
class SimpleGraphPattern graph input output es where
  simpleGraph :: (input -> Eff es (GotoChoice '[To Exit output]))
               -> graph (AsHandler es)

-- Manual instances for each simple graph (or derive via Generic)
instance SimpleGraphPattern FindCallersGraph FindCallersArgs FindCallersResult es where
  simpleGraph logic = FindCallersGraph
    { fcEntry = ()
    , fcRun = logic
    , fcExit = ()
    }

-- Usage (same as target)
findCallersHandlers = simpleGraph findCallersLogic
```

**Pros:**
- Works with existing MCP export machinery
- No changes to validation, reification, or dispatch
- Type inference works (graph type inferred from context)

**Cons:**
- Requires instance per graph OR complex Generic derivation
- Still have graph record definitions (but simplified usage)

**Effort:** ~2-3 hours for manual instances, ~4-6 hours for Generic derivation

### Option C: Documentation-Only (Simplest)

Acknowledge that 3-field records with `()` entries is acceptable ceremony:

```haskell
-- Pattern: Simple Entry → Logic → Exit graphs
-- Convention: Use () for Entry/Exit, handler for Logic
findCallersHandlers = FindCallersGraph
  { fcEntry = ()          -- ← Standard pattern
  , fcRun = findCallersLogic
  , fcExit = ()           -- ← Standard pattern
  }
```

**Pros:**
- Zero implementation work
- Clear, explicit code
- No magic or type-level complexity

**Cons:**
- Doesn't reduce LOC (5 lines vs target 1 line)
- Doesn't achieve "zero ceremony" goal

**Effort:** ~30 minutes documentation update

## Recommendation

**Option B with manual instances** as an incremental improvement:

1. Add `simpleGraph` helper class to Generic.hs
2. Write manual instances for 3 LSPTools graphs
3. Document pattern for future graphs
4. Leave Generic derivation for future work

This achieves most of the goal (1 line usage) with minimal disruption to existing systems.

**Reasoning:**
- MCP export is critical infrastructure (can't break)
- Manual instances are simple and maintainable
- Generic derivation can be added later if needed
- Validates the pattern before investing in complex machinery

## Current Status

**Completed:**
- ✅ NodeF functor definition (foundation for future Free-based composition)
- ✅ Node wrapper type (foundation, currently unused)
- ✅ Analysis of MCP export dependencies

**Blocked:**
- ❌ Direct Node usage (breaks MCP export)
- ❌ NodeHandler type family updates (not needed for Option B)
- ❌ RunNode dispatch (not needed for Option B)

**Next Step:**
Awaiting decision on which option to pursue.

## Code Locations

- MCP reification: `haskell/dsl/core/src/Tidepool/Graph/MCPReify.hs`
- Tool discovery: `haskell/control-server/src/Tidepool/Control/Export.hs:928-947`
- LSPTools graphs: `haskell/control-server/src/Tidepool/Control/LSPTools.hs`
- Graph validation: `haskell/dsl/core/src/Tidepool/Graph/Validate/RecordStructure.hs`
