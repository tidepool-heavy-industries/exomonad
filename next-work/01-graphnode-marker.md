# Work Item 01: GraphNode Marker

**Priority**: High (foundation)
**Parallelizable with**: 02, 04
**Blocks**: 03, 06, 07

## Goal

Add `GraphNode` marker type to Generic/Core.hs, mirroring the LLMNode pattern.

## Files to Modify

- `haskell/dsl/core/src/Tidepool/Graph/Generic/Core.hs`

## Implementation

### 1. Add GraphNode marker (after LLMNode definition ~line 96)

```haskell
-- | Graph node marker - embeds a subgraph as a node.
--
-- GraphNode allows graphs to contain graphs, enabling compositional
-- agent design at any scale. The subgraph parameter is a graph type
-- constructor (kind: Type -> Type).
--
-- @
-- data ParentGraph mode = ParentGraph
--   { scout :: mode :- GraphNode SemanticScoutGraph :@ Input Order
--   }
-- @
--
-- The Input annotation wires to the child graph's Entry type.
-- Exit type is inferred via GetGraphExit.
type GraphNode :: (Type -> Type) -> Type
data GraphNode subgraph
```

### 2. Add to exports

```haskell
module Tidepool.Graph.Generic.Core
  ( -- ...existing exports...
  , GraphNode
  ) where
```

## Verification

```bash
cd haskell/dsl/core
cabal build tidepool-core

# Check kind signature
cabal repl tidepool-core
> :kind GraphNode
-- GraphNode :: (Type -> Type) -> Type
```

## Success Criteria

- [ ] `GraphNode` compiles with kind `(Type -> Type) -> Type`
- [ ] Exported from Generic.Core module
- [ ] Haddock documentation present
