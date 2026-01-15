# Work Item 06: GraphNode Dispatch

**Priority**: Medium-High
**Depends on**: 01 (GraphNode marker), 03 (extraction families)
**Parallelizable with**: 04, 05
**Blocks**: 07 (full integration)

## Goal

Add dispatch support for `GraphNode` in Interpret.hs so that child graphs can be executed as nodes within parent graphs.

## Files to Modify

- `haskell/dsl/core/src/Tidepool/Graph/Interpret.hs`
- `haskell/dsl/core/src/Tidepool/Graph/Generic/Core.hs` (NodeHandler)

## Implementation

### 1. Add NodeHandler instance for GraphNode (Generic/Core.hs)

In the `NodeHandler` type family section:

```haskell
-- | GraphNode handler: runs child graph to completion.
--
-- The handler receives the graph's Entry type and returns its Exit type.
-- With Input annotation, the input is transformed before entry.
type instance NodeHandler mode (GraphNode subgraph) =
  NodeHandler mode (GraphNode subgraph :@ Input (GetGraphEntry subgraph))

type instance NodeHandler (AsHandler es) (GraphNode subgraph :@ Input inputT) =
  inputT -> Eff es (GetGraphExit subgraph)
```

### 2. Add GraphNode execution in Interpret.hs

Add imports:

```haskell
import Tidepool.Graph.Generic.Core (GraphNode)
import Tidepool.Graph.Edges (GetGraphEntry, GetGraphExit)
```

Add execution function:

```haskell
-- ════════════════════════════════════════════════════════════════════════════
-- GRAPHNODE EXECUTION
-- ════════════════════════════════════════════════════════════════════════════

-- | Execute a GraphNode by running the child graph to completion.
--
-- The child graph's handlers must be provided. The input is passed to
-- the child graph's Entry, and execution continues until Exit is reached.
--
-- @
-- result <- executeGraphNode childHandlers input
-- @
executeGraphNode
  :: forall childGraph es.
     ( ValidGraphRecord childGraph
     , DispatchGoto childGraph (GotoTargetsFromGraph childGraph) es (GetGraphExit childGraph)
     )
  => childGraph (AsHandler es)
  -> GetGraphEntry childGraph
  -> Eff es (GetGraphExit childGraph)
executeGraphNode handlers input = runGraph handlers input

-- | Type family to get goto targets from a graph's entry.
type GotoTargetsFromGraph :: (Type -> Type) -> [Type]
type family GotoTargetsFromGraph graph where
  GotoTargetsFromGraph g = GotoTargetsFromEntry (Rep (g AsGraph))
```

### 3. Add DispatchGoto instance for GraphNode targets

```haskell
-- | Dispatch when target is a GraphNode.
--
-- This allows Goto to target a GraphNode field, executing the child graph.
instance ( ValidGraphRecord childGraph
         , DispatchGoto childGraph childTargets es exitType
         , DispatchGoto graph rest es exitType
         )
      => DispatchGoto graph (To name (GraphNode childGraph payload) ': rest) es exitType where
  dispatchGoto handlers (Here (To payload)) = do
    -- Get child graph handlers from parent record
    let childHandlers = -- extract from handlers using field name
    executeGraphNode childHandlers payload
  dispatchGoto handlers (There rest) = dispatchGoto handlers rest
```

### 4. Add CallHandler instance for GraphNode

```haskell
-- | Call a GraphNode handler.
instance ( ValidGraphRecord childGraph
         , Member (Reader (childGraph (AsHandler es))) es
         )
      => CallHandler (GraphNode childGraph :@ Input inputT) where
  type HandlerInput (GraphNode childGraph :@ Input inputT) = inputT
  type HandlerOutput (GraphNode childGraph :@ Input inputT) es = GetGraphExit childGraph

  callHandler input = do
    childHandlers <- ask @(childGraph (AsHandler es))
    executeGraphNode childHandlers input
```

## Verification

```bash
cd haskell/dsl/core
cabal build tidepool-core

# Verify types
cabal repl tidepool-core
> :t executeGraphNode
-- executeGraphNode :: (ValidGraphRecord childGraph, ...)
--   => childGraph (AsHandler es) -> GetGraphEntry childGraph -> Eff es (GetGraphExit childGraph)
```

## Success Criteria

- [ ] `NodeHandler` computes correct type for GraphNode
- [ ] `executeGraphNode` runs child graph to completion
- [ ] GraphNode can be target of Goto
- [ ] Child graph Exit type flows correctly to parent
