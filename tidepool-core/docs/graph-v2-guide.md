# Graph V2 DSL Guide

The V2 Graph DSL uses **record-based graph definitions** with automatic typed dispatch. This guide covers the core patterns.

## Quick Example

```haskell
-- 1. Define graph as a record parameterized by mode
data SimpleGraph mode = SimpleGraph
  { entry   :: mode :- Entry Int
  , compute :: mode :- LogicNode :@ Needs '[Int] :@ UsesEffects '[Goto Exit Int]
  , exit    :: mode :- Exit Int
  }
  deriving Generic

-- 2. Implement handlers for AsHandler mode
simpleHandlers :: SimpleGraph (AsHandler '[])
simpleHandlers = SimpleGraph
  { entry   = Proxy
  , compute = \n -> pure $ gotoExit (n + 1)
  , exit    = Proxy
  }

-- 3. Run the graph
result <- runGraph simpleHandlers (42 :: Int)
-- result == 43
```

## Core Concepts

### Modes

The `mode` parameter determines how the graph is interpreted:

| Mode | Purpose | Field Type |
|------|---------|------------|
| `AsGraph` | Validation, reflection | Node definition as-is |
| `AsHandler es` | Execution | Handler function type |

The `GraphMode` class and `:-` operator transform node definitions based on mode:

```haskell
class GraphMode mode where
  type mode :- nodeDef :: Type

-- AsGraph is identity
instance GraphMode AsGraph where
  type AsGraph :- nodeDef = nodeDef

-- AsHandler computes handler types
instance GraphMode (AsHandler es) where
  type (AsHandler es) :- nodeDef = NodeHandler nodeDef es
```

### Node Types

| Type | Purpose | Handler Signature |
|------|---------|------------------|
| `Entry a` | Graph input | `Proxy a` |
| `Exit a` | Graph output | `Proxy a` |
| `LLMNode` | Calls the LLM | `LLMHandler needs schema targets es tpl` |
| `LogicNode` | Pure routing | `needs -> Eff es (GotoChoice targets)` |

### Annotations

Annotations attach metadata to nodes using the `:@` operator (left-associative):

```haskell
myNode :: mode :- LogicNode :@ Needs '[Input] :@ UsesEffects '[Goto "next" Output]
```

| Annotation | Purpose |
|------------|---------|
| `Needs '[types...]` | Input types for the handler |
| `Schema output` | LLM output type (for LLMNode) |
| `Template tpl` | Jinja template context type (for LLMNode) |
| `UsesEffects '[effects...]` | Available effects including Goto transitions |

## Transitions with GotoChoice

Handlers return `GotoChoice targets` to declare which node to execute next.

### Constructing GotoChoice

```haskell
-- Transition to a named node
gotoChoice @"nodeName" payload

-- Exit the graph with a result
gotoExit result

-- Self-loop (re-execute current node)
gotoSelf updatedState
```

### OneOf GADT

`GotoChoice` wraps `OneOf`, a type-indexed sum type:

```haskell
GotoChoice '[To "a" Int, To "b" String, To Exit Bool]
  = GotoChoice (OneOf '[Int, String, Bool])

-- Pattern matching:
case choice of
  GotoChoice (Here n)                    -> -- n :: Int, going to "a"
  GotoChoice (There (Here s))            -> -- s :: String, going to "b"
  GotoChoice (There (There (Here b)))    -> -- b :: Bool, exiting
```

You rarely pattern match manually - `dispatchGoto` handles this automatically.

## Execution

### runGraph

The `runGraph` function executes a graph from Entry to Exit:

```haskell
runGraph
  :: ( ... constraints ... )
  => graph (AsHandler es)  -- Handlers record
  -> entryType             -- Input value
  -> Eff es exitType       -- Result
```

Internally, `runGraph`:
1. Finds the first handler that accepts the entry type (via `Needs`)
2. Calls that handler with the input
3. Uses `DispatchGoto` to dispatch on the returned `GotoChoice`
4. Recursively calls handlers until `Exit` is reached

### DispatchGoto Typeclass

The `DispatchGoto` typeclass provides automatic typed dispatch:

```haskell
class DispatchGoto graph targets es exitType where
  dispatchGoto :: graph (AsHandler es) -> GotoChoice targets -> Eff es exitType
```

Instances pattern match on `OneOf` and call the appropriate handler:
- `To Exit exitType` → return the value
- `To "nodeName" payload` → get handler via `HasField`, call it, recurse

## LLM Handlers

LLM nodes use `LLMHandler` with three variants:

```haskell
data LLMHandler needs schema targets es tpl where
  -- Before-only: build context, use implicit data flow after LLM
  LLMBefore :: (needs -> Eff es tpl) -> LLMHandler needs schema '[] es tpl

  -- After-only: route based on LLM output
  LLMAfter :: (schema -> Eff es (GotoChoice targets)) -> LLMHandler ...

  -- Both: custom context AND explicit routing
  LLMBoth
    :: Maybe (TypedTemplate tpl SourcePos)      -- system template
    -> TypedTemplate tpl SourcePos              -- user template
    -> (needs -> Eff es tpl)                    -- build context
    -> (schema -> Eff es (GotoChoice targets))  -- route
    -> LLMHandler needs schema targets es tpl
```

### Example: LLM Node with Routing

```haskell
data MyGraph mode = MyGraph
  { entry    :: mode :- Entry Message
  , classify :: mode :- LLMNode
                     :@ Needs '[Message]
                     :@ Template ClassifyTpl
                     :@ Schema Intent
                     :@ UsesEffects '[Goto "handle" Intent, Goto Exit Response]
  , handle   :: mode :- LogicNode :@ Needs '[Intent] :@ UsesEffects '[Goto Exit Response]
  , exit     :: mode :- Exit Response
  }

handlers = MyGraph
  { entry    = Proxy
  , classify = LLMBoth
      Nothing              -- no system template
      classifyTemplate     -- user template
      (\msg -> pure $ ClassifyContext msg.content)  -- build context
      (\intent -> case intent of
          QuestionIntent -> pure $ gotoChoice @"handle" intent
          DoneIntent     -> pure $ gotoExit (Response "Goodbye"))
  , handle   = \intent -> pure $ gotoExit (Response $ "Handling: " <> show intent)
  , exit     = Proxy
  }
```

## Validation

The `ValidGraphRecord` constraint bundles compile-time checks:

```haskell
type ValidGraphRecord graph =
  ( Generic (graph AsGraph)
  , ValidateEntryExit graph          -- Exactly one Entry and Exit
  , ValidateGotoTargets graph        -- All Goto targets exist
  , AllFieldsReachable graph         -- All nodes reachable from Entry
  , AllLogicFieldsReachExit graph    -- All Logic nodes can reach Exit
  , NoDeadGotosRecord graph          -- No Gotos to non-existent nodes
  )
```

Invalid graphs fail at compile time with helpful error messages.

## Mermaid Diagrams

Generate diagrams from graph types:

```haskell
import Tidepool.Graph.Mermaid (graphToMermaid)

diagram :: Text
diagram = graphToMermaid @MyGraph
```

## Reference Implementation

See `tidepool-wasm/src/Tidepool/Wasm/HabiticaRoutingGraph.hs` for a complete example with:
- Multiple LLM and Logic nodes
- Branching and retry logic
- External effect integration (Habitica API, Telegram)
