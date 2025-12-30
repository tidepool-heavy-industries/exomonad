# Tidepool Graph DSL

Type-safe, compile-time validated state machine graphs for LLM agent orchestration.

## Overview

The Graph DSL enables declarative definition of LLM agent state machines where:
- **Nodes** are either LLM calls or pure logic
- **Edges** are derived automatically from type annotations
- **Validation** happens at compile time with clear error messages
- **Handlers** are generated with correct types via Template Haskell

```haskell
type CustomerServiceGraph = Graph
  '[ Entry :~> Message
   , "classify" := LLM :@ Needs '[Message] :@ Schema Intent
   , "route"    := Logic :@ Needs '[Intent] :@ Eff '[Goto "refund" Message, Goto "faq" Message]
   , "refund"   := LLM :@ Needs '[Message] :@ Schema Response
   , "faq"      := LLM :@ Needs '[Message] :@ Schema Response
   , Exit :<~ Response
   ]
```

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           COMPILE TIME                                       │
│                                                                              │
│  Types.hs ──────► Edges.hs ──────► Validate.hs                              │
│  (DSL syntax)     (type families   (ValidGraph                              │
│                    for edge         constraint)                              │
│                    derivation)                                               │
│                         │                                                    │
│                         ▼                                                    │
│                    TH.hs ────────────────────────────────────────────────►  │
│                    (generates typed handler records)                         │
│                                                                              │
├─────────────────────────────────────────────────────────────────────────────┤
│                           RUNTIME                                            │
│                                                                              │
│  Reify.hs ──────► Mermaid.hs        Runner.hs                               │
│  (GraphInfo,      (diagram          (graph execution,                        │
│   NodeInfo)        generation)       Goto handling)                          │
│                                                                              │
│  Goto.hs                                                                     │
│  (effectful effect for transitions)                                          │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Module Reference

### Types.hs - Core DSL Syntax

Defines the type-level language for graph declarations.

#### Graph Structure

```haskell
-- A graph is a type-level list of declarations
type Graph :: [Type] -> Type
data Graph nodes

-- Entry point: what type enters the graph
type (:~>) :: Type -> Type -> Type
data Entry :~> InputType
infixr 5 :~>

-- Exit point: what type leaves the graph
type (:<~) :: Type -> Type -> Type
data Exit :<~ OutputType
infixr 5 :<~
```

#### Node Definitions

```haskell
-- Node declaration: name and kind
type (:=) :: Symbol -> NodeKind -> Type
data name := kind
infixr 8 :=  -- Binds tighter than :@

-- Node kinds
data NodeKind
  = LLM    -- Calls language model, outputs via Schema
  | Logic  -- Pure/effectful code, transitions via Goto
```

#### Annotations (attached with `:@`)

```haskell
type (:@) :: Type -> Type -> Type
data node :@ annotation
infixl 7 :@  -- Left-associative, lower precedence than :=

-- Available annotations:
Needs '[Type, ...]    -- Input dependencies (becomes handler params)
Schema Type           -- Output type (LLM nodes)
Template Type         -- Prompt template type
Vision                -- Marker for vision/image input
Tools '[Type, ...]    -- Available tools during LLM call
When Type             -- Conditional execution guard
Eff '[Effect, ...]    -- Effect stack (Logic nodes, includes Goto)
```

#### Operator Precedence

```
:~>  :@ fixity 5 (Entry/Exit declarations)
:<~  :@ fixity 5
:=   infixr 8    (node definition - binds tightest)
:@   infixl 7    (annotation attachment)
:&   infixl 4    (graph-level annotations - binds loosest)
```

This allows natural syntax without parentheses:
```haskell
"node" := LLM :@ Needs '[A] :@ Schema B
-- Parses as: (("node" := LLM) :@ Needs '[A]) :@ Schema B
```

### Goto.hs - Transition Effect

The `Goto` effect enables typed transitions in Logic nodes.

```haskell
-- Effect definition (polykinded target)
data Goto (target :: k) (payload :: Type) :: Effect where
  GotoOp :: a -> Goto target a m ()

-- Perform a transition
goto :: forall {k} (target :: k) a es. Goto target a :> es => a -> Eff es ()

-- Special Exit target (reuses Exit from Types.hs)
-- Usage: Goto Exit ResponseType
```

#### Capturing Goto Results

```haskell
-- Run a computation, capturing which Goto was taken
data GotoResult where
  GotoNode :: Text -> Dynamic -> GotoResult  -- Named node target
  GotoExit :: Dynamic -> GotoResult          -- Exit target

runGotoCapture :: Eff (Goto target a : es) () -> Eff es (Maybe GotoResult)
```

### Memory.hs - Persistent State Effect

The `Memory` effect provides typed persistent state for graph nodes. A single
parameterized effect is used for both node-private and graph-level memory.

```haskell
-- Effect definition (parameterized by state type)
data Memory (s :: Type) :: Effect where
  GetMem    :: Memory s m s
  UpdateMem :: (s -> s) -> Memory s m ()

-- Access functions
getMem    :: forall s es. Memory s :> es => Eff es s
updateMem :: forall s es. Memory s :> es => (s -> s) -> Eff es ()

-- Optics helper (uses update, not set, to prevent clobber bugs)
modifyMem :: (Memory s :> es, Is k A_Setter) => Optic' k is s a -> (a -> a) -> Eff es ()
```

#### Usage

The same effect type works for both node-private and shared state - the
difference is just which type parameter you use:

```haskell
-- Graph definition with Memory annotations
type MyGraph = Graph
  '[ Entry :~> Message
   , "explore" := LLM :@ Needs '[Message] :@ Schema Findings :@ Memory ExploreMem
   , Exit :<~ Response
   ]
  :& Global SessionState

-- Handler with both memory types in scope
exploreHandler :: (Memory ExploreMem :> es, Memory SessionState :> es) => Eff es ()
exploreHandler = do
  myMem <- getMem @ExploreMem      -- Node's private state
  global <- getMem @SessionState   -- Graph's shared state

  updateMem @ExploreMem $ \m -> m { urlsVisited = url : m.urlsVisited }
  modifyMem @SessionState #totalSearches (+ 1)  -- With optics
```

#### Interpreters

```haskell
-- Run memory with State, returns (result, finalState)
runMemory :: s -> Eff (Memory s : es) a -> Eff es (a, s)

-- Run memory, discard final state
evalMemory :: s -> Eff (Memory s : es) a -> Eff es a
```

### Edges.hs - Edge Derivation

Type families that extract graph structure from declarations.

#### Annotation Extraction

```haskell
-- Extract Needs types from a node
type GetNeeds :: Type -> [Type]
GetNeeds ("foo" := LLM :@ Needs '[A, B] :@ Schema C) = '[A, B]

-- Extract Schema output type
type GetSchema :: Type -> Maybe Type
GetSchema ("foo" := LLM :@ Schema C) = 'Just C

-- Extract effect stack (polykinded for effectful compatibility)
type GetEff :: forall k. Type -> Maybe [k]
GetEff ("foo" := Logic :@ Eff '[State S, Goto "bar" X]) = 'Just '[State S, Goto "bar" X]

-- Other extractors
type GetWhen :: Type -> Maybe Type
type GetTemplate :: Type -> Maybe Type
type GetVision :: Type -> Bool
type GetTools :: Type -> [Type]
```

#### Goto Target Extraction

```haskell
-- Extract (target, payload) pairs from effect list
type GetGotoTargets :: forall k. [k] -> [(Symbol, Type)]
GetGotoTargets '[State S, Goto "foo" A, Goto "bar" B] = '[ '("foo", A), '("bar", B) ]

-- Check for Exit transition
type HasGotoExit :: forall k. [k] -> Bool
type GetGotoExitPayload :: forall k. [k] -> Maybe Type
```

#### Edge Derivation Rules

1. **Implicit edges** (data flow):
   - If node A has `Schema T` and node B has `T ∈ Needs`
   - Edge: A → B carrying T

2. **Explicit edges** (transitions):
   - If node A has `Goto "B" T` in its `Eff`
   - Edge: A → B carrying T

3. **Entry edges**:
   - Entry provides its type to all nodes that need it

4. **Exit edges**:
   - Nodes with `Goto Exit T` connect to Exit

### Validate.hs - Compile-Time Validation

Constraints that produce clear type errors for invalid graphs.

```haskell
-- Main validation constraint
type ValidGraph g =
  ( HasEntry g          -- Must have Entry :~> T
  , HasExit g           -- Must have Exit :<~ T
  , AllNeedsSatisfied g -- All Needs provided by Schema/Entry
  , AllGotoTargetsExist g  -- All Goto targets reference existing nodes
  )
```

#### Error Messages

```haskell
-- Missing Entry
type MissingEntryError = TypeError
  ('Text "Graph validation failed: missing Entry declaration"
   ':$$: 'Text "Add: Entry :~> YourInputType")

-- Missing Exit
type MissingExitError = TypeError
  ('Text "Graph validation failed: missing Exit declaration"
   ':$$: 'Text "Add: Exit :<~ YourOutputType")

-- Unsatisfied dependency
type UnsatisfiedNeedError nodeName needType = TypeError
  ('Text "Graph validation failed: unsatisfied dependency"
   ':$$: 'Text "Node '" ':<>: 'Text nodeName ':<>: 'Text "' needs type:"
   ':$$: 'Text "  " ':<>: 'ShowType needType
   ':$$: 'Text "But no node provides it via Schema and Entry doesn't provide it.")

-- Invalid Goto target
type InvalidGotoTargetError srcName targetName = TypeError
  ('Text "Graph validation failed: invalid Goto target"
   ':$$: 'Text "Node '" ':<>: 'Text srcName ':<>: 'Text "' has:"
   ':$$: 'Text "  Goto \"" ':<>: 'Text targetName ':<>: 'Text "\" ..."
   ':$$: 'Text "But no node named \"" ':<>: 'Text targetName ':<>: 'Text "\" exists."
   ':$$: 'Text "Fix: Create the target node or use Goto Exit for termination.")
```

### Reify.hs - Runtime Graph Info

Data types for runtime introspection (Mermaid generation, debugging).

```haskell
data GraphInfo = GraphInfo
  { giEntryType :: Maybe TypeRep
  , giExitType :: Maybe TypeRep
  , giNodes :: [NodeInfo]
  , giEdges :: [EdgeInfo]
  , giGroups :: [(Text, [Text])]  -- For Mermaid subgraphs
  }

data NodeInfo = NodeInfo
  { niName :: Text
  , niKind :: RuntimeNodeKind      -- RuntimeLLM | RuntimeLogic
  , niNeeds :: [TypeRep]
  , niSchema :: Maybe TypeRep
  , niGotoTargets :: [(Text, TypeRep)]
  , niHasGotoExit :: Bool
  , niIsConditional :: Bool
  , niHasVision :: Bool
  , niTools :: [TypeRep]
  , niTemplate :: Maybe TypeRep
  }

-- Typeclass for reification (implement via TH or manually)
class ReifyGraph (g :: Type) where
  reifyGraph :: GraphInfo
```

**Note**: Full automatic reification of polykinded effect stacks requires Template Haskell. The default implementation returns an empty stub.

### Mermaid.hs - Diagram Generation

Generates Mermaid flowchart syntax from `GraphInfo`.

```haskell
toMermaid :: GraphInfo -> Text
toMermaidWithConfig :: MermaidConfig -> GraphInfo -> Text

data MermaidConfig = MermaidConfig
  { mcDirection :: Text      -- "TD" (top-down) or "LR" (left-right)
  , mcShowTypes :: Bool      -- Show type names on edges
  , mcShowNodeKind :: Bool   -- Show LLM/Logic labels
  , mcEntryLabel :: Text     -- Label for entry node
  , mcExitLabel :: Text      -- Label for exit node
  }
```

#### Node Shapes
- Entry/Exit: `((circle))`
- LLM nodes: `[[double brackets]]`
- Logic nodes: `{{hexagon}}`

#### Edge Styles
- Implicit (Schema → Needs): `-->` solid arrow
- Explicit (Goto): `-->` solid arrow
- Conditional (from When): `-.->` dashed arrow

### TH.hs - Template Haskell

Generates typed handler records from graph definitions.

```haskell
-- Generate handler record type
deriveHandlers :: Name -> Q [Dec]

-- Generate RunnableGraph instance
deriveRunnableGraph :: Name -> Q [Dec]
```

#### Generated Code Example

For `CustomerServiceGraph`:
```haskell
data Handlers_CustomerServiceGraph = Handlers_CustomerServiceGraph
  { h_classify :: Message -> IO Intent
  , h_route :: Intent -> IO ()  -- Uses Goto effects
  , h_refund :: Message -> IO Response
  , h_faq :: Message -> IO Response
  }
```

### Runner.hs - Graph Execution

Runtime execution engine for graphs.

```haskell
class ValidGraph g => RunnableGraph g where
  type HandlersFor g :: Type
  type EntryType g :: Type
  type ExitType g :: Type
  type GraphEffects g :: [Effect]

runGraph
  :: (RunnableGraph g, ...)
  => HandlersFor g
  -> EntryType g
  -> Eff es (ExitType g)
```

#### Execution Model

1. Entry value added to available values
2. Find nodes whose `Needs` are all satisfied
3. Execute node handler
4. For LLM: add Schema output to available values
5. For Logic: follow Goto to next node or Exit
6. Repeat until Exit reached

## Usage Patterns

### Basic Linear Graph

```haskell
type LinearGraph = Graph
  '[ Entry :~> Input
   , "step1" := LLM :@ Needs '[Input] :@ Schema Middle
   , "step2" := LLM :@ Needs '[Middle] :@ Schema Output
   , Exit :<~ Output
   ]
```

### Branching with Logic Node

```haskell
type BranchingGraph = Graph
  '[ Entry :~> Query
   , "classify" := LLM :@ Needs '[Query] :@ Schema Intent
   , "router" := Logic
       :@ Needs '[Intent]
       :@ Eff '[Goto "pathA" Query, Goto "pathB" Query, Goto Exit Response]
   , "pathA" := LLM :@ Needs '[Query] :@ Schema Response
   , "pathB" := LLM :@ Needs '[Query] :@ Schema Response
   , Exit :<~ Response
   ]
```

### Fan-In (Multiple Producers)

```haskell
type FanInGraph = Graph
  '[ Entry :~> Input
   , "analyze" := LLM :@ Needs '[Input] :@ Schema Analysis
   , "enrich"  := LLM :@ Needs '[Input] :@ Schema Enrichment
   , "combine" := LLM :@ Needs '[Analysis, Enrichment] :@ Schema Output
   , Exit :<~ Output
   ]
```

### Conditional Execution

```haskell
type ConditionalGraph = Graph
  '[ Entry :~> Input
   , "check" := LLM :@ Needs '[Input] :@ Schema (Maybe Flag)
   , "conditional" := LLM
       :@ Needs '[Input]
       :@ Schema Extra
       :@ When Flag  -- Only runs if Flag present
   , "final" := LLM :@ Needs '[Input, Extra] :@ Schema Output  -- Gets Maybe Extra
   , Exit :<~ Output
   ]
```

### With Tools and Vision

```haskell
type RichGraph = Graph
  '[ Entry :~> Photo
   , "analyze" := LLM
       :@ Needs '[Photo]
       :@ Schema Description
       :@ Vision
       :@ Tools '[SearchTool, CalculatorTool]
       :@ Template AnalysisPrompt
   , Exit :<~ Description
   ]
```

### Graph-Level Annotations

```haskell
type AnnotatedGraph = Graph
  '[ Entry :~> Input
   , "a" := LLM :@ Needs '[Input] :@ Schema A
   , "b" := LLM :@ Needs '[A] :@ Schema B
   , "c" := LLM :@ Needs '[A] :@ Schema C
   , "d" := LLM :@ Needs '[B, C] :@ Schema Output
   , Exit :<~ Output
   ]
  :& Groups '[ '("intake", '["a"]), '("process", '["b", "c", "d"]) ]
  :& Requires '[IOE, Log]
```

## Design Decisions

### Why Polykinded Type Families?

effectful's `Effect` kind is `(Type -> Type) -> Type -> Type`, not `Type`. To pattern match on `Goto` inside effect lists, type families like `GetGotoTargets` must be polykinded:

```haskell
type GetGotoTargets :: forall k. [k] -> [(Symbol, Type)]
```

This enables matching on both `[Type]` lists and `[Effect]` lists.

### Why Stub for Reify.hs?

Full type-level reification of polykinded structures into runtime data hits GHC limitations:
- Kind inference struggles with `Effect` kind in instance heads
- Instance overlap becomes complex with mixed kinds

**Solution**: Provide data types and the `ReifyGraph` typeclass, but generate instances via Template Haskell which has access to the full type structure at splice time.

Note: The `AllGotoTargetsExist` validation *does* work at compile-time by pattern matching directly on the `Eff effs` annotation, avoiding the kind inference issues that affect runtime reification.

### Why Two Edge Types?

1. **Implicit edges** (Schema → Needs): Natural data flow. LLM produces output, consumers declare what they need. No explicit wiring.

2. **Explicit edges** (Goto): Control flow. Logic nodes make decisions and explicitly transition to the next state.

This mirrors how LLM agents actually work:
- LLM calls produce typed outputs that flow to consumers
- Routing logic makes explicit branching decisions

### Why Exit as Both Marker and Goto Target?

Reusing `Exit` for both `Exit :<~ T` and `Goto Exit T` provides consistency:
- The Exit declaration specifies the output type
- `Goto Exit` transitions to that exit with a payload
- Type checking ensures the Goto payload matches the Exit type

## Extension Points

### Custom Annotations

Add new annotation types in Types.hs:
```haskell
data MyAnnotation (config :: Type)
```

Add extraction in Edges.hs:
```haskell
type GetMyAnnotation :: Type -> Maybe Type
type family GetMyAnnotation node where
  GetMyAnnotation (_ := _) = 'Nothing
  GetMyAnnotation (node :@ MyAnnotation cfg) = 'Just cfg
  GetMyAnnotation (node :@ _) = GetMyAnnotation node
```

### Custom Validation

Add constraints in Validate.hs:
```haskell
type MyValidation :: Type -> Constraint
type family MyValidation g where
  MyValidation (Graph nodes) = CheckMyRule nodes
```

### Custom Reification

Implement `ReifyGraph` for your graph type, or use TH:
```haskell
instance ReifyGraph MyGraph where
  reifyGraph = GraphInfo { ... }
```

## Common Errors and Solutions

### "Expected a type, but 'LLM' has kind 'NodeKind'"

**Cause**: Operator precedence issue - `:@` binding before `:=`

**Solution**: Fixed by correct precedence (`:=` at 8, `:@` at 7). If you see this, ensure you're importing from `Tidepool.Graph.Types`.

### "Unsatisfied dependency: Node 'X' needs type Y"

**Cause**: No node provides type Y via Schema, and Entry doesn't provide it.

**Solution**:
- Add a node with `Schema Y`
- Or add Y to Entry type
- Or check for typos in type names

### "Missing Entry/Exit declaration"

**Cause**: Graph doesn't have `Entry :~> T` or `Exit :<~ T`

**Solution**: Add the missing declaration to the graph's node list.

### Kind errors with Eff

**Cause**: Effect list has wrong kind (e.g., `[Type]` instead of `[Effect]`)

**Solution**: Ensure effects in `Eff '[...]` are actual effectful effects with kind `(Type -> Type) -> Type -> Type`.

## File Inventory

| File | Lines | Purpose |
|------|-------|---------|
| Types.hs | ~220 | Core DSL syntax types |
| Goto.hs | ~100 | Goto effect for transitions |
| Memory.hs | ~210 | Memory effect for persistent state |
| Edges.hs | ~375 | Edge derivation type families |
| Validate.hs | ~270 | Compile-time validation |
| Reify.hs | ~120 | Runtime info types (stub) |
| Mermaid.hs | ~220 | Diagram generation |
| TH.hs | ~210 | Template Haskell generation |
| Runner.hs | ~280 | Graph execution engine |
| Example.hs | ~270 | Usage examples |
