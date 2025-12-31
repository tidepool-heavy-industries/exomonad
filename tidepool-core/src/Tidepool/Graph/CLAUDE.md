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
   , "route"    := Logic :@ Needs '[Intent] :@ UsesEffects '[Goto "refund" Message, Goto "faq" Message]
   , "refund"   := LLM :@ Needs '[Message] :@ Schema Response
   , "faq"      := LLM :@ Needs '[Message] :@ Schema Response
   , Exit :<~ Response
   ]
```

### Record-Based Syntax (Servant-style)

An alternative syntax uses mode-parameterized records, inspired by Servant's
`NamedRoutes` pattern. This provides:

* **Field names as node names** - No `:=` annotation needed
* **Type-safe handler records** - The `AsHandler` mode computes handler types
* **Generic traversal** - Validate and reify via `GHC.Generics`

```haskell
import Tidepool.Graph.Generic (GraphMode(..), AsHandler)
import qualified Tidepool.Graph.Generic as G

data SupportGraph mode = SupportGraph
  { sgEntry    :: mode :- G.Entry Message
  , sgClassify :: mode :- G.LLMNode :@ Needs '[Message] :@ Template ClassifyTpl :@ Schema Intent
  , sgRoute    :: mode :- G.LogicNode :@ Needs '[Intent] :@ UsesEffects '[Goto "refund", Goto "faq"]
  , sgRefund   :: mode :- G.LLMNode :@ Needs '[Message] :@ Template RefundTpl :@ Schema Response
  , sgFaq      :: mode :- G.LLMNode :@ Needs '[Message] :@ Template FaqTpl :@ Schema Response
  , sgExit     :: mode :- G.Exit Response
  }
  deriving Generic

-- Handler record: handlers return template context, runner handles LLM call
handlers :: SupportGraph (AsHandler '[State SessionState])
handlers = SupportGraph
  { sgEntry    = Proxy @Message           -- Entry marker
  , sgClassify = \msg -> do               -- Builds ClassifyContext
      st <- get @SessionState
      pure ClassifyContext { topic = msg.content, ... }
  , sgRoute    = \intent -> do            -- Uses Goto effects
      case intent of
        RefundIntent -> goto @"refund" msg
        FaqIntent    -> goto @"faq" msg
  , sgRefund   = \msg -> pure RefundContext { ... }
  , sgFaq      = \msg -> pure FaqContext { ... }
  , sgExit     = Proxy @Response          -- Exit marker
  }
```

#### Handler Type Computation

The `NodeHandler` type family in `Generic.hs` computes handler types:

| Node Definition | Handler Type |
|-----------------|--------------|
| `G.Entry Message` | `Proxy Message` |
| `G.Exit Response` | `Proxy Response` |
| `G.LLMNode :@ Needs '[A] :@ Template T :@ Schema B` | `A -> Eff es (TemplateContext T)` |
| `G.LogicNode :@ Needs '[A] :@ UsesEffects effs` | `A -> Eff effs ()` |

Key insight: **LLM handlers return template context, not Schema output.**
The runner handles template rendering, LLM API call, and structured output parsing.

**Why LLMNode/LogicNode?** The list-based syntax uses `"name" := LLM` where `:=`
lifts `LLM` (kind `NodeKind`) to `Type`. In record syntax, field names serve as
node names, so we need `LLMNode`/`LogicNode` which have kind `Type` directly.

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           COMPILE TIME                                       │
│                                                                              │
│  Types.hs ──────► Edges.hs ──────► Validate.hs                              │
│  (DSL syntax)     (type families   (ValidGraph                              │
│                    for edge         constraint)                              │
│                    derivation)                                               │
│                                                                              │
│  Generic.hs ◄──── Template.hs ◄──── Tool.hs                                 │
│  (Servant-style   (TemplateDef      (ToolDef                                │
│   record modes)    typeclass)        typeclass)                              │
│                                                                              │
├─────────────────────────────────────────────────────────────────────────────┤
│                           RUNTIME                                            │
│                                                                              │
│  Reify.hs ──────► Mermaid.hs                                                │
│  (GraphInfo,      (diagram                                                   │
│   NodeInfo)        generation)                                               │
│                                                                              │
│  Goto.hs ─────────► Execute.hs ──────► Memory.hs                            │
│  (transition       (DispatchGoto       (persistent                           │
│   effect,           typeclass,          state effect)                        │
│   OneOf GADT)       typed dispatch)                                          │
│                                                                              │
│  Docs.hs                                                                     │
│  (template dependency documentation)                                         │
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
System Type           -- System prompt template (optional, LLM nodes)
Template Type         -- User prompt template (LLM nodes)
Vision                -- Marker for vision/image input
Tools '[Type, ...]    -- Available tools during LLM call
UsesEffects '[Effect, ...]    -- Effect stack (Logic nodes, includes Goto)
Memory Type           -- Node-private persistent state
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

### Generic.hs - Servant-Style Record Modes

Provides infrastructure for defining graphs as mode-parameterized records,
following the pattern established by Servant's `NamedRoutes`.

#### GraphMode Class

```haskell
-- Mode determines how graph record fields are interpreted
class GraphMode mode where
  type mode :- nodeDef :: Type

infixl 0 :-
```

#### Modes

```haskell
-- AsGraph: Identity mode, fields are node definitions
data AsGraph
instance GraphMode AsGraph where
  type AsGraph :- nodeDef = nodeDef

-- AsHandler: Fields are handler function types
type AsHandler :: [Effect] -> Type
data AsHandler es
instance GraphMode (AsHandler es) where
  type (AsHandler es) :- nodeDef = NodeHandler nodeDef es
```

#### Entry/Exit Markers (for records)

```haskell
-- Entry point marker (parameterized, unlike Types.hs Entry)
type Entry :: Type -> Type
data Entry inputType

-- Exit point marker
type Exit :: Type -> Type
data Exit outputType
```

Note: These are *different* from `Types.Entry` and `Types.Exit` which are
unparameterized markers for the type-level list syntax.

#### NodeHandler Type Family

Computes handler types from node definitions:

```haskell
type NodeHandler :: Type -> [Effect] -> Type
type family NodeHandler nodeDef es where
  NodeHandler (Entry a) es = Proxy a
  NodeHandler (Exit a) es = Proxy a
  NodeHandler (node :@ ann) es = NodeHandlerDispatch (node :@ ann) es '[] 'Nothing
  NodeHandler LLM es = LLMRequiresAnnotations
  NodeHandler Logic es = LogicRequiresAnnotations
```

The `NodeHandlerDispatch` accumulator peels annotations from outside-in:
- Collects `Needs` types for function parameters
- Records `Template` type for LLM nodes (returns `Eff es (TemplateContext tpl)`)
- Records `UsesEffects` stack for Logic nodes (returns `Eff effs ()`)

### Goto.hs - Transition Effect and GotoChoice

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

#### OneOf: Type-Indexed Sum Type

`OneOf` is a GADT that represents "one of these types" where position in the
type-level list encodes which type was chosen. Pattern matching is fully typed.

```haskell
-- Position encodes which type was chosen
data OneOf (ts :: [Type]) where
  Here  :: t -> OneOf (t ': ts)         -- value is first type
  There :: OneOf ts -> OneOf (t ': ts)  -- value is in rest

-- Examples for OneOf '[Int, String, Bool]:
Here 42                     :: OneOf '[Int, String, Bool]  -- an Int
There (Here "hi")           :: OneOf '[Int, String, Bool]  -- a String
There (There (Here True))   :: OneOf '[Int, String, Bool]  -- a Bool
```

The `Inject` typeclass finds the correct position automatically:

```haskell
class Inject (t :: Type) (ts :: [Type]) where
  inject :: t -> OneOf ts

-- inject @Int @'[Int, String] 42 = Here 42
-- inject @String @'[Int, String] "hi" = There (Here "hi")
```

For target injection with full `To name payload` matching (handles same payload
types at different targets correctly):

```haskell
class InjectTarget (target :: Type) (targets :: [Type]) where
  injectTarget :: PayloadOf target -> OneOf (Payloads targets)

-- injectTarget @(To "a" Int) 42  -- injects at position of To "a" Int
-- injectTarget @(To "b" Int) 42  -- injects at position of To "b" Int (different!)
```

#### GotoChoice: Handler Return Type

`GotoChoice` is what Logic node handlers return. It wraps `OneOf` with target
metadata, guaranteeing a transition is always selected.

```haskell
-- Target marker type
data To (target :: k) (payload :: Type)

-- Extract payload types from To markers
type family Payloads targets where
  Payloads '[] = '[]
  Payloads (To name payload ': rest) = payload ': Payloads rest

-- GotoChoice wraps OneOf with target metadata
newtype GotoChoice (targets :: [Type]) =
  GotoChoice { unGotoChoice :: OneOf (Payloads targets) }

-- Smart constructors
gotoChoice :: forall name payload targets. (...) => payload -> GotoChoice targets
gotoExit   :: forall payload targets. (...) => payload -> GotoChoice targets
gotoSelf   :: forall payload targets. (...) => payload -> GotoChoice targets
```

Example handler returning a choice:

```haskell
routerHandler :: Intent -> Eff es (GotoChoice '[To "refund" Msg, To Exit Response])
routerHandler intent = case intent of
  RefundIntent -> pure $ gotoChoice @"refund" msg
  DoneIntent   -> pure $ gotoExit response
```

#### Why OneOf Instead of Existentials?

The previous approach used existential types with `Dynamic`:

```haskell
-- OLD: Required unsafeCoerce to extract payloads
data GotoChoiceNode = forall a. Typeable a => GotoChoiceNode Text a
data GotoChoiceExit = forall a. Typeable a => GotoChoiceExit a
```

With `OneOf`, pattern matching gives exact typed payloads directly:

```haskell
-- NEW: Fully typed, no unsafeCoerce needed
case oneOf of
  Here payload    -> payload :: A   -- exact type known!
  There (Here p)  -> p :: B         -- exact type known!
```

#### Capturing Goto Results (Legacy)

For compatibility, there's still a Dynamic-based capture mechanism:

```haskell
data GotoResult where
  GotoNode :: Text -> Dynamic -> GotoResult  -- Named node target
  GotoExit :: Dynamic -> GotoResult          -- Exit target

runGotoCapture :: Eff (Goto target a : es) () -> Eff es (Maybe GotoResult)
```

### Execute.hs - Typed Graph Dispatch

The `DispatchGoto` typeclass provides fully typed graph execution without
Dynamic or unsafeCoerce. It pattern matches on `OneOf` to dispatch handlers.

```haskell
class DispatchGoto graph targets es exitType where
  dispatchGoto :: graph (AsHandler es) -> GotoChoice targets -> Eff es exitType
```

#### How Dispatch Works

Given `GotoChoice '[To "a" A, To "b" B, To Exit R]`:

1. Pattern match on `OneOf '[A, B, R]`
2. If `Here payload`: call handler "a" with `payload :: A`
3. If `There (Here payload)`: call handler "b" with `payload :: B`
4. If `There (There (Here result))`: return `result :: R`

Each handler returns its own `GotoChoice`, so dispatch recurses until Exit.

#### Instances

```haskell
-- Base case: Exit is the only target
instance DispatchGoto graph '[To Exit exitType] es exitType where
  dispatchGoto _ (GotoChoice (Here result)) = pure result

-- Exit first with more targets
instance (DispatchGoto graph rest es exitType)
  => DispatchGoto graph (To Exit exitType ': rest) es exitType where
  dispatchGoto _ (GotoChoice (Here result)) = pure result
  dispatchGoto graph (GotoChoice (There rest)) =
    dispatchGoto @graph @rest graph (GotoChoice rest)

-- Named node: call handler and recurse
instance
  ( KnownSymbol name
  , HasField name (graph (AsHandler es)) (payload -> Eff es (GotoChoice handlerTargets))
  , DispatchGoto graph handlerTargets es exitType
  , DispatchGoto graph rest es exitType
  ) => DispatchGoto graph (To name payload ': rest) es exitType where
  dispatchGoto graph (GotoChoice (Here payload)) = do
    let handler = getField @name graph
    nextChoice <- handler payload  -- payload has exact type!
    dispatchGoto graph nextChoice
  dispatchGoto graph (GotoChoice (There rest)) =
    dispatchGoto @graph @rest graph (GotoChoice rest)
```

#### Example Usage

```haskell
-- TestGraph: Entry(Int) → compute(+1) → Exit(Int)
data TestGraph mode = TestGraph
  { entry   :: mode :- G.Entry Int
  , compute :: mode :- G.LogicNode :@ Needs '[Int] :@ UsesEffects '[Goto Exit Int]
  , exit    :: mode :- G.Exit Int
  }

testHandlers :: TestGraph (AsHandler es)
testHandlers = TestGraph
  { entry   = Proxy @Int
  , compute = \n -> pure $ gotoExit (n + 1)
  , exit    = Proxy @Int
  }

-- Running the graph
runDispatchTest :: Int -> Int
runDispatchTest n = runPureEff $ do
  choice <- testHandlers.compute n  -- GotoChoice '[To Exit Int]
  dispatchGoto testHandlers choice  -- Returns Int directly

-- runDispatchTest 5 = 6
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

### Template.hs - Typed Prompt Templates

The `TemplateDef` typeclass defines typed templates for LLM nodes. Templates
combine a Jinja file (validated at compile time via ginger TH) with an
effectful context builder.

```haskell
class TemplateDef t where
  -- What context type this template renders
  type TemplateContext t :: Type

  -- Effects required to build the context (default: none)
  type TemplateConstraint t (es :: [Effect]) :: Constraint

  templateName :: Text
  templateDescription :: Text
  templateCompiled :: TypedTemplate (TemplateContext t) SourcePos
  buildContext :: TemplateConstraint t es => Eff es (TemplateContext t)

-- One-shot build + render
renderTemplate :: (TemplateDef t, GingerContext (TemplateContext t), TemplateConstraint t es)
               => Eff es Text
```

#### Usage (Two-Phase Pattern)

Due to TH staging, context types must be in a separate module compiled before
the TH splice:

```haskell
-- In YourContext.hs (compiled first)
data ClassifyContext = ClassifyContext { topic :: Text, categories :: Text }
instance ToGVal (...) ClassifyContext where ...

-- In YourTemplates.hs (compiled second)
import YourContext (ClassifyContext)

classifyCompiled :: TypedTemplate ClassifyContext SourcePos
classifyCompiled = $(typedTemplateFile ''ClassifyContext "templates/classify.jinja")

data ClassifyTpl

instance TemplateDef ClassifyTpl where
  type TemplateContext ClassifyTpl = ClassifyContext
  type TemplateConstraint ClassifyTpl es = (State S :> es)

  templateName = "classify"
  templateDescription = "Classify user intent"
  templateCompiled = classifyCompiled

  buildContext = do
    st <- get @S
    pure ClassifyContext { topic = "...", categories = "..." }
```

See `Example.hs` and `Example/Context.hs` for a working demonstration.

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
type GetUsesEffects :: forall k. Type -> Maybe [k]
GetUsesEffects ("foo" := Logic :@ UsesEffects '[State S, Goto "bar" X]) = 'Just '[State S, Goto "bar" X]

-- Other extractors
type GetSystem :: Type -> Maybe Type    -- System prompt template
type GetTemplate :: Type -> Maybe Type  -- User prompt template
type GetVision :: Type -> Bool
type GetTools :: Type -> [Type]
type GetMemory :: Type -> Maybe Type
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
  , niHasVision :: Bool
  , niTools :: [TypeRep]
  , niSystem :: Maybe TypeRep      -- System prompt template
  , niTemplate :: Maybe TypeRep    -- User prompt template
  , niMemory :: Maybe TypeRep      -- Node-private state type
  }

-- Typeclass for reification (implement via TH or manually)
class ReifyGraph (g :: Type) where
  reifyGraph :: GraphInfo
```

**Note**: Full automatic reification of polykinded effect stacks requires Template Haskell. The default implementation returns an empty stub.

### Mermaid.hs - Diagram Generation

Generates Mermaid flowchart syntax from graph types or `GraphInfo`.

```haskell
-- Generate Mermaid directly from a graph type (most common usage)
graphToMermaid :: (Generic (graph AsGraph), GReifyFields (Rep (graph AsGraph)), ...)
               => Proxy graph -> Text
graphToMermaidWithConfig :: ... => MermaidConfig -> Proxy graph -> Text

-- Lower-level functions using GraphInfo
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

#### Type-Driven Usage

```haskell
-- Define your graph
data MyGraph mode = MyGraph
  { entry   :: mode :- Entry Input
  , process :: mode :- LLMNode :@ Needs '[Input] :@ Schema Output
  , exit    :: mode :- Exit Output
  }
  deriving Generic

-- Generate Mermaid diagram
diagram :: Text
diagram = graphToMermaid (Proxy @MyGraph)
-- Produces:
-- flowchart TD
--     entry((start))
--     process[["process<br/>LLM"]]
--     exit__((end))
--     entry --> |Input| process
--     process --> |Output| exit__
```

#### Node Shapes
- Entry/Exit: `((circle))`
- LLM nodes: `[[double brackets]]`
- Logic nodes: `{{hexagon}}`

#### Edge Styles
- Implicit (Schema → Needs): `-->` solid arrow
- Explicit (Goto): `-->` solid arrow

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
       :@ UsesEffects '[Goto "pathA" Query, Goto "pathB" Query, Goto Exit Response]
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

### Polykind Resolution for Goto Reification

The original limitation stated that Goto targets couldn't be extracted at runtime due to polykind issues with `Effect`. The solution was found by examining how validation code handles the same problem:

```haskell
-- Key insight: Apply @Effect kind explicitly to resolve ambiguity
type GotoTargetsFromDef :: Type -> [(Symbol, Type)]
type family GotoTargetsFromDef def where
  GotoTargetsFromDef def = GotoTargetsFromEffects (GetUsesEffects @Effect def)

type HasGotoExitInDef :: Type -> Bool
type family HasGotoExitInDef def where
  HasGotoExitInDef def = HasGotoExitFromEffects (GetUsesEffects @Effect def)
```

This pattern (from `Generic.hs:693`) explicitly applies the `@Effect` kind, allowing GHC to resolve the polykind ambiguity. The `ReifyAnnotatedNode` instance for `LogicNode` now uses these type families to extract Goto targets and exit information at runtime.

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

### Kind errors with UsesEffects

**Cause**: Effect list has wrong kind (e.g., `[Type]` instead of `[Effect]`)

**Solution**: Ensure effects in `UsesEffects '[...]` are actual effectful effects with kind `(Type -> Type) -> Type -> Type`.

## Known Limitations

These are intentional simplifications for the initial implementation:

### LLM Configuration Hardcoded

The LLM model and parameters (model, temperature, etc.) are currently hardcoded
in the LLM effect interpreter:
- **Model**: Claude Haiku 4.5 (for fast iteration during development)
- **Temperature**: Default (not configurable per-node)

**Future work**: Add node-level or graph-level LLM configuration annotations.

### Retry Logic Hardcoded

Retry behavior for LLM calls is hardcoded in the LLM effect:
- Fixed retry count and backoff strategy
- No per-node retry configuration

**Future work**: Consider `Retry` annotation or graph-level retry policy.

### Vision Input Handling TBD

The `Vision` annotation marks a node as accepting image input, but:
- How images are passed (which input in `Needs`) is not yet defined
- Multi-image handling unclear
- Base64 vs URL handling unclear

**Future work**: Research multimodal input patterns and design appropriate DSL.

### Graph Execution Runtime

Graph execution is **partially implemented** via `Execute.hs`:
- `DispatchGoto` typeclass handles typed dispatch through `GotoChoice` targets
- Pattern matching on `OneOf` gives exact typed payloads without Dynamic/unsafeCoerce
- Works for Logic nodes that return `GotoChoice`

**Not yet implemented**:
- Full `runGraph` entry point that finds the first handler from Entry
- LLM node execution (requires LLM effect interpretation)
- Self-loop handling via `GotoSelf`

**Future work**: Implement full graph runner with LLM node support.

## Known Issues / Gotchas

### Record Field Names vs Template/JSON Names

**Problem**: Haskell's record field namespace is flat per module, so we use
prefixes like `ccTopic` for `ClassifyContext.topic`. But ginger's TH validates
template variables against actual record field names, not ToGVal dict keys.

This means if your template uses `{{ topic }}` but your record has `ccTopic`,
ginger's compile-time validation will fail with "field 'topic' not found".

**Current workaround**: For types used with ginger TH, use unprefixed field
names that match template variables:

```haskell
-- Good: field names match template {{ topic }} {{ categories }}
data ClassifyContext = ClassifyContext { topic :: Text, categories :: Text }

-- Bad: TH will fail because {{ topic }} doesn't match 'ccTopic'
data ClassifyContext = ClassifyContext { ccTopic :: Text, ccCategories :: Text }
```

**Same issue affects**: Aeson JSON derivation (field names become JSON keys).
The codebase uses prefixes for JSON schema types but this creates friction
with ginger templates.

**Future fix**: Custom TH that reads a field mapping, or ginger enhancement
to check ToGVal instances instead of raw record fields.

## File Inventory

| File | Lines | Purpose |
|------|-------|---------|
| Types.hs | ~270 | Core DSL syntax types |
| Generic.hs | ~380 | Servant-style record modes (GraphMode, AsHandler) |
| Goto.hs | ~460 | Goto effect, OneOf GADT, GotoChoice, Inject typeclasses |
| Execute.hs | ~140 | DispatchGoto typeclass for typed graph dispatch |
| Memory.hs | ~210 | Memory effect for persistent state |
| Template.hs | ~250 | TemplateDef typeclass for typed prompts |
| Tool.hs | ~150 | Unified tool definitions (ToolDef typeclass) |
| Edges.hs | ~375 | Edge derivation type families |
| Validate.hs | ~400 | Compile-time validation |
| Reify.hs | ~560 | Runtime graph info extraction (now includes Goto target reification) |
| Mermaid.hs | ~510 | Diagram generation (graphToMermaid, toMermaid, state/sequence diagrams) |
| Docs.hs | ~80 | Template dependency tree documentation |
| Example.hs | ~570 | Usage examples (type-level list + record syntax) |
| Example/Context.hs | ~40 | Example context types (for TH staging) |
