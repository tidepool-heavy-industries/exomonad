# Tidepool Graph DSL

Type-safe, compile-time validated state machine graphs for LLM agent orchestration.

## Overview

The Graph DSL enables declarative definition of LLM agent state machines where:
- **Nodes** are either LLM calls or pure logic
- **Edges** are derived automatically from type annotations
- **Validation** happens at compile time with clear error messages
- **Handlers** are generated with correct types via GHC.Generics

```haskell
import Tidepool.Graph.Generic (GraphMode(..), AsHandler)
import qualified Tidepool.Graph.Generic as G

data SupportGraph mode = SupportGraph
  { sgEntry    :: mode :- G.Entry Message
  , sgClassify :: mode :- G.LLMNode :@ Input Message :@ Template ClassifyTpl :@ Schema Intent
  , sgRoute    :: mode :- G.LogicNode :@ Input Intent :@ UsesEffects '[Goto "refund", Goto "faq"]
  , sgRefund   :: mode :- G.LLMNode :@ Input Message :@ Template RefundTpl :@ Schema Response
  , sgFaq      :: mode :- G.LLMNode :@ Input Message :@ Template FaqTpl :@ Schema Response
  , sgExit     :: mode :- G.Exit Response
  }
  deriving Generic
```

Key features:
* **Field names as node names** - No string annotations needed
* **Type-safe handler records** - The `AsHandler` mode computes handler types
* **Generic traversal** - Validate and reify via `GHC.Generics`

## Handler Records

Handlers are defined as a record with the same shape, using `AsHandler` mode:

```haskell
handlers :: SupportGraph (AsHandler '[State SessionState])
handlers = SupportGraph
  { sgEntry    = Proxy @Message           -- Entry marker
  , sgClassify = \msg -> do               -- Builds ClassifyContext
      st <- get @SessionState
      pure ClassifyContext { topic = msg.content, ... }
  , sgRoute    = \intent -> do            -- Returns GotoChoice
      case intent of
        RefundIntent -> pure $ gotoChoice @"sgRefund" msg
        FaqIntent    -> pure $ gotoChoice @"sgFaq" msg
  , sgRefund   = \msg -> pure RefundContext { ... }
  , sgFaq      = \msg -> pure FaqContext { ... }
  , sgExit     = Proxy @Response          -- Exit marker
  }
```

### Handler Type Computation

The `NodeHandler` type family computes handler types from node definitions:

| Node Definition | Handler Type |
|-----------------|--------------|
| `G.Entry Message` | `Proxy Message` |
| `G.Exit Response` | `Proxy Response` |
| `G.LLMNode :@ Input A :@ Template T :@ Schema B` | `A -> Eff es (TemplateContext T)` |
| `G.LogicNode :@ Input A :@ UsesEffects effs` | `A -> Eff es (GotoChoice targets)` |

**Key insight: LLM handlers return template context, not Schema output.**
The runner handles template rendering, LLM API call, and structured output parsing.

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
│  (record modes)   (TemplateDef      (ToolDef                                │
│                    typeclass)        typeclass)                              │
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
│                          │                                                   │
│                          ▼                                                   │
│                    Execute/Instrumented.hs                                   │
│                    (traced dispatch with                                     │
│                     OpenTelemetry spans)                                     │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Node Types

### Entry and Exit

```haskell
-- Entry point: what type enters the graph
gEntry :: mode :- G.Entry InputType

-- Exit point: what type leaves the graph
gExit :: mode :- G.Exit OutputType
```

### LLMNode

Calls a language model with a template and produces structured output:

```haskell
gClassify :: mode :- G.LLMNode
    :@ Input Message              -- Input type (single type, use tuple for multiple)
    :@ Template ClassifyTpl       -- Jinja template for prompt
    :@ Schema Intent              -- Structured output type
```

### LogicNode

Pure or effectful routing logic:

```haskell
gRoute :: mode :- G.LogicNode
    :@ Input Intent
    :@ UsesEffects '[Goto "gProcess" Data, Goto Exit Response]
```

## Annotations

Annotations are attached with `:@` (left-associative):

```haskell
gNode :: mode :- G.LLMNode :@ Input A :@ Template T :@ Schema B :@ Vision :@ Tools '[MyTool]
```

### Available Annotations

| Annotation | Applies To | Purpose |
|------------|-----------|---------|
| `Input T` | LLM, Logic | Input type (single type; use tuple `(A, B)` for multiple inputs) |
| `Schema T` | LLM | Output type (JSON schema derived) |
| `Template T` | LLM | User prompt template |
| `System T` | LLM | System prompt template (optional) |
| `Vision` | LLM | Enable image input |
| `Tools '[T1, T2]` | LLM | Available tools during LLM call |
| `UsesEffects '[E1, E2]` | Logic | Effects including Goto targets |
| `Memory T` | LLM, Logic | Node-private persistent state |
| `ClaudeCode model cwd` | LLM | Execute via Claude Code subprocess |

### ClaudeCode Annotation

Marks an LLM node as executed via Claude Code subprocess:

```haskell
gWork :: mode :- G.LLMNode
    :@ Input BeadInfo
    :@ Template WorkTpl
    :@ Schema WorkResult
    :@ ClaudeCode 'Sonnet ('Just "/path/to/worktree")
```

When present:
- Template is rendered and passed to `claude -p` via zellij-cc
- Claude Code spawns as subprocess with file system access
- JSON output is parsed according to Schema

## Goto and GotoChoice

Logic nodes transition to other nodes via `Goto` effects. Handlers return `GotoChoice`:

```haskell
-- Graph definition uses Goto in UsesEffects
gRoute :: mode :- G.LogicNode
    :@ Input Intent
    :@ UsesEffects '[Goto "gProcess" Data, Goto "gFallback" Data, Goto Exit Response]

-- Handler returns GotoChoice with To markers
routeHandler :: Intent -> Eff es (GotoChoice '[To "gProcess" Data, To "gFallback" Data, To Exit Response])
routeHandler intent = case intent of
  ProcessIntent d -> pure $ gotoChoice @"gProcess" d
  FallbackIntent d -> pure $ gotoChoice @"gFallback" d
  DoneIntent r -> pure $ gotoExit r
```

### GotosToTos Type Family

To avoid duplicating the target list, use `GotosToTos`:

```haskell
type MyGotos = '[Goto "gProcess" Data, Goto Exit Response]

-- Handler signature derives To markers automatically:
routeHandler :: Intent -> Eff es (GotoChoice (GotosToTos MyGotos))
-- GotosToTos MyGotos = '[To "gProcess" Data, To Exit Response]
```

### Let GHC Infer (Cleanest)

When handlers are defined inline, GHC infers the type:

```haskell
handlers = MyGraph
  { gRoute = \input -> case classify input of
      Process x -> pure $ gotoChoice @"gProcess" x
      Done r -> pure $ gotoExit r
  }
```

## OneOf: Type-Indexed Sum Type

`GotoChoice` wraps `OneOf`, a GADT representing "one of these types":

```haskell
data OneOf (ts :: [Type]) where
  Here  :: t -> OneOf (t ': ts)         -- value is first type
  There :: OneOf ts -> OneOf (t ': ts)  -- value is in rest

-- Pattern matching gives exact types:
case oneOf of
  Here payload    -> payload :: A   -- exact type known
  There (Here p)  -> p :: B         -- exact type known
```

No Dynamic, no unsafeCoerce - fully typed dispatch.

## Execute.hs - Typed Graph Dispatch

The `DispatchGoto` typeclass dispatches handlers based on `GotoChoice`:

```haskell
class DispatchGoto graph targets es exitType where
  dispatchGoto :: graph (AsHandler es) -> GotoChoice targets -> Eff es exitType
```

Pattern matches on `OneOf` to call the correct handler, recursing until Exit.

## Memory Effect

Typed persistent state for nodes:

```haskell
data Memory (s :: Type) :: Effect where
  GetMem    :: Memory s m s
  UpdateMem :: (s -> s) -> Memory s m ()

-- Usage in handler
exploreHandler = do
  nodeMem <- getMem @ExploreMem      -- Node's private state
  globalMem <- getMem @SessionState  -- Graph's shared state (via :& Global)
  updateMem @ExploreMem $ \m -> m { visited = url : m.visited }
```

## Template.hs - Typed Prompt Templates

The `TemplateDef` typeclass defines templates with compile-time validation:

```haskell
class TemplateDef t where
  type TemplateContext t :: Type
  type TemplateConstraint t (es :: [Effect]) :: Constraint

  templateName :: Text
  templateDescription :: Text
  templateCompiled :: TypedTemplate (TemplateContext t) SourcePos
  buildContext :: TemplateConstraint t es => Eff es (TemplateContext t)
```

### Two-Phase Pattern (TH Staging)

Context types must be in a separate module compiled before the TH splice:

```haskell
-- In YourContext.hs (compiled first)
data ClassifyContext = ClassifyContext { topic :: Text, categories :: Text }
instance ToGVal (...) ClassifyContext where ...

-- In YourTemplates.hs (compiled second)
classifyCompiled :: TypedTemplate ClassifyContext SourcePos
classifyCompiled = $(typedTemplateFile ''ClassifyContext "templates/classify.jinja")
```

## Tool.hs - Unified Tool Definitions

The `ToolDef` typeclass defines LLM-invocable tools:

```haskell
class ToolDef t where
  toolName :: Text
  toolDescription :: Text
  toolSchema :: Value  -- JSON Schema
  toolExecute :: Value -> Eff es (Either Text Value)
```

## Mermaid.hs - Diagram Generation

Generate Mermaid diagrams from graph types:

```haskell
diagram :: Text
diagram = graphToMermaid (Proxy @MyGraph)
```

Node shapes:
- `((circle))` - Entry/Exit
- `[[double brackets]]` - LLM nodes
- `{{hexagon}}` - Logic nodes

## LLM Handler

LLM handlers use the `LLMHandler` constructor with named record fields:

```haskell
gProcess = LLMHandler
  { llmSystem = Nothing                           -- optional system template
  , llmUser   = templateCompiled @ProcessTpl      -- user template (required)
  , llmBefore = \input -> do                      -- builds template context
      st <- get @SessionState
      pure ProcessContext { ... }
  , llmAfter  = \output -> do                     -- routes based on output
      pure $ gotoExit result
  }
```

All four fields are required:
- `llmSystem`: System template (use `Nothing` if not needed)
- `llmUser`: User template (required)
- `llmBefore`: Builds template context from input
- `llmAfter`: Routes based on LLM output

## Validation

Compile-time validation via type families:

- `HasEntry` - Must have an Entry node
- `HasExit` - Must have an Exit node
- `InputSatisfied` - Input type provided by Schema/Entry
- `AllGotoTargetsExist` - All Goto targets reference existing nodes

Error messages are clear:

```
Graph validation failed: unsatisfied dependency
Node 'gClassify' has Input: Message
But no node provides it via Schema and Entry doesn't provide it.
```

## Effect vs Effects

- **`Tidepool.Effect.*`** (singular) - Core effect infrastructure
- **`Tidepool.Effects.*`** (plural) - Integration/contrib effects (Habitica, Telegram, etc.)

## WASM Execution

The `tidepool-wasm` package enables yield/resume across FFI boundaries:

```haskell
type WasmM a = Eff '[Yield SerializableEffect EffectResult] a

-- Effects yield to TypeScript, resume with result
computeHandler :: Int -> WasmM (GotoChoice '[To Exit Int])
computeHandler n = do
  logInfo $ "Computing: " <> T.pack (show n)  -- Yields to TS
  pure $ gotoExit (n + 1)
```

Uses `freer-simple` for reified continuations (required for WASM).

## File Inventory

| File | Purpose |
|------|---------|
| Generic.hs | Record modes (GraphMode, AsHandler, NodeHandler) |
| Goto.hs | Goto effect, OneOf GADT, GotoChoice |
| Execute.hs | DispatchGoto typeclass |
| Execute/Instrumented.hs | Traced dispatch with OpenTelemetry |
| Memory.hs | Memory effect for persistent state |
| Template.hs | TemplateDef typeclass |
| Tool.hs | ToolDef typeclass |
| Edges.hs | Edge derivation type families |
| Validate.hs | Compile-time validation |
| Reify.hs | Runtime graph info extraction |
| Mermaid.hs | Diagram generation |
| Types.hs | Core type definitions (internal) |
