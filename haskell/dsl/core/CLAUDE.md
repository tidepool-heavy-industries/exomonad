# ExoMonad Graph DSL

Type-safe, compile-time validated state machine graphs for LLM agent orchestration.

## Overview

The Graph DSL enables declarative definition of LLM agent state machines where:
- **Nodes** are either LLM calls or pure logic
- **Edges** are derived automatically from type annotations
- **Validation** happens at compile time with clear error messages
- **Handlers** are generated with correct types via GHC.Generics

```haskell
import ExoMonad.Graph.Generic (GraphMode(..), AsHandler)
import qualified ExoMonad.Graph.Generic as G

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
  { sgEntry    = ()                       -- Entry marker (changed from Proxy in Phase 1)
  , sgClassify = \msg -> do               -- Builds ClassifyContext
      st <- get @SessionState
      pure ClassifyContext { topic = msg.content, ... }
  , sgRoute    = \intent -> do            -- Returns GotoChoice
      case intent of
        RefundIntent -> pure $ gotoChoice @"sgRefund" msg
        FaqIntent    -> pure $ gotoChoice @"sgFaq" msg
  , sgRefund   = \msg -> pure RefundContext { ... }
  , sgFaq      = \msg -> pure FaqContext { ... }
  , sgExit     = ()                       -- Exit marker (changed from Proxy in Phase 1)
  }
```

### Simple MCP Tools (GraphEntries + Return)

For simple tools, use the simplified pattern with `GraphEntries` type family and `Return` effect:
1. **Graph type** - A newtype with a single LogicNode using `Return` effect
2. **GraphEntries instance** - Declares the external MCP tool name and metadata
3. **Logic function** - Uses `returnValue` to return the result

No Entry/Exit nodes needed:

```haskell
-- Graph definition (single node, no Entry/Exit ceremony)
newtype FindCallersGraph mode = FindCallersGraph
  { fcRun :: mode :- LogicNode
      :@ Input FindCallersArgs
      :@ UsesEffects '[Return FindCallersResult]
  }
  deriving Generic

-- Entry point declaration (tool name, node field, input type, description)
type instance GraphEntries FindCallersGraph =
  '[ "find_callers" ':~> '("fcRun", FindCallersArgs, "Find actual call sites...") ]

-- Logic function (uses Return effect instead of Goto Exit)
findCallersLogic
  :: (Member LSP es, Member Log es, Member (Return FindCallersResult) es, LastMember IO es)
  => FindCallersArgs
  -> Eff es FindCallersResult
findCallersLogic args = do
  -- ... implementation ...
  returnValue result  -- Returns the result directly
```

**How it works:**
- `reifyGraphEntries (Proxy @FindCallersGraph)` extracts tool metadata from `GraphEntries` instance
- MCP handler runs `runReturn (findCallersLogic args)` to extract the result
- No Entry/Exit nodes needed - the `Return` effect terminates execution with a value

**When to use:**
- Single-node tools (most MCP tools)
- Any graph where Entry/Exit ceremony is overhead
- Tool needs MCP export

**When NOT to use:**
- Complex multi-node graphs that need routing between nodes
- Graphs with LLM nodes (need before/after functions and full handler record)
- Graphs with fork/barrier patterns

### Legacy Pattern (Entry/Exit)

For backwards compatibility, the old Entry/Exit pattern with `MCPExport` annotation still works.
Use `reifyMCPTools` instead of `reifyGraphEntries` for graphs using this pattern.

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
│  Goto.hs ─────────► Interpret.hs ────► Memory.hs                            │
│  (transition       (DispatchGoto       (persistent                           │
│   effect,           typeclass,          state effect)                        │
│   OneOf GADT)       typed dispatch)                                          │
│                          │                                                   │
│                          ▼                                                   │
│                    Interpret/Instrumented.hs                                 │
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

### ForkNode and BarrierNode (Parallel Fan-Out)

For parallel execution, use the Fork/Barrier pattern:

```haskell
data ParallelGraph mode = ParallelGraph
  { entry   :: mode :- G.Entry Task
  , fork    :: mode :- G.ForkNode
      :@ Input Task
      :@ Spawn '[To "worker1" Task, To "worker2" Task]
      :@ Barrier "join"
  , worker1 :: mode :- G.LLMNode
      :@ Input Task
      :@ Template W1Tpl
      :@ Schema Result1
  , worker2 :: mode :- G.LLMNode
      :@ Input Task
      :@ Template W2Tpl
      :@ Schema Result2
  , join    :: mode :- G.BarrierNode
      :@ Awaits '[Result1, Result2]
      :@ UsesEffects '[Goto Exit (Result1, Result2)]
  , exit    :: mode :- G.Exit (Result1, Result2)
  }
```

**Annotations:**
- `Spawn '[To "worker1" A, To "worker2" B]` - Define parallel spawn targets
- `Barrier "joinNodeName"` - Name of barrier that collects results
- `Awaits '[Result1, Result2]` - Expected result types at barrier
- `Arrive "barrierName"` - (On workers) Deposit result at named barrier

**How it works:**
1. ForkNode receives input, routes copies to all workers
2. Workers run independently (can be on different machines)
3. Each worker implicitly calls `Arrive` when done
4. BarrierNode blocks until all workers arrive
5. Results collected and passed to barrier handler

**Compile-time validation:**
- Spawn targets must reference existing fields
- Awaits types must match worker output types
- Workers must reach their Barrier

> **Source**: `Graph/Types.hs` (annotations), `Graph/Validate/ForkBarrier.hs` (validation), `runtime/actor/src/ExoMonad/Actor/Fork.hs` (execution)

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

Marks an LLM node as executed via Claude Code subprocess instead of the standard LLM API:

```haskell
gWork :: mode :- G.LLMNode
    :@ Input IssueInfo
    :@ Template WorkTpl
    :@ Schema WorkResult
    :@ ClaudeCode 'Sonnet ('Just "/path/to/worktree")
```

**Type parameters:**
- `model :: ModelChoice` - One of `'Haiku`, `'Sonnet`, `'Opus`
- `cwd :: Maybe Symbol` - Working directory path, or `'Nothing` for current dir

**How it works:**
1. Handler's `before` function builds template context from input
2. System and user templates are rendered and concatenated
3. Prompt is passed to `claude -p` via the exomonad interpreter
4. Claude Code spawns with file system access to `cwd`
5. JSON output is parsed according to `Schema` type
6. Handler's `after` function routes based on parsed output

**Handler type:** When `ClaudeCode` is present, the `NodeHandler` type family produces a `ClaudeCodeLLMHandler` instead of regular `LLMHandler`:

```haskell
-- Standard LLM handler (API call)
type LLMHandler payload schema targets es tpl

-- ClaudeCode handler (subprocess)
type ClaudeCodeLLMHandler model cwd payload schema targets es tpl
```

Both have the same 4 fields (`llmSystem`, `llmUser`, `llmBefore`, `llmAfter`), but dispatch differently at runtime.

**Use cases:**
- Tasks requiring file system access (code generation, refactoring)
- Long-running operations that benefit from Claude Code's context management
- Tasks needing Claude Code's built-in tools (file editing, bash execution)

> **Source**: `exomonad-core/src/ExoMonad/Graph/Types.hs` (ClaudeCode, ModelChoice),
> `exomonad-core/src/ExoMonad/Graph/Interpret.hs:264-302` (executeClaudeCodeHandler)

### Graph-Level Annotations (`:&`)

Attach metadata to the entire graph using `:&` (right-associative):

```haskell
type MyGraphWithMeta = MyGraph
  :& Global SessionState           -- Shared state across all nodes
  :& Groups '[ '("llm", '["classify", "process"])
             , '("routing", '["route", "dispatch"])
             ]                     -- Mermaid subgraph organization
  :& Requires '[LLM, State SessionState, Log]  -- Effect documentation
  :& Backend 'NativeAnthropic      -- LLM provider selection
```

**Available graph annotations:**

| Annotation | Purpose |
|------------|---------|
| `Global StateType` | Shared state accessible from all nodes via Memory effect |
| `Groups '[("name", ["field1", ...])]` | Organize nodes in Mermaid diagram subgraphs |
| `Requires '[Effect1, Effect2]` | Document required effects (for runners) |
| `Backend NativeAnthropic \| CloudflareAI` | Select LLM provider |

**Global vs Node Memory:**
- `Global S` - All nodes share one `S` instance
- `Memory S` (on node) - Each node has its own `S` instance

> **Source**: `Graph/Types.hs`

### GraphEntries Type Family (MCP Tool Declaration)

The `GraphEntries` type family declares external entry points for a graph. This is how you expose graphs as MCP tools without Entry/Exit node ceremony.

```haskell
-- Data kind for entry point declarations
data GraphEntry = Symbol :~> (Symbol, Type, Symbol)
--                toolName    nodeField  inputType  description

-- Open type family - each graph defines its own instance
type family GraphEntries (graph :: Type -> Type) :: [GraphEntry]

-- Example: Single entry point
type instance GraphEntries FindCallersGraph =
  '[ "find_callers" ':~> '("fcRun", FindCallersArgs, "Find call sites") ]

-- Example: Multiple entry points into same graph
type instance GraphEntries TeachGraph =
  '[ "teach"        ':~> '("classify", Query, "Learn about a concept")
   , "teach_direct" ':~> '("explore", Intent, "Explore with pre-classified intent")
   ]
```

**Reification:**

```haskell
-- Extract tool definitions at runtime
tools <- reifyGraphEntries (Proxy @FindCallersGraph)
-- [MCPToolInfo { mtdName = "find_callers", mtdDescription = "...", ... }]
```

**Key types:**

| Type | Purpose |
|------|---------|
| `GraphEntry` | Kind for entry point declarations |
| `':~>` | Type-level operator mapping tool name to node info |
| `GraphEntries graph` | Type family returning list of entry points |
| `ReifyGraphEntries` | Typeclass for runtime reification |
| `MCPToolInfo` | Runtime tool metadata (name, description, schema) |

> **Source**: `Graph/Types.hs` (GraphEntry, GraphEntries), `Graph/MCPReify.hs` (reification)

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

## Interpret.hs - Typed Graph Dispatch

The `DispatchGoto` typeclass dispatches handlers based on `GotoChoice`:

```haskell
class DispatchGoto graph targets es exitType where
  dispatchGoto :: graph (AsHandler es) -> GotoChoice targets -> Eff es exitType
```

Pattern matches on `OneOf` to call the correct handler, recursing until Exit.

### Self-Loop Dispatch

When a node can transition back to itself via `Goto Self`, the standard `dispatchGoto` doesn't know which handler to re-invoke. Use `DispatchGotoWithSelf` instead:

```haskell
-- Define handler that may loop back to itself
loopHandler :: Int -> Eff es (GotoChoice '[To Self Int, To Exit Int])
loopHandler n
  | n >= 10   = pure $ gotoExit n
  | otherwise = pure $ gotoSelf (n + 1)

-- Run with explicit self-handler
initialChoice <- loopHandler 0
result <- dispatchGotoWithSelf loopHandler graph initialChoice
```

The typeclass signature:

```haskell
class DispatchGotoWithSelf graph selfPayload allTargets targets es exitType where
  dispatchGotoWithSelf
    :: (selfPayload -> Eff es (GotoChoice allTargets))  -- Self-handler
    -> graph (AsHandler es)                              -- Graph handlers
    -> GotoChoice targets                                -- Current choice
    -> Eff es exitType
```

**Why is this needed?** The graph record has fields like `compute`, `route`, etc., but there's no `self` field. When `Goto Self` is encountered, the dispatcher needs to know which handler to re-invoke. `dispatchGotoWithSelf` takes the self-handler as an explicit parameter.

**Error guidance**: If you try to use `dispatchGoto` with a handler that returns `Goto Self`, you'll get a clear `TypeError` directing you to use `dispatchGotoWithSelf`.

> **Source**: `exomonad-core/src/ExoMonad/Graph/Interpret.hs:599-677`

## Return Effect

The `Return` effect terminates graph execution with a value. It's the simplified alternative to `Goto Exit` for single-node graphs.

### Usage

```haskell
-- In handler constraint
findCallersLogic
  :: (Member LSP es, Member Log es, Member (Return FindCallersResult) es, LastMember IO es)
  => FindCallersArgs
  -> Eff es FindCallersResult
findCallersLogic args = do
  result <- computeResult args
  returnValue result  -- Terminates and returns the value
```

### API

```haskell
data Return (a :: Type) r where
  ReturnValue :: a -> Return a a

-- Terminate execution with a value
returnValue :: Member (Return a) effs => a -> Eff effs a

-- Run the Return effect, extracting the returned value
runReturn :: forall a effs. Eff (Return a ': effs) a -> Eff effs a
```

### When to Use

| Pattern | When |
|---------|------|
| `Return` effect | Single-node graphs, MCP tools, simple computations |
| `Goto Exit` | Multi-node graphs with routing, complex state machines |

**Key difference**: `Return` is an effect you use within a computation. `Goto Exit` is a transition target in a graph with Entry/Exit nodes.

> **Source**: `exomonad-core/src/ExoMonad/Effect/Types.hs`

## Memory Effect

Typed persistent state that survives across node invocations. There are two scopes:

### Node-Private Memory

Each node can have its own private state via the `Memory` annotation:

```haskell
-- Graph definition
gExplore :: mode :- G.LLMNode
    :@ Input URL
    :@ Template ExploreTpl
    :@ Schema Findings
    :@ Memory ExploreMem  -- Node-private state

-- State type
data ExploreMem = ExploreMem
  { visited :: [URL]
  , depth :: Int
  }

-- Usage in handler
exploreHandler = do
  mem <- getMem @ExploreMem
  updateMem @ExploreMem $ \m -> m { visited = url : m.visited, depth = m.depth + 1 }
```

### Graph-Global Memory

Shared state across all nodes via the `:&` graph-level annotation:

```haskell
-- Graph definition with global state
type MyGraphWithGlobal = MyGraph :& Global SessionState

-- Access in any handler
anyHandler = do
  session <- getMem @SessionState  -- Available to all nodes
  updateMem @SessionState $ \s -> s { turnCount = s.turnCount + 1 }
```

### Memory Effect API

```haskell
data Memory (s :: Type) :: Effect where
  GetMem    :: Memory s m s
  UpdateMem :: (s -> s) -> Memory s m ()

-- Smart constructors
getMem :: forall s es. Member (Memory s) es => Eff es s
updateMem :: forall s es. Member (Memory s) es => (s -> s) -> Eff es ()
modifyMem :: forall s es. Member (Memory s) es => (s -> (a, s)) -> Eff es a
```

**Persistence:** Memory state is serialized between graph steps. For WASM execution, it's included in the `StepOutput`'s `GraphState`. For native execution, it's held in the runner's state.

> **Source**: `exomonad-core/src/ExoMonad/Graph/Memory.hs`

## Subgraph Effect (Tree Recursion)

Spawn child instances of the same graph for recursive tree execution:

```haskell
import ExoMonad.Effect.Subgraph

-- In a handler that can spawn children
decomposeHandler :: Spec -> Eff (Subgraph Spec Result ': es) (GotoChoice targets)
decomposeHandler spec = do
  let childSpecs = partition spec  -- Break into smaller specs

  -- Spawn children (run in parallel)
  handles <- traverse spawnSelf childSpecs

  -- Wait for all children to complete
  results <- collectAll handles

  pure $ gotoExit (combine results)

collectAll :: [ChildHandle] -> Eff (Subgraph Spec Result ': es) [Result]
collectAll handles = go handles []
  where
    go [] acc = pure (reverse acc)
    go pending acc = do
      (childId, result) <- awaitAny  -- Blocks until any child completes
      go (filter (\h -> handleId h /= childId) pending) (result : acc)
```

**Operations:**
- `spawnSelf :: input -> Eff (Subgraph input output ': es) ChildHandle`
- `awaitAny :: Eff (Subgraph input output ': es) (ChildId, output)`
- `getPending :: Eff (Subgraph input output ': es) [ChildHandle]`

**Runtime:** Uses `async` for concurrent execution with `TQueue` for completion notifications.

**Use case:** V3 TDD protocol where Scaffold spawns child graphs for decomposed specs.

> **Source**: `Effect/Subgraph.hs` (types), `runtime/actor/src/ExoMonad/Actor/Subgraph.hs` (interpreter)

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

## Schema.hs - JSON Schema for Structured Output

LLM nodes use the `Schema` annotation to specify output types. The `HasJSONSchema` typeclass generates JSON Schema for these types:

```haskell
class HasJSONSchema a where
  jsonSchema :: JSONSchema
```

### Deriving JSON Schema

**Generic derivation (recommended):**

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

data Intent
  = RefundIntent { reason :: Text, amount :: Double }
  | FaqIntent { question :: Text }
  deriving (Generic, FromJSON, ToJSON, HasJSONSchema)
```

**Manual instance (for custom schemas):**

```haskell
instance HasJSONSchema CustomType where
  jsonSchema = JSONSchema
    { schemaType = Just "object"
    , schemaProperties = Just $ M.fromList
        [ ("field1", JSONSchema { schemaType = Just "string", ... })
        , ("field2", JSONSchema { schemaType = Just "integer", ... })
        ]
    , schemaRequired = Just ["field1"]
    , ...
    }
```

### How Schema Flows to the LLM

1. **At compile time**: `Schema Intent` annotation on LLM node links the type
2. **At handler definition**: `executeLLMHandler` calls `jsonSchema @schema`
3. **At runtime**: Schema is converted to Anthropic's format via `schemaToValue`
4. **In LLM call**: Schema passed to API as `tool_schema` or structured output constraint
5. **Response parsing**: LLM output parsed via `FromJSON` instance

### Schema Requirements

Types used with `Schema` must have:
- `HasJSONSchema` instance (for schema generation)
- `FromJSON` instance (for parsing LLM response)
- Ideally `ToJSON` instance (for debugging/logging)

```haskell
gClassify :: mode :- G.LLMNode
    :@ Input Message
    :@ Template ClassifyTpl
    :@ Schema Intent  -- Intent needs HasJSONSchema + FromJSON
```

> **Source**: `exomonad-core/src/ExoMonad/Schema.hs`

### Anthropic Structured Output Compatibility

Anthropic's structured outputs **do not support oneOf** schemas. The type system enforces this at compile time to prevent runtime failures:

```haskell
-- ❌ This will NOT compile
data Choice = OptionA Text | OptionB Int
  deriving (Generic, StructuredOutput)

-- Error: Schema error for structured output type: Choice
-- Anthropic's structured output does not support 'oneOf' schemas.
```

**Why?** Anthropic uses constrained decoding - the schema compiles to a grammar that restricts token generation. This makes oneOf fundamentally incompatible with their approach.

#### Nullary Sum Types (Enums) - Automatic!

For nullary sum types (enums with no data), ExoMonad **automatically generates efficient string enums**:

```haskell
-- ✅ Just derive - automatically optimized!
data Priority = Low | Medium | High
  deriving (Generic, StructuredOutput)

-- Generated schema: {"type": "string", "enum": ["Low", "Medium", "High"]}
-- Encoding: "Low", "Medium", "High" (plain strings, not {"tag": "Low", "contents": {}})
```

**Before this fix:** Nullary enums generated wasteful tag+contents schemas (3-4x larger).
**After this fix:** Automatic detection and string enum generation.

#### Sum Types with Data - NOT Allowed

Sum types with data in any variant cannot be used in structured output:

```haskell
-- ❌ Won't compile - has data
data Result = Success Text | Failure Error
  deriving (Generic, StructuredOutput)
```

**Fix options** (shown in error message):
1. **Use tagged record:**
   ```haskell
   data Result = Result { tag :: ResultTag, details :: Text }
   data ResultTag = Success | Failure
   ```

2. **Use separate Maybe fields:**
   ```haskell
   data Result = Result
     { success :: Maybe Text
     , failure :: Maybe Error
     }
   ```

3. **Use an enum if appropriate:**
   ```haskell
   data ResultTag = Success | Failure  -- No data
     deriving (Generic, StructuredOutput)
   ```

#### Maybe is Allowed (Special Case)

Even though `Maybe` is technically a sum type (`Nothing | Just a`), it has a well-defined JSON encoding (`null | value`) that Anthropic supports:

```haskell
-- ✅ Maybe is explicitly allowed
data Output = Output { priority :: Maybe Priority }
  deriving (Generic, StructuredOutput)
```

#### Error Message

When you try to use a sum type with data in structured output, you get:

```
═══════════════════════════════════════════════════════════════════
  Schema error for structured output type: Choice
═══════════════════════════════════════════════════════════════════

Anthropic's structured output does not support 'oneOf' schemas.
This type uses a sum type or union that generates oneOf.

Fix options:
  1. Use a tagged record: data MyChoice = MyChoice { tag :: Tag, ... }
  2. Use separate fields: data Output = Output { optionA :: Maybe A, optionB :: Maybe B }
  3. Use an enum if choices are simple strings
```

#### Benefits

- ✅ **Compile-time errors** instead of cryptic runtime failures
- ✅ **Efficient schemas** for enums: `{"enum": ["Low", "High"]}` instead of oneOf
- ✅ **Guaranteed Anthropic compatibility**
- ✅ **Self-documenting code**: Type errors explain constraints

> **References:**
> - [Anthropic Structured Outputs Docs](https://docs.anthropic.com/en/docs/build-with-claude/tool-use#structured-outputs)
> - [GitHub #4886: oneOf not supported](https://github.com/anthropics/claude-code/issues/4886)
> - **Source**: `exomonad-core/src/ExoMonad/Schema.hs`, `exomonad-core/src/ExoMonad/StructuredOutput/Generic.hs`

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

## ClaudeCode Handler (Subprocess Execution)

When an LLM node has the `ClaudeCode` annotation, the handler type changes to `ClaudeCodeLLMHandler`:

```haskell
gWork :: mode :- G.LLMNode
    :@ Input TaskInfo
    :@ Template WorkTpl
    :@ Schema WorkResult
    :@ ClaudeCode 'Sonnet ('Just "/path/to/worktree")

-- At handler definition, use ClaudeCodeLLMHandler with same 4 fields:
work = ClaudeCodeLLMHandler @'Sonnet
  { llmSystem = Nothing
  , llmUser   = templateCompiled @WorkTpl
  , llmBefore = \input -> do                      -- NOTE: Returns TUPLE!
      st <- getMem @SessionState
      pure (WorkContext { ... }, StartFresh "work-session")  -- (context, SessionOp)
  , llmAfter  = \(result, sessionId) -> do        -- NOTE: Receives TUPLE!
      -- result :: ClaudeCodeResult WorkResult
      -- sessionId :: SessionId
      -- Can reuse session for continuation via ContinueFrom sessionId
      updateMem @SessionState $ \s -> s { lastSessionId = Just sessionId }
      pure $ gotoExit result.ccrParsedOutput
  }
```

### Execution Pipeline: runGraph

The `runGraph` interpreter in `exomonad-core/src/ExoMonad/Graph/Interpret.hs` orchestrates the full ClaudeCode execution flow:

```haskell
-- Full type signature (simplified)
runGraph :: (ValidGraphRecord graph, DispatchGoto graph targets es exitType)
         => graph (AsHandler es)
         -> input
         -> Eff es exitType

-- For ClaudeCode nodes specifically:
executeClaudeCodeHandler
  :: ClaudeCodeLLMHandler model needs schema targets effs tpl
  -> needs
  -> Eff (Session ': effs) (GotoChoice targets)
```

**Execution steps** (for each ClaudeCode node):

1. **Before phase**: Call `llmBefore input`
   - Returns `(templateContext, SessionOperation)`
   - SessionOperation determines session strategy:
     - `StartFresh slug` → Create new Claude Code session
     - `ContinueFrom sessionId` → Reuse existing session (preserves context)
     - `ForkFrom parentId childSlug` → Create child session from parent

2. **Template rendering**: Render `llmSystem` and `llmUser` templates using `templateContext`

3. **Session dispatch**: Via Session effect interpreter
   - Calls `startSession` (fresh) or `continueSession` (existing)
   - Session runs `claude -p` via exomonad with rendered prompt
   - Claude Code executes in containerized environment with file access

4. **Structured output parsing**:
   - Claude Code outputs JSON matching schema
   - Parsed via `FromJSON` instance into `schema` type
   - If parsing fails, automatic retry (max 5 retries)
   - Errors include original LLM response for debugging

5. **After phase**: Call `llmAfter (result, sessionId)`
   - Result wrapped in `ClaudeCodeResult schema`
   - SessionId available for storage in Memory
   - Routes via `gotoChoice` based on parsed output

**Error recovery**: If LLM response violates schema, nag loop retries:
- Sends back error message + original schema
- Claude Code retries response (transparent to handler)
- Max 5 retries before surfacing error to handler

> **Source**: `exomonad-core/src/ExoMonad/Graph/Interpret.hs:264-442`

### Decision Tools: Structured Output via Tool Calls

ClaudeCode handlers use `ToDecisionTools` typeclass to generate MCP tools from output schemas:

```haskell
-- Handlers define output as sum types
data WorkExit
  = WorkComplete { result :: Text, effort :: Int }
  | WorkBlocked { blocker :: Text, suggestion :: Text }
  deriving (Generic, ToDecisionTools)

-- ToDecisionTools auto-generates MCP tools:
-- Tool 1: "work_complete" { "result": string, "effort": integer }
-- Tool 2: "work_blocked" { "blocker": string, "suggestion": string }

-- Claude Code picks one tool → tool_result → parseToolCall reconstructs WorkExit
instance ToDecisionTools WorkExit where
  toDecisionTools = ... -- Generic derivation
  parseToolCall toolName args = ... -- Reconstruct from tool name + args

-- Parsing happens automatically in after-phase
llmAfter (result :: ClaudeCodeResult WorkExit, sessionId) = do
  -- result.ccrParsedOutput :: WorkExit  -- Already reconstructed!
  case result.ccrParsedOutput of
    WorkComplete r e -> pure $ gotoExit (success r e)
    WorkBlocked b s  -> pure $ gotoExit (failure b s)
```

**How it works**:
1. **Schema generation**: Sum type variants → MCP tool definitions
   - Variant names converted to snake_case (auto prefix stripping): `WorkComplete` → `work_complete`
   - Fields become required tool parameters
   - Field names auto-converted to snake_case: `effort` → `effort`

2. **Tool invocation**: Claude Code calls one tool with parameters

3. **Reconstruction**: `parseToolCall` matches tool name and reconstructs value
   - Exact type safety via Haskell pattern matching
   - No `Dynamic`, no string-based dispatch

**Advantage over raw JSON**: Tools are self-documenting, Claude understands them as discrete choices rather than free-form JSON.

> **Source**: `exomonad-core/src/ExoMonad/StructuredOutput/DecisionTools.hs`

### Key Differences from LLMHandler

| `llmAfter` | `output -> Eff es (GotoChoice targets)` | Routes based on LLM output |
| `logic` | `input -> Eff es (GotoChoice targets)` | Pure routing logic |


### Before Function: SessionOperation

The `llmBefore` function must return a tuple `(context, SessionOperation)`:

```haskell
data SessionOperation
  = StartFresh slug           -- Create new conversation with slug identifier
  | ContinueFrom sessionId    -- Reuse existing session (for state continuation)
  | ForkFrom parentId childSlug  -- Create child session (for parallel work)
```

**Why it matters:**
- **StartFresh**: Each node gets its own conversation history by default
- **ContinueFrom**: Threads conversation state across multiple nodes (e.g., TDD ↔ Review cycles)
- **ForkFrom**: Spawn parallel sessions from shared parent (e.g., child decompositions)

**Example: Session Continuation in TDD Loop**

```haskell
-- TDDWriteTests before: start fresh conversation
tddWriteTestsBefore input = do
  pure (TDDWriteTestsContext { ... }, StartFresh "tdd-write-tests")

-- TDDWriteTests after: store SessionId in Memory
tddWriteTestsAfter (result, sid) = do
  updateMem @TDDMem $ \m -> m { conversationId = Just sid }  -- STORE IT
  pure $ gotoChoice @"v3Impl" ...

-- Impl before: continue conversation from stored SessionId
implBefore input = do
  mem <- getMem @TDDMem
  case mem.conversationId of
    Just sid -> pure (ImplContext { ... }, ContinueFrom sid)    -- REUSE IT
    Nothing  -> pure (ImplContext { ... }, StartFresh "impl")
```

### After Function: ClaudeCodeResult

The `llmAfter` function receives `(ClaudeCodeResult schema, SessionId)`:

```haskell
data ClaudeCodeResult schema = ClaudeCodeResult
  { ccrParsedOutput :: schema       -- The parsed structured output
  , ccrSessionId :: SessionId       -- Session ID (same as tuple second element)
  }
```

**Why the duplicate?** `ccrSessionId` is available both ways:
- As second element of tuple: `(\(result, sessionId) -> ...)`
- Inside result: `(result.ccrSessionId)`

Use whichever is clearer in context.

### Type Parameters: Model Selection

The `ClaudeCodeLLMHandler` type has 6 parameters, including `model`:

```haskell
ClaudeCodeLLMHandler
  :: forall model tpl needs schema targets effs.
     SingModelChoice model  -- Model constraint
  => Maybe (TypedTemplate tpl SourcePos)    -- system prompt
  -> TypedTemplate tpl SourcePos             -- user prompt
  -> (needs -> Eff effs (tpl, SessionOperation))  -- before
  -> ((ClaudeCodeResult schema, SessionId) -> Eff effs (GotoChoice targets))  -- after
  -> ClaudeCodeLLMHandler model needs schema targets effs tpl
```

**Model selection** happens at annotation time:

```haskell
:@ ClaudeCode 'Haiku     -- Small, fast model
:@ ClaudeCode 'Sonnet    -- Balanced (recommended)
:@ ClaudeCode 'Opus      -- Large, powerful (research-grade)
```

GHC enforces the selected model as a type parameter throughout the handler's lifetime.

### Real-World Example: types-first-dev V3 Protocol

The types-first-dev project uses `ClaudeCodeLLMHandler` for TDD workflow nodes:

```haskell
-- Scaffold: DecomposeSpec → Criteria
scaffoldHandler = ClaudeCodeLLMHandler @'Sonnet
  Nothing
  (templateCompiled @ScaffoldTpl)
  scaffoldBefore    -- TaskInfo → (ScaffoldContext, SessionOp)
  scaffoldAfter     -- (ClaudeCodeResult ScaffoldExit, SessionId) → GotoChoice

-- Impl: Make tests pass (with retry loop)
implHandler = ClaudeCodeLLMHandler @'Sonnet
  Nothing
  (templateCompiled @ImplTpl)
  implBefore        -- ImplInput → (ImplContext, ContinueFrom sessionId)
  implAfter         -- Retry logic + route to TDDReviewImpl

-- TDDReviewImpl: Review implementation (decision tools)
reviewHandler = ClaudeCodeLLMHandler @'Sonnet
  (Just systemTpl)
  (templateCompiled @TDDReviewImplTpl)
  tddReviewBefore   -- Build context
  tddReviewAfter    -- Route to Merger or MoreTests
```

Key design:
- All use `'Sonnet` model (fast enough for iteration)
- Before functions return `(context, SessionOperation)` tuples
- After functions handle session IDs for conversation continuation
- Session reuse enables TDD ↔ Review ↔ Impl ping-pongs

> **Source**: `exomonad-core/src/ExoMonad/Graph/Goto.hs:496-557` (types),
> `exomonad-core/src/ExoMonad/Graph/Interpret.hs:264-442` (execution)

## Validation

The `ValidGraphRecord` constraint bundles all compile-time validation checks. When validation fails, you get clear error messages explaining what's wrong and how to fix it.

### Complete Validation Inventory

| Check | Purpose | Error Indicates |
|-------|---------|-----------------|
| `RequireGeneric` | Graph type must derive `Generic` | Add `deriving Generic` to your graph type |
| `ValidateEntryExit` | Exactly one `Entry` and one `Exit` node | Add missing Entry/Exit or remove duplicates |
| `ValidateGotoTargets` | All `Goto "name"` targets must exist as fields | Typo in target name, or missing node |
| `ValidateNoToInEffects` | `To` markers belong in handlers, not `UsesEffects` | Use `Goto` in graph definition, `To` in handler return types |
| `AllFieldsReachable` | All nodes must be reachable from Entry | Orphaned nodes with no incoming edges |
| `AllLogicFieldsReachExit` | Logic nodes must have path to Exit | Logic node that can never complete |
| `NoDeadGotosRecord` | Goto targets must lead somewhere | Dead-end transitions |
| `AllLogicNodesHaveGoto` | Logic nodes must have at least one Goto | Logic node with no transitions |
| `NoGotoSelfOnly` | Self-loops must have exit path | Infinite loop with no escape |

### Example Error Messages

**Missing node target:**
```
═══════════════════════════════════════════════════════════════════
  Goto target "gProces" doesn't exist in graph
═══════════════════════════════════════════════════════════════════

WHAT HAPPENED
  Node 'gRoute' has: Goto "gProces" SomeType
  But there's no field named "gProces" in your graph.

HOW TO FIX
  • Check spelling: did you mean "gProcess"?
  • Add the missing node to your graph type
  • Remove the Goto if the target was deleted
```

**Self-loop only (no exit):**
```
═══════════════════════════════════════════════════════════════════
  Node "gLoop" can only Goto Self - infinite loop!
═══════════════════════════════════════════════════════════════════

WHAT HAPPENED
  UsesEffects contains only: Goto Self Payload
  This node has no way to exit the loop.

HOW TO FIX
  Add an exit path:
    UsesEffects '[Goto Self Payload, Goto Exit Result]
    UsesEffects '[Goto Self Payload, Goto "nextNode" Data]
```

> **Source**: `exomonad-core/src/ExoMonad/Graph/Generic.hs:1152-1165` (ValidGraphRecord),
> `exomonad-core/src/ExoMonad/Graph/Validate/RecordStructure.hs` (reachability checks)

## Type-Level Error Messages

The Graph DSL uses GHC's custom `TypeError` mechanism to provide clear, actionable error messages. These are not just error text - they're **documentation embedded in the type system**.

### Error Message Structure

All error messages follow a consistent format:

```
═══════════════════════════════════════════════════════════════════
  [Title: What went wrong]
═══════════════════════════════════════════════════════════════════

WHAT HAPPENED
  [Explanation of what the type checker found]

HOW IT WORKS
  [Background on why this constraint exists]

HOW TO FIX
  • [Actionable fix option 1]
  • [Actionable fix option 2]

  [Code examples if applicable]
```

### Error Categories

**Graph Structure Errors** (`Generic.hs`):
- Missing or duplicate Entry/Exit nodes
- Invalid Goto targets (typos, missing nodes)
- Using `To` instead of `Goto` in graph definitions

**Dispatch Errors** (`Interpret.hs`):
- Empty target list (handler has no exit points)
- Self-loop with `dispatchGoto` (should use `dispatchGotoWithSelf`)

**Goto Constraint Errors** (`Goto.hs`):
- Empty `GotoChoice` (invalid construction)
- Target not in list (type mismatch)

### Example: Self-Loop Guidance

When you try to use `dispatchGoto` with a self-looping handler:

```
═══════════════════════════════════════════════════════════════════
  Self-loop requires special dispatch
═══════════════════════════════════════════════════════════════════

WHAT HAPPENED
  Your handler can 'gotoSelf', but you called 'dispatchGoto'.
  The standard dispatcher doesn't know which handler to re-invoke.

HOW IT WORKS
  Normal dispatch:  GotoChoice -> find handler by name -> call it
  Self dispatch:    GotoChoice -> ??? -> call... which handler?

  The graph record has fields like 'compute', 'route', etc.
  But there's no 'self' field! We need you to tell us what
  'self' means for this particular dispatch.

HOW TO FIX
  • Use dispatchGotoWithSelf and pass the self-handler:

  -- Instead of:
  choice <- loopHandler input
  result <- dispatchGoto handlers choice        -- ERROR

  -- Use:
  choice <- loopHandler input
  result <- dispatchGotoWithSelf loopHandler handlers choice  -- OK
```

> **Source**: Error formatting helpers in `exomonad-core/src/ExoMonad/Graph/Errors.hs`,
> `TypeError` instances throughout `Generic.hs`, `Interpret.hs`, `Goto.hs`

## Effect vs Effects

- **`ExoMonad.Effect.*`** (singular) - Core effect infrastructure
- **`ExoMonad.Effects.*`** (plural) - Integration/contrib effects (Habitica, Telegram, etc.)

## WASM Execution

The `exomonad-wasm` package enables yield/resume across FFI boundaries:

```haskell
type WasmM a = Eff '[Yield SerializableEffect EffectResult] a

-- Effects yield to TypeScript, resume with result
computeHandler :: Int -> WasmM (GotoChoice '[To Exit Int])
computeHandler n = do
  logInfo $ "Computing: " <> T.pack (show n)  -- Yields to TS
  pure $ gotoExit (n + 1)
```

Uses `freer-simple` for reified continuations (required for WASM).

## Common Patterns

### Parallel Execution (Fork/Join)

The graph DSL doesn't express parallel fan-out directly. Use `LogicNode` with `concurrently`:

```haskell
-- Graph definition: linear transitions (parallelism not visible)
, fork  :: mode :- G.LogicNode :@ Input ForkInput :@ UsesEffects '[Goto "merge" Results]
, merge :: mode :- G.LogicNode :@ Input Results :@ UsesEffects '[Goto Exit Output]

-- Handler: parallel execution hidden inside
forkHandler input = do
  (result1, result2) <- sendM $ concurrently
    (runAgent1 input)
    (runAgent2 input)
  pure $ gotoChoice @"merge" Results { r1 = result1, r2 = result2 }
```

**Why this works:**
- Graph shows workflow steps (fork, merge) as distinct phases
- Parallelism is an implementation detail, not a structural concern
- Results are collected before transitioning to merge

### Retry Logic: Local Recursion vs Self-Loop

Choose based on visibility needs:

**Local recursion** (retry is implementation detail):
```haskell
runWithRetry :: Int -> IO Result
runWithRetry attempt
  | attempt >= maxRetries = error "Max retries"
  | otherwise = do
      result <- tryOperation
      case result of
        Success r -> pure r
        Failure _ -> runWithRetry (attempt + 1)
```

**Graph self-loop** (retry is workflow step, visible in tracing):
```haskell
-- Graph definition
, validate :: mode :- G.LogicNode
    :@ Input Attempt
    :@ UsesEffects '[Goto Self Attempt, Goto "next" ValidResult]

-- Handler
validateHandler attempt
  | attempt.count >= maxRetries = pure $ gotoChoice @"next" (errorResult attempt)
  | otherwise = case validate attempt.data of
      Valid r   -> pure $ gotoChoice @"next" r
      Invalid e -> pure $ gotoSelf attempt { count = attempt.count + 1, lastError = e }
```

Use self-loops when retry attempts should be observable (telemetry, debugging). Use local recursion when retry is purely an implementation detail.

## File Inventory

All paths relative to `exomonad-core/src/ExoMonad/Graph/`.

| File | Key Exports | Purpose |
|------|-------------|---------|
| `Types.hs` | `(:@)`, `Input`, `Schema`, `Goto`, `Exit`, `Self`, `GraphEntry`, `(:~>)`, `GraphEntries` | Core DSL syntax and annotations |
| `Generic.hs` | `GraphMode`, `AsHandler`, `AsGraph`, `NodeHandler`, `ValidGraphRecord` | Mode system and handler type computation |
| `Edges.hs` | `GetInput`, `GetSchema`, `GetUsesEffects`, `GotoEffectsToTargets`, `GotosToTos` | Type families for annotation extraction |
| `Goto.hs` | `Goto`, `To`, `OneOf`, `GotoChoice`, `gotoChoice`, `gotoExit`, `gotoSelf`, `LLMHandler` | Transition types and smart constructors |
| `Interpret.hs` | `DispatchGoto`, `DispatchGotoWithSelf`, `runGraph`, `runGraphFrom`, `CallHandler` | Typed graph dispatch |
| `Interpret/Instrumented.hs` | Traced `DispatchGoto` instances | OpenTelemetry span emission |
| `Memory.hs` | `Memory`, `getMem`, `updateMem`, `modifyMem` | Node-private persistent state |
| `MCPReify.hs` | `MCPToolInfo`, `ReifyMCPTools`, `ReifyGraphEntries`, `reifyGraphEntries` | MCP tool reification from graph types |
| `Template.hs` | `TemplateDef`, `TemplateContext`, `templateCompiled`, `buildContext` | Typed Jinja templates |
| `Tool.hs` | `ToolDef`, `toolName`, `toolSchema`, `toolExecute` | LLM-invocable tools |
| `Validate/Validate.hs` | Error message templates (`UnsatisfiedNeedError`, etc.) | Validation error formatting |
| `Validate/RecordStructure.hs` | `AllFieldsReachable`, `AllLogicFieldsReachExit`, `NoDeadGotosRecord` | Reachability validation |
| `Reify.hs` | `GraphInfo`, `NodeInfo`, `reifyGraph` | Runtime graph introspection |
| `Mermaid.hs` | `graphToMermaid` | Diagram generation |
| `Errors.hs` | `HR`, `WhatHappened`, `HowItWorks`, `Fixes`, `CodeLine`, `Bullet` | Error formatting primitives |

### Related Modules (outside Graph/)

| Path | Key Exports | Purpose |
|------|-------------|---------|
| `Effect/Types.hs` | `State`, `LLM`, `Log`, `Emit`, `RequestInput`, `Time`, `Random`, `Return`, `returnValue`, `runReturn` | Core effect definitions |
| `Effect/Session.hs` | `Session`, `SessionOutput`, `startSession`, `continueSession` | Dockerized Claude Code sessions via exomonad |
| `Schema.hs` | `HasJSONSchema`, `JSONSchema`, `schemaToValue` | JSON Schema for structured output |
| `Effects/*.hs` | `GitHub`, `Habitica`, `Telegram`, `Git`, etc. | Integration effects |

## Related Documentation

- [exomonad-wasm/CLAUDE.md](../exomonad-wasm/CLAUDE.md) - WASM compilation, FFI, wire types
- [exomonad-generated-ts/CLAUDE.md](../exomonad-generated-ts/CLAUDE.md) - Generated TypeScript types
- [deploy/CLAUDE.md](../deploy/CLAUDE.md) - Cloudflare Worker harness and effect handlers
- [Root CLAUDE.md](../CLAUDE.md) - Project overview and consuming repo patterns
