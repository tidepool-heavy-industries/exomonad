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

> **Source**: `Graph/Types.hs` (annotations), `Graph/Validate/ForkBarrier.hs` (validation), `runtime/actor/src/Tidepool/Actor/Fork.hs` (execution)

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
    :@ Input BeadInfo
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
3. Prompt is passed to `claude -p` via the mantle executor
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

> **Source**: `tidepool-core/src/Tidepool/Graph/Types.hs` (ClaudeCode, ModelChoice),
> `tidepool-core/src/Tidepool/Graph/Execute.hs:264-302` (executeClaudeCodeHandler)

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

> **Source**: `tidepool-core/src/Tidepool/Graph/Execute.hs:599-677`

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

> **Source**: `tidepool-core/src/Tidepool/Graph/Memory.hs`

## Subgraph Effect (Tree Recursion)

Spawn child instances of the same graph for recursive tree execution:

```haskell
import Tidepool.Effect.Subgraph

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

> **Source**: `Effect/Subgraph.hs` (types), `runtime/actor/src/Tidepool/Actor/Subgraph.hs` (interpreter)

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

> **Source**: `tidepool-core/src/Tidepool/Schema.hs`

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

For nullary sum types (enums with no data), Tidepool **automatically generates efficient string enums**:

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
> - **Source**: `tidepool-core/src/Tidepool/Schema.hs`, `tidepool-core/src/Tidepool/StructuredOutput/Generic.hs`

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

> **Source**: `tidepool-core/src/Tidepool/Graph/Generic.hs:1152-1165` (ValidGraphRecord),
> `tidepool-core/src/Tidepool/Graph/Validate/RecordStructure.hs` (reachability checks)

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

**Dispatch Errors** (`Execute.hs`):
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

> **Source**: Error formatting helpers in `tidepool-core/src/Tidepool/Graph/Errors.hs`,
> `TypeError` instances throughout `Generic.hs`, `Execute.hs`, `Goto.hs`

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

All paths relative to `tidepool-core/src/Tidepool/Graph/`.

| File | Key Exports | Purpose |
|------|-------------|---------|
| `Types.hs` | `(:@)`, `Input`, `Schema`, `Goto`, `Exit`, `Self`, `ClaudeCode`, `ModelChoice` | Core DSL syntax and annotations |
| `Generic.hs` | `GraphMode`, `AsHandler`, `AsGraph`, `NodeHandler`, `ValidGraphRecord` | Mode system and handler type computation |
| `Edges.hs` | `GetInput`, `GetSchema`, `GetUsesEffects`, `GotoEffectsToTargets`, `GotosToTos` | Type families for annotation extraction |
| `Goto.hs` | `Goto`, `To`, `OneOf`, `GotoChoice`, `gotoChoice`, `gotoExit`, `gotoSelf`, `LLMHandler` | Transition types and smart constructors |
| `Execute.hs` | `DispatchGoto`, `DispatchGotoWithSelf`, `runGraph`, `runGraphFrom`, `CallHandler` | Typed graph dispatch |
| `Execute/Instrumented.hs` | Traced `DispatchGoto` instances | OpenTelemetry span emission |
| `Memory.hs` | `Memory`, `getMem`, `updateMem`, `modifyMem` | Node-private persistent state |
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
| `Effect/Types.hs` | `State`, `LLM`, `Log`, `Emit`, `RequestInput`, `Time`, `Random` | Core effect definitions |
| `Effect/Session.hs` | `Session`, `SessionOutput`, `startSession`, `continueSession` | Dockerized Claude Code sessions via mantle |
| `Schema.hs` | `HasJSONSchema`, `JSONSchema`, `schemaToValue` | JSON Schema for structured output |
| `Effects/*.hs` | `BD`, `GitHub`, `Habitica`, `Telegram`, `Git`, etc. | Integration effects |

## Related Documentation

- [tidepool-wasm/CLAUDE.md](../tidepool-wasm/CLAUDE.md) - WASM compilation, FFI, wire types
- [tidepool-generated-ts/CLAUDE.md](../tidepool-generated-ts/CLAUDE.md) - Generated TypeScript types
- [deploy/CLAUDE.md](../deploy/CLAUDE.md) - Cloudflare Worker harness and effect handlers
- [Root CLAUDE.md](../CLAUDE.md) - Project overview and consuming repo patterns
