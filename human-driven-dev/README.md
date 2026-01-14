# human-driven-dev

Template repository for building type-driven development workflows with Tidepool.

## Structure

```
human-driven-dev/
├── src/
│   ├── HumanDrivenDev/
│   │   ├── Types.hs              # Re-export all types
│   │   ├── Types/
│   │   │   ├── Core.hs           # Core entry/exit types
│   │   │   └── MyTask.hs         # Task-specific types
│   │   ├── MyTaskGraph.hs        # Graph definition (nodes + edges)
│   │   ├── MyTaskContext.hs      # Template context builders
│   │   ├── MyTaskHandlers.hs     # Before/after handlers
│   │   ├── MyTaskTemplates.hs    # Jinja template declarations
│   │   └── MyTaskInterpreter.hs  # Effect interpreters
│   └── templates/
│       └── my_task.jinja         # Prompt templates
├── app/
│   └── Main.hs                   # Executable runner
└── human-driven-dev.cabal        # Package definition
```

## Usage Pattern

1. **Define types** in `Types/Core.hs` and `Types/MyTask.hs`
   - Input/Output for each node
   - Exit types with data constructors for different outcomes

2. **Define graph** in `MyTaskGraph.hs`
   - Entry node
   - LLM nodes with type annotations
   - Exit node
   - Type annotations encode data flow

3. **Write templates** in `templates/`
   - Jinja templates with typed context
   - Declare in `MyTaskTemplates.hs` with TemplateHaskell

4. **Implement handlers** in `MyTaskHandlers.hs`
   - Before-handlers: build context, choose session strategy
   - After-handlers: route based on exit type, manage memory

5. **Wire effects** in `MyTaskInterpreter.hs`
   - Compose effect interpreters
   - Connect handlers to graph execution

## Key Concepts

### Type-Driven Data Flow
Annotations like `:@ Input X` and `:@ Schema Y` encode data flow:
- Input annotation: what data flows IN to a node
- Schema annotation: what structured data comes OUT

### Exit Type Routing
Handlers pattern match on exit types to decide transitions:
```haskell
case exit of
  SuccessCase data -> gotoChoice @"nextNode" input
  RetryCase reason -> gotoSelf retryInput
  FailureCase err -> gotoChoice @"exitNode" result
```

### Session Continuation
Use Memory effect to thread SessionIds across retries:
```haskell
mem <- getMem @MyMem
let sessionOp = case mem.sessionId of
      Just sid -> ContinueFrom sid    -- Reuse session
      Nothing -> StartFresh "slug"    -- New session
```

## Customization

Replace "MyTask" with your actual task name throughout:
- Module names: `HumanDrivenDev.MyTask*` → `HumanDrivenDev.YourTask*`
- Graph types: `MyTaskGraph` → `YourTaskGraph`
- Type names: `MyTaskInput` → `YourTaskInput`

Keep the structure, change the semantics.
