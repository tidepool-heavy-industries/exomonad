# Tidepool Agent Template

This is a minimal agent skeleton using the tidepool Graph DSL.

## What is Tidepool?

Tidepool is a type-safe LLM agent framework. The key insight: LLMs don't need raw IO - they need:
- **Typed state** they can read (via Jinja templates)
- **Typed mutations** they can express (via structured output schemas)

An agent is a **state machine** where:
- **Nodes** are either LLM calls or pure logic
- **Edges** are typed data flow (implicit) or explicit transitions (Goto)
- **Templates** render state into LLM prompts
- **Schemas** define structured output the LLM must produce

## How It All Connects

```
┌─────────────────────────────────────────────────────────────┐
│                      Execution Flow                         │
│                                                             │
│  1. Entry receives Input                                    │
│  2. sgProcess (LLM node):                                   │
│     - Handler builds template context from Input + History  │
│     - Template renders context → prompt string              │
│     - LLM called with prompt + Schema                       │
│     - LLM returns structured Output                         │
│  3. sgRoute (Logic node):                                   │
│     - Handler receives Output                               │
│     - Returns Goto decision (exit with Result)              │
│  4. Exit returns Result                                     │
└─────────────────────────────────────────────────────────────┘
```

## Architecture

See `docs/graph.md` for the generated Mermaid diagrams.

The graph follows the pattern:
```
Entry → LLM Node → Logic Node → Exit
```

- **Entry**: Receives input to the agent (typed as `Input`)
- **LLM Node**: Calls an LLM with structured output (produces `Output` via Schema)
- **Logic Node**: Routes based on LLM output (uses Goto to exit with `Result`)
- **Exit**: Returns the final result (typed as `Result`)

## Key Files

| File | Purpose |
|------|---------|
| `src/Template/Context.hs` | Context type with `ToGVal` (compiled first for TH) |
| `src/Template/Templates.hs` | Template compilation via TH + `TemplateDef` |
| `src/Template/Graph.hs` | Type-level graph definition |
| `src/Template/Handlers.hs` | Handler implementations |
| `templates/process.jinja` | Main Jinja template (wired to graph) |
| `templates/_shared/` | Shared template partials |
| `app/GenerateDocs.hs` | Generates Mermaid from types + templates |
| `docs/graph.md` | Generated diagrams (committed) |

## Graph DSL Quick Reference

### Syntax

```haskell
data MyGraph mode = MyGraph
  { fieldName :: mode :- NodeType :@ Annotation1 :@ Annotation2
  }
```

- `mode` - Graph mode (AsGraph for validation, AsHandler for execution)
- `:-` - Separates mode from node definition
- `:@` - Applies an annotation to a node (right-associative, chains)

### Node Types

```haskell
-- Entry point - where data enters the graph
mode :- G.Entry InputType

-- LLM node - calls language model, produces structured output
mode :- G.LLMNode :@ Needs '[InputType] :@ Schema OutputType

-- Logic node - pure routing based on inputs
mode :- G.LogicNode :@ Needs '[InputType] :@ UsesEffects '[Goto "target" PayloadType]

-- Exit point - where data leaves the graph
mode :- G.Exit OutputType
```

### Annotations

| Annotation | Applies To | Purpose |
|------------|-----------|---------|
| `Needs '[T1, T2]` | LLM, Logic | Input types this node consumes |
| `Schema T` | LLM | Structured output type (JSON schema derived) |
| `Template T` | LLM | Jinja template for prompt (requires TemplateDef) |
| `UsesEffects '[E1, E2]` | LLM, Logic | Effects/transitions (Goto targets) |
| `Tools '[T1, T2]` | LLM | Tools available to the LLM |
| `Memory T` | LLM, Logic | Node-private persistent state |
| `Vision` | LLM | Enable vision/image input |

### Data Flow

- **Schema → Needs**: Implicit edges. If node A has `Schema T` and node B has `Needs '[T]`, there's an edge A → B
- **Goto**: Explicit transitions. `UsesEffects '[Goto "nodeName" Payload]` creates edge to that node

### Full Documentation

See `tidepool-core/src/Tidepool/Graph/CLAUDE.md` for comprehensive DSL documentation.

## Modifying the Graph

1. Edit `src/Template/Graph.hs` to change the graph structure
2. Update `src/Template/Handlers.hs` with matching handlers
3. Run `just generate-docs` to regenerate the diagram
4. Commit the updated `docs/graph.md`

## Implementing Handlers

### LLM Node Handlers

LLM handlers use the `LLMHandler` constructor with four arguments:

```haskell
sgProcess = LLMHandler
  Nothing                    -- optional system template
  (templateCompiled @MyTpl)  -- user template
  (\input -> do              -- before: build context
    pure ContextType { ... })
  (\output -> do             -- after: route based on output
    pure $ gotoExit result)
```

### Logic Node Handlers

Logic handlers return `GotoChoice`:

```haskell
sgRoute = \output -> do
  pure $ gotoExit (Result output.outputText)
```

## Jinja Templates

Templates live in `templates/` and use Jinja syntax. The dependency tree is automatically
generated in `docs/graph.md`.

### How Templates Connect to the Graph

The `sgProcess` LLM node has a `Template ProcessTpl` annotation. This wires:

1. **`Template.Context`** - Defines `ProcessContext` with `ToGVal` instance
2. **`Template.Templates`** - Compiles `templates/process.jinja` at build time via TH
3. **`Template.Graph`** - The node has `:@ Template ProcessTpl` annotation
4. **`Template.Handlers`** - Uses `LLMHandler` to build `ProcessContext` and route output

At runtime: Handler builds context → Template renders to prompt → LLM called → Schema parsed.

### Template Structure

```
templates/
├── process.jinja           # Main template (wired to sgProcess node)
└── _shared/
    └── output_format.jinja # Shared partials
```

### Adding Templates

1. Create `.jinja` files in `templates/`
2. Use `{% include "_shared/partial.jinja" %}` for includes
3. Use `{% extends "base.jinja" %}` for inheritance
4. Run `just generate-docs` to update the dependency diagram

### Template Variables

Templates receive context from LLM handlers. Variables like `{{ input }}` are
populated by the handler's `buildContext` function. The context type must have
a `ToGVal` instance for template rendering.

## Chat History

The template includes chat history integration via the `ChatHistory` effect.

### How It Works

1. **Effect**: Handlers declare `ChatHistory` in their effect list via `AsHandler '[ChatHistory]`
2. **Access**: Use `getHistory` to retrieve conversation history as `[Message]`
3. **Formatting**: `formatHistory` converts raw messages to simplified `[HistoryMessage]`
4. **Template**: The `{{ history }}` variable is available in templates

### Template Usage

```jinja
{% if history %}
## Conversation History

{% for msg in history %}
**{{ msg.role }}**: {{ msg.content }}

{% endfor %}
---

{% endif %}
```

### HistoryMessage Type

The `HistoryMessage` type provides a simplified view of messages:

```haskell
data HistoryMessage = HistoryMessage
  { role :: Text    -- "user" or "assistant"
  , content :: Text -- Text content only (images/tools stripped)
  }
```

### Runner Requirements

The runner must interpret the `ChatHistory` effect. Options:
- `runChatHistory` - Simple in-memory (tidepool-core)
- `runChatHistoryWithDB` - SQLite-backed (tidepool-platform)
- `runChatHistoryWithCompression` - Auto-compresses long histories (tidepool-platform)

## Unfinished Features (Stubs)

The following features are planned but not yet stable:

- **Self-Loops**: Nodes that can loop back to themselves
- **Tools**: LLM tool use within nodes

Check tidepool-core updates for when these features are ready.

## Commands

```bash
just build          # Build the agent
just generate-docs  # Regenerate Mermaid diagram
just all            # Build + generate docs
```

## Running the Agent

This skeleton does not include a runner executable. To run an agent, you need:

1. **An interpreter for the LLM effect** - handles API calls to Anthropic/OpenAI
2. **A graph executor** - dispatches to handlers based on graph structure
3. **An entry point** - calls the executor with initial input

See `tidepool-platform` for the Anthropic LLM interpreter and `~/dev/anemone`
for complete runnable agents.

## What This Skeleton Provides

| Feature | Status |
|---------|--------|
| Type-safe graph definition | Complete |
| Mermaid diagram generation | Complete |
| Template dependency tree | Complete |
| Chat history integration | Complete |
| Stub handlers | Complete |
| LLM API integration | Requires tidepool-platform |
| Graph executor | Requires tidepool-core runner |
| Runnable main | Not included |
