# Graph DSL - Generated Diagrams

This file contains Mermaid diagrams generated from the example graphs in `Example.hs`.

## SimpleGraph

A linear graph: Entry → classify → respond → Exit

```haskell
type SimpleGraph = Graph
  '[ Entry :~> Message
   , "classify" := LLM :@ Needs '[Message] :@ Schema Intent
   , "respond"  := LLM :@ Needs '[Message, Intent] :@ Schema Response
   , Exit :<~ Response
   ]
```

### Flowchart

```mermaid
flowchart TD

    %% Entry and Exit
    entry((start))
    exit__((end))

    %% Nodes
    classify[["classify<br/>LLM"]]
    respond[["respond<br/>LLM"]]

    %% Edges
    entry --> |Message| classify
    entry --> |Message| respond
    classify --> |Intent| respond
    respond --> |Response| exit__
```

### State Diagram

```mermaid
stateDiagram-v2

    %% State definitions
    classify : LLM
    respond : LLM

    %% Transitions
    [*] --> classify: Message
    [*] --> respond: Message
    classify --> respond: Intent
    respond --> [*]: Response
```

### Sequence Diagram

```mermaid
sequenceDiagram

    %% Participants
    participant Entry
    participant classify
    participant respond
    participant Exit

    %% Message flow
    Entry->>classify: Message
    classify->>respond: Intent
    respond->>Exit: Response
```

---

## BranchingGraph

A graph with Logic node routing to different targets via Goto.

```haskell
type BranchingGraph = Graph
  '[ Entry :~> Message
   , "route"  := Logic
       :@ Needs '[Message]
       :@ Eff '[Goto "refund" Message, Goto "answer" Message, Goto Exit Response]
   , "refund" := LLM :@ Needs '[Message] :@ Schema Response
   , "answer" := LLM :@ Needs '[Message] :@ Schema Response
   , Exit :<~ Response
   ]
```

### Flowchart

```mermaid
flowchart TD

    %% Entry and Exit
    entry((start))
    exit__((end))

    %% Nodes
    route{{"route<br/>Logic"}}
    refund[["refund<br/>LLM"]]
    answer[["answer<br/>LLM"]]

    %% Edges
    entry --> |Message| route
    entry --> |Message| refund
    entry --> |Message| answer
    route --> |Message| refund
    route --> |Message| answer
    route --> |Response| exit__
    refund --> |Response| exit__
    answer --> |Response| exit__
```

### State Diagram

```mermaid
stateDiagram-v2

    %% State definitions
    route : Logic
    refund : LLM
    answer : LLM

    %% Transitions
    [*] --> route: Message
    [*] --> refund: Message
    [*] --> answer: Message
    route --> refund: Message
    route --> answer: Message
    route --> [*]: Response
    refund --> [*]: Response
    answer --> [*]: Response
```

### Sequence Diagram (refund path)

```mermaid
sequenceDiagram

    %% Participants
    participant Entry
    participant route
    participant refund
    participant Exit

    %% Message flow
    Entry->>route: Message
    route->>refund: Message
    refund->>Exit: Response
```

### Sequence Diagram (answer path)

```mermaid
sequenceDiagram

    %% Participants
    participant Entry
    participant route
    participant answer
    participant Exit

    %% Message flow
    Entry->>route: Message
    route->>answer: Message
    answer->>Exit: Response
```

---

## AnnotatedGraph

A graph with various annotations: Template, Vision, Tools, When.

```haskell
type AnnotatedGraph = Graph
  '[ Entry :~> Message
   , "analyze" := LLM
       :@ Needs '[Message]
       :@ Schema Intent
       :@ Template MyTemplate
       :@ Vision
       :@ Tools '[PhotoTool]
   , "conditional" := LLM
       :@ Needs '[Intent]
       :@ Schema Response
       :@ When Intent  -- Only runs when Intent is present
   , Exit :<~ Response
   ]
```

### Flowchart

```mermaid
flowchart TD

    %% Entry and Exit
    entry((start))
    exit__((end))

    %% Nodes
    analyze[["analyze<br/>LLM"]]
    conditional[["conditional<br/>LLM"]]

    %% Edges
    entry --> |Message| analyze
    analyze --> |Intent| conditional
    conditional --> |Response| exit__
```

### State Diagram

```mermaid
stateDiagram-v2

    %% State definitions
    analyze : LLM
    conditional : LLM

    %% Transitions
    [*] --> analyze: Message
    analyze --> conditional: Intent
    conditional --> [*]: Response
```

---

## Diagram Type Comparison

| Diagram Type | Best For | Limitations |
|--------------|----------|-------------|
| **Flowchart** | Visual overview, node shapes show LLM vs Logic | Shows all Schema→Needs edges (can be noisy) |
| **State Diagram** | Accurate state machine representation | Less visual distinction between node types |
| **Sequence Diagram** | Showing specific execution paths | Requires choosing a path through branches |

## Node Shapes (Flowchart)

- `((circle))` - Entry/Exit points
- `[["double brackets"]]` - LLM nodes
- `{{"hexagon"}}` - Logic nodes

## Edge Styles

- `-->` solid arrow - Normal transitions
- `-.->` dashed arrow - Conditional (from `When` annotation)
