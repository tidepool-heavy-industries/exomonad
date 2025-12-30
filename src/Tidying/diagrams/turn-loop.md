# Turn Loop

The core loop of the Tidying agent. Each turn: Mode → Template → LLM → Output → Update.

## Core Loop

```mermaid
flowchart TD
    subgraph Turn["Single Turn"]
        M["Mode (sum type with data)"]
        T["Template (mode data = Jinja context)"]
        L["LLM Call<br/>(multimodal: photos + text + history)"]
        O["Structured Output<br/>(mode-specific schema)"]
        U["Update Mode Data"]
    end

    M --> T
    T --> L
    L --> O
    O --> U

    U -->|No transition tool| M
    U -->|Transition tool called| TR

    subgraph TR["Transition"]
        NT["Tool creates NEW Mode<br/>(args → initial data)"]
        TB["ToolBreak"]
        SY["Synthetic message injected"]
    end

    NT --> TB --> SY --> M
```

## Detailed Turn Flow

```mermaid
flowchart TD
    A["Turn starts"]
    B["Get Mode from SessionState"]
    C["Select template for Mode"]
    D["Select tools for Mode"]
    E["Select output schema for Mode"]
    F["Render system prompt<br/>(template + mode data as context)"]
    G["LLM call<br/>(system prompt + history + photos + text)"]
    H["Parse structured output"]
    I{"Tool calls?"}
    J["Execute tools"]
    K{"Any ToolBreak?"}
    L["Update mode data from output"]
    M["Return response"]
    N["Handle ToolBreak:<br/>1. Change Mode (args → data)<br/>2. End turn<br/>3. Inject synthetic message"]
    O["Start new turn"]

    A --> B --> C --> D --> E --> F --> G --> H --> I
    I -->|No| L
    I -->|Yes| J --> K
    K -->|No| L
    K -->|Yes| N --> O --> B
    L --> M
```

## Three Data Flows

```mermaid
flowchart LR
    subgraph Flow1["1. Mode Data → Template"]
        MD1["Mode Data<br/>(e.g., ClarifyingData)"]
        T1["Jinja Template"]
        P1["System Prompt"]
    end

    subgraph Flow2["2. Structured Output → Mode Data"]
        SO["LLM Structured Output<br/>{current_item, location}"]
        MD2["Update Mode Data"]
    end

    subgraph Flow3["3. Transition Tool → New Mode"]
        TT["Transition Tool Args<br/>{item, context, reason}"]
        NM["New Mode<br/>Clarifying(ClarifyingData)"]
    end

    MD1 --> T1 --> P1
    SO --> MD2
    TT --> NM
```

## Multimodal LLM Call

Photos and text are sent together in a single call:

```mermaid
flowchart TD
    subgraph Input["User Input"]
        P["Photos (base64/URL)"]
        T["Text message"]
    end

    subgraph Context["Built Context"]
        S["System Prompt<br/>(from mode template)"]
        H["Conversation History<br/>(accumulated, compressed)"]
    end

    subgraph Call["Single LLM Call"]
        API["Anthropic API<br/>- system: rendered template<br/>- messages: history + [photos, text]<br/>- tools: mode-specific<br/>- tool_choice: auto"]
    end

    subgraph Output["Response"]
        R["Structured Output<br/>(mode-specific schema)"]
        TC["Tool Calls<br/>(if any)"]
    end

    P --> API
    T --> API
    S --> API
    H --> API
    API --> R
    API --> TC
```

## Mode Selection

```haskell
templateForMode :: Mode -> Template
templateForMode (Surveying _)       = surveyingTemplate
templateForMode (Sorting _)         = sortingTemplate
templateForMode (Clarifying _)      = clarifyingTemplate
templateForMode (DecisionSupport _) = decisionSupportTemplate
templateForMode (WindingDown _)     = windingDownTemplate

toolsForMode :: Mode -> [Tool]
toolsForMode (Surveying _)       = [beginSorting]
toolsForMode (Sorting _)         = [proposeDisposition, needToClarify, userSeemsStuck, timeToWrap]
toolsForMode (Clarifying _)      = [resumeSorting, skipItem]
toolsForMode (DecisionSupport _) = [resumeSorting]
toolsForMode (WindingDown _)     = [endSession]

schemaForMode :: Mode -> Schema
schemaForMode (Surveying _)       = surveyingOutputSchema
schemaForMode (Sorting _)         = sortingOutputSchema
-- etc.
```

## ToolBreak vs ToolSuccess

```mermaid
flowchart TD
    T["Tool Execution"]
    T --> TS["ToolSuccess Value"]
    T --> TB["ToolBreak Text"]

    TS --> C["Continue current turn<br/>(may call more tools)"]
    TB --> E["End current turn"]
    E --> I["Inject synthetic message"]
    I --> N["Start new turn<br/>(with new Mode)"]
```

## Example Turn Sequence

**Sorting mode, user says "idk what that is"**:

```mermaid
sequenceDiagram
    participant U as User
    participant L as Loop
    participant LLM
    participant T as Tool

    U->>L: "idk what that is"
    L->>L: Mode = Sorting(SortingData)
    L->>L: template = sortingTemplate
    L->>L: tools = [proposeDisposition, needToClarify, ...]
    L->>LLM: Call with Sorting template + tools
    LLM->>T: need_to_clarify(item="green device", reason="user confused")
    T->>L: ToolBreak("[Continue as: Clarifying]")
    L->>L: Mode = Clarifying(ClarifyingData {...})
    L->>L: End turn, inject synthetic message
    L->>LLM: New turn with Clarifying template + tools
    LLM->>U: "The green device on the floor, between the desk leg and wall..."
```

## Conversation History

History accumulates across turns and is occasionally compressed:

```mermaid
flowchart TD
    H["Conversation History"]
    T["New Turn"]
    C{"Length > threshold?"}
    CP["Compress older messages"]
    A["Append new exchange"]

    H --> T --> C
    C -->|Yes| CP --> A
    C -->|No| A
    A --> H
```

## Key Files

- `Loop.hs` - `runTurn`, ToolBreak handling
- `Templates.hs` - `templateForMode`, template rendering
- `Tools.hs` - `toolsForMode`, tool definitions
- `Output.hs` - Mode-specific output schemas
