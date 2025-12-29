# OODA Loop

The tidying agent follows an OODA (Observe-Orient-Decide-Act) pattern.

## Main OODA Cycle

```mermaid
flowchart TD
    subgraph OBSERVE["OBSERVE (Photo Analysis)"]
        O1["analyzePhotos [Photo]"]
        O2["LLM Vision API"]
        O3["PhotoAnalysis<br/>roomType, chaosLevel<br/>visibleItems, blockedFunction<br/>firstTarget"]
    end

    subgraph ORIENT["ORIENT (Extraction)"]
        R1["extractFromInput"]
        R2["LLM + extractSchema"]
        R3["Extract<br/>intent, item, choice<br/>place, function, anchors"]
    end

    subgraph DECIDE["DECIDE (Pure Routing)"]
        D1["decideFromExtract"]
        D2["Pattern match on<br/>Phase + Intent + Choice<br/>+ PhotoAnalysis"]
        D3["(Action, Phase)"]
    end

    subgraph ACT["ACT (Response)"]
        A1["actResponse"]
        A2{"Canned<br/>response?"}
        A3["Return canned"]
        A4["LLM + tidyingTools"]
        A5["Text response"]
    end

    UserInput --> O1
    O1 --> O2 --> O3
    O3 --> R1
    UserInput --> R1
    R1 --> R2 --> R3
    R3 --> D1
    SessionState --> D1
    O3 --> D1
    D1 --> D2 --> D3
    D3 --> A1
    A1 --> A2
    A2 -->|yes| A3
    A2 -->|no| A4
    A3 --> A5
    A4 --> A5
    A5 --> Response
```

## Photo-Aware Fast-Track Routing

Photo analysis enables fast-track routing decisions:

```mermaid
flowchart LR
    subgraph Photo["PhotoAnalysis"]
        P1["chaosLevel == Buried"]
        P2["hasBlockedFunction"]
        P3["chaosLevel == Clear"]
    end

    subgraph Action["Fast-Track Actions"]
        A1["Skip Surveying questions<br/>→ FirstInstruction<br/>→ Sorting"]
        A2["Start sorting immediately<br/>(if hasFunction)"]
        A3["AckProgress<br/>'Space looking clear!'"]
    end

    P1 --> A1
    P2 --> A2
    P3 --> A3
```

## OBSERVE: Photo Analysis

```mermaid
flowchart TD
    A["analyzePhotos [Photo]"]
    B{"Photos<br/>empty?"}
    C["Return Nothing"]
    D["Convert to ImageSource<br/>URL or Base64"]
    E["LLM Vision API<br/>photoAnalysisSchema"]
    F{"Parse<br/>success?"}
    G["PhotoAnalysis"]
    H["stubPhotoAnalysis<br/>(fallback)"]

    A --> B
    B -->|yes| C
    B -->|no| D --> E --> F
    F -->|yes| G
    F -->|no| H
```

## ORIENT: Extraction

```mermaid
flowchart TD
    A["extractFromInput"]
    B["Build context:<br/>photo analysis + user text"]
    C["LLM call with extractSchema"]
    D{"Parse<br/>success?"}
    E["Extract<br/>{intent, item, choice,<br/>place, function, anchors}"]
    F["Fallback Extract"]

    A --> B --> C --> D
    D -->|yes| E
    D -->|no| F
```

### Extract Fields

| Field | Type | Description |
|-------|------|-------------|
| `exIntent` | `Intent` | start/continue/item/decided/help/stop |
| `exItem` | `Maybe ItemName` | Item mentioned |
| `exChoice` | `Maybe Choice` | trash/keep/place/unsure |
| `exPlace` | `Maybe Location` | Where to put it |
| `exFunction` | `Maybe SpaceFunction` | Space purpose |
| `exAnchors` | `Maybe [ItemName]` | Anchor items |

## DECIDE: Pure Routing

No LLM calls - pure function from state to action:

```mermaid
flowchart TD
    A["decideFromExtract<br/>SessionState + PhotoAnalysis + Extract"]
    B{"Intent ==<br/>Stop?"}
    C["Summary"]
    D{"Intent ==<br/>Help?"}
    E["DecisionAid"]
    F["Phase-specific<br/>routing"]

    A --> B
    B -->|yes| C
    B -->|no| D
    D -->|yes| E
    D -->|no| F
```

## ACT: Response Generation

```mermaid
flowchart TD
    A["actResponse ctx action"]
    B{"cannedResponse<br/>exists?"}
    C["Return canned<br/>'Trashed! Next?'"]
    D["LLM call with<br/>tidyingTools"]
    E["Text response"]

    A --> B
    B -->|yes| C --> E
    B -->|no| D --> E
```

## Key Files

- `Loop.hs` - tidyingTurn, analyzePhotos, extractFromInput
- `Decide.hs` - decideFromExtract
- `Act.hs` - actResponse, cannedResponse
- `Output.hs` - Extract, Intent, Choice schemas
