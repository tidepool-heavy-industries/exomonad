# Flowcharts

Decision flows and data pipelines.

## Tool Dispatch

How tool calls are executed and how transition tools restart the loop.

```mermaid
flowchart TD
    A[Tool Invocation] --> B{In transitionToolNames?}
    B -->|No| C[Execute tool directly]
    C --> D[Return ToolSuccess]

    B -->|Yes| E[Capture mood before]
    E --> F[Execute tool]
    F --> G[Capture mood after]
    G --> H{Mood changed?}
    H -->|No| D
    H -->|Yes| I[Return ToolBreak]
    I --> J[Loop restarts with new mood]

    subgraph transitionToolNames
        T1[engage]
        T2[resolve]
        T3[accept_bargain]
        T4[retreat]
        T5[pass_out]
    end
```

**Source:** `Tools.hs` transitionToolNames

```haskell
transitionToolNames :: [Text]
transitionToolNames = ["engage", "resolve", "accept_bargain", "retreat", "pass_out"]
```

Note: `accept` is NOT a transition tool - it completes the turn, doesn't restart it.

---

## Clock Completion & Consequences

What happens when a clock fills.

```mermaid
flowchart TD
    A[Clock ticks] --> B{clockFilled >= clockSegments?}
    B -->|No| C[Continue]
    B -->|Yes| D[checkClockConsequences]

    D --> E[Emit ClockCompleted event<br/>with narrative text]
    D --> F[Remove clock from state]

    E --> G[LLM interprets narrative<br/>in next turn]
    G --> H[LLM decides mechanical effects<br/>stress/heat/coin/etc]
```

**Source:** `Loop.hs` checkClockConsequences

### Narrative-Based Consequences

Clock consequences are 2-3 sentences of narrative text describing what happens when the clock fills. The LLM interprets this at runtime and applies appropriate mechanical effects through its normal output (stress/heat/coin deltas).

---

## Compression Flow

Scene beats get summarized when they exceed 20 entries.

```mermaid
flowchart TD
    A[Turn completes] --> B{sceneBeats.length >= 20?}
    B -->|No| C[Continue]
    B -->|Yes| D[buildCompressionContext]

    D --> E[renderCompression template]
    E --> F[Call LLM with compression schema]
    F --> G[Parse CompressionOutput]

    G --> H[Create SceneSummary]
    H --> I[Append to sessionHistory]

    G --> J[Extract newRumors]
    J --> K[Append to state.rumors]

    G --> L[Clear sceneBeats]
    L --> M[Emit SceneCompressed]
```

**Source:** `Loop.hs` compressIfNeeded, `Output.hs` CompressionOutput

### CompressionOutput

```haskell
data CompressionOutput = CompressionOutput
  { summary :: Text           -- One paragraph summary
  , keyMoments :: [Text]      -- 3-5 key moments
  , consequenceSeeds :: Text  -- Comma-separated seeds
  , stressChange :: Int       -- Net stress change
  , coinChange :: Int         -- Net coin change
  , newRumors :: [RumorInit]  -- Rumors that emerged
  }
```

---

## Event Flow (Game -> GUI)

How events flow from the effectful game loop to the threepenny GUI.

```mermaid
flowchart LR
    subgraph Loop["Game Loop (effectful)"]
        E1[emit StressChanged]
        E2[emit DieSpent]
        E3[emit MoodTransition]
        E4[emit NarrativeAdded]
    end

    subgraph Bridge["GUIBridge (STM)"]
        T1[TVar gbState]
        T2[TVar gbNarrativeLog]
        T3[TVar gbDebugLog]
    end

    subgraph GUI["Threepenny GUI"]
        G1[Stats panel]
        G2[Narrative pane]
        G3[Debug panel]
    end

    E1 --> T1
    E2 --> T2
    E3 --> T3
    E4 --> T2

    T1 -->|poll 100ms| G1
    T2 -->|poll 100ms| G2
    T3 -->|poll 100ms| G3
```

**Source:** `Tidepool/GUI/Core.hs` GUIBridge type, `DM/GUI/App.hs` polling loop

### Key Events

| Event | Purpose |
|-------|---------|
| NarrativeAdded | Primary channel for DM prose to GUI |
| StressChanged | Triggers stats panel update |
| DieSpent | Logs die usage to narrative |
| MoodTransition | Debug tracking of state changes |
| ClockCompleted | Fires consequence, updates clocks panel |
