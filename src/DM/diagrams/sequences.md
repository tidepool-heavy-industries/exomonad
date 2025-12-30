# Sequence Diagrams

Turn-by-turn message flows.

## Main Turn Sequence (dmTurn)

The core game loop. Shows how player input flows through context building, LLM call, and state mutation.

```mermaid
sequenceDiagram
    participant P as Player
    participant L as Loop.dmTurn
    participant C as Context
    participant T as Template
    participant LLM as Claude
    participant D as Dispatch
    participant S as State

    P->>L: PlayerInput (action text)
    L->>S: Record beat (PlayerAction)
    L->>L: checkPendingInterrupts

    alt Clock forces action
        L->>S: updateMood(MoodAction)
        L->>L: restart turn
    end

    L->>S: Extract scene + mood from PhasePlaying
    L->>C: buildDMContext(scene, mood, state)
    C-->>L: DMContext
    L->>T: renderForMood(mood, context)
    T-->>L: system prompt

    L->>LLM: runTurn(prompt, tools, schema)

    loop Tool calls
        LLM->>D: tool invocation
        D->>S: modify state
        D-->>LLM: result
    end

    LLM-->>L: TurnResult

    alt TurnBroken (mood transition)
        L->>L: restart with new mood
    else TurnCompleted
        L->>S: applyTurnOutput (deltas)
        L->>S: checkClockConsequences
        L->>S: checkTraumaTrigger
        L->>S: compressIfNeeded (20+ beats)
        L-->>P: Response (narrative + deltas)
    end
```

**Source:** `Loop.hs` dmTurn

### Key Steps

1. **Record input** - Append to `sceneBeats` for history
2. **Check interrupts** - Clock may have forced an action
3. **Build context** - Gather all state into DMContext
4. **Render template** - Mood selects which template
5. **Call LLM** - With tools and output schema
6. **Dispatch tools** - Tools modify state, may trigger transition
7. **Apply deltas** - stress/heat/coin changes
8. **Check consequences** - Clock completion, trauma trigger
9. **Compress** - Summarize if 20+ beats

---

## Dice Selection Flow

The precommitted outcome pattern. LLM calls `spend_die` tool, which handles dice selection and populates `pendingOutcome` for the `resolve` tool.

```mermaid
sequenceDiagram
    participant P as Player
    participant L as Loop
    participant LLM as Claude
    participant T as SpendDie Tool
    participant G as GUI
    participant S as State

    Note over L,LLM: MoodAction active

    L->>LLM: runTurn with action template
    LLM->>T: spend_die { situation, position, outcomes }

    Note over T: outcomes has hint+costs for each die

    T->>G: requestDice(situation, diceWithHints)
    G->>P: Show dice cards with hints

    Note over G: [4] +1 stress "Scrape through"<br/>[2] +2 stress "Barely..."<br/>[6] +0 stress "Clean break"

    P->>G: Select die
    G-->>T: selectedIdx

    T->>S: Remove die from pool
    T->>S: Apply stressCost, heatCost, coinDelta
    T->>S: Set pendingOutcome
    T->>S: Emit DieSpent event
    T-->>LLM: SpendDieResult { tier, narrative }

    LLM->>T: resolve { outcome, what, costs }
    T->>S: updateMood(MoodAftermath)
    T-->>LLM: ()

    Note over L: Aftermath template renders, LLM calls accept

    L-->>P: Aftermath narrative
```

**Source:** `Tools.hs` SpendDie, Resolve

### What Player Sees

```
Pool: [4] [2] [6]

[4] +1 stress, +0 heat — "Scrape through, bruised"
[2] +2 stress, +1 heat — "Barely, and they saw your face"
[6] +0 stress, +0 heat — "Clean break"
```

High dice = good outcomes but cost opportunity. Low dice = bad outcomes but save high dice for later.
