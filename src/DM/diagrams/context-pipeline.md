# Context Pipeline

How world state flows into templates. This is the core of LLM behavior modulation.

## DMContext Assembly

```mermaid
flowchart TD
    subgraph Inputs["WorldState + Scene + Mood"]
        WS[WorldState]
        AS[ActiveScene]
        M[DMMood]
    end

    subgraph Build["buildDMContext"]
        WS --> PS[PlayerState]
        WS --> PC[calculatePrecarity]
        M --> MVC[buildMoodContext]
        AS --> LOC[Location lookup]
        AS --> NPC[enrichNpc per present]
        AS --> BEATS[renderBeat per beat]
        AS --> STYLE[SceneStyle]
        WS --> CLK[Clocks: visible/hidden split]
        WS --> THR[Threads: filter Simmering]
        WS --> RUM[Rumors: filter > Whisper]
        WS --> FAC[Factions: summarize]
        WS --> ECHO[recentCosts, unresolvedThreats]
        WS --> ENTRY[sceneEntryContext]
    end

    subgraph Output["DMContext (20+ fields)"]
        CTX[DMContext]
    end

    PS --> CTX
    PC --> CTX
    MVC --> CTX
    LOC --> CTX
    NPC --> CTX
    BEATS --> CTX
    STYLE --> CTX
    CLK --> CTX
    THR --> CTX
    RUM --> CTX
    FAC --> CTX
    ECHO --> CTX
    ENTRY --> CTX

    CTX --> TMPL[renderForMood]
```

**Source:** `Context.hs` buildDMContext

---

## Precarity Calculation

Precarity determines narrative voice intensity.

```mermaid
flowchart LR
    subgraph Inputs
        S[stress]
        H[heat]
        W[wanted × 2]
    end

    S --> SUM[Sum]
    H --> SUM
    W --> SUM

    SUM --> |"< 5"| OS[OperatingFromStrength]
    SUM --> |"5-9"| RM[RoomToManeuver]
    SUM --> |"10-14"| WC[WallsClosingIn]
    SUM --> |"≥ 15"| HBT[HangingByThread]
```

Note: `hunted` and `recovering` params were removed - if needed later, add fields to PlayerState.

| Precarity | Score | Prose Style |
|-----------|-------|-------------|
| OperatingFromStrength | < 5 | Expansive, bold, carelessness has costs |
| RoomToManeuver | 5-9 | Noir cool, professional, measured |
| WallsClosingIn | 10-14 | Tense, options narrowing, vice tightening |
| HangingByThread | ≥ 15 | Desperate, staccato, world compressed |

**Source:** `Context.hs` calculatePrecarity

---

## MoodVariantContext (~40 nullable fields)

Different mood variants populate different subsets:

```mermaid
flowchart TD
    subgraph DMMood
        MS[MoodScene]
        MA[MoodAction]
        MAF[MoodAftermath]
        MT[MoodTrauma]
        MB[MoodBargain]
    end

    subgraph SceneVariants
        ENC[Encounter]
        OPP[Opportunity]
        DIS[Discovery]
    end

    subgraph ActionVariants
        CTRL[Controlled]
        RSK[Risky]
        DESP[Desperate]
    end

    subgraph AftermathVariants
        CLN[Clean]
        CST[Costly]
        SET[Setback]
        DST[Disaster]
    end

    MS --> ENC
    MS --> OPP
    MS --> DIS

    MA --> CTRL
    MA --> RSK
    MA --> DESP
    MA --> DOM[ActionDomain]

    MAF --> CLN
    MAF --> CST
    MAF --> SET
    MAF --> DST
    MAF --> ATAC[ActionToAftermathContext]

    MT --> TVAR[Breaking variant]
    MB --> BVAR[Bargaining variant]
```

### Fields by Variant

| Variant | Fields Populated |
|---------|------------------|
| Encounter | mvcSceneType, mvcSource, mvcSceneUrgency, mvcEscapable |
| Opportunity | mvcSceneType, mvcOfferedBy, mvcNature, mvcCatch |
| Discovery | mvcSceneType, mvcWhat, mvcImplications |
| Controlled | mvcPosition, mvcAdvantageSource, mvcOpportunity, mvcThreat |
| Risky | mvcPosition, mvcThreat, mvcOpportunity |
| Desperate | mvcPosition, mvcWhyDesperate, mvcThreat, mvcOpportunity, mvcPotentialTrauma |
| Clean | mvcOutcomeType, mvcWhatAchieved + action context |
| Costly | mvcOutcomeType, mvcWhatAchieved, mvcCostsPaid, mvcNewComplications + action context |
| Setback | mvcOutcomeType, mvcWhatWentWrong, mvcImmediateDanger, mvcEscapeRoute + action context |
| Disaster | mvcOutcomeType, mvcWhatWentWrong, mvcImmediateDanger, mvcPotentialTrauma + action context |
| Breaking | mvcTraumaType, mvcWhatBroke, mvcAdrenaline |
| Bargaining | mvcBargainContext, mvcCanRetreat, mvcRetreatDesc, mvcPassOutDesc |

**Action context fields** (carried into Aftermath): mvcActionDieChosen, mvcActionPosition, mvcActionEffect, mvcActionTier, mvcActionOtherDice, mvcActionDomain, mvcActionStakes

**Source:** `Context.hs` MoodVariantContext, variant context builders

---

## SceneStyle (3 Compositional Axes)

```mermaid
flowchart LR
    subgraph Atmosphere["Atmosphere (Supernatural)"]
        MUN[Mundane]
        LIM[Liminal]
        SUP[Supernatural]
    end

    subgraph Pressure["Pressure (Tension)"]
        CLM[Calm]
        WTC[Watchful]
        URG[Urgent]
    end

    subgraph Class["ClassRegister (Social)"]
        GUT[Gutter]
        STR[Street]
        SAL[Salon]
    end

    MUN --> PROSE[Prose Style]
    LIM --> PROSE
    SUP --> PROSE
    CLM --> PROSE
    WTC --> PROSE
    URG --> PROSE
    GUT --> PROSE
    SAL --> PROSE
    STR --> PROSE
```

| Axis | Values | Effect on Prose |
|------|--------|-----------------|
| Atmosphere | Mundane | Physics works, ghosts are stories |
| | Liminal | Wrongness accumulates, peripheral movement |
| | Supernatural | Ghosts present, electroplasm pulsing |
| Pressure | Calm | Long sentences, breathing room |
| | Watchful | Background tension, subtext |
| | Urgent | Staccato, short sentences, compress |
| ClassRegister | Gutter | Rot, desperation, blunt dialogue |
| | Street | Working criminal, codes, reputation |
| | Salon | Polished, calculated, poison over blade |

**Source:** `State.hs` SceneStyle, `templates/_shared/style_shards.jinja`

---

## Output Schema Per Mood

Each mood has a DIFFERENT JSON schema for structured output:

```mermaid
flowchart TD
    subgraph Moods
        SC[Scene]
        AC[Action]
        AF[Aftermath]
        TR[Trauma]
        BG[Bargain]
    end

    subgraph Schemas
        SCS["narration, coinDelta, suggestedActions"]
        ACS["narration, suggestedActions (dice via spend_die tool)"]
        AFS["narration, coinDelta, suggestedActions, costDescription, threatDescription"]
        TRS["narration, **traumaAssigned** (REQUIRED), suggestedActions"]
        BGS["narration, suggestedActions"]
    end

    SC --> SCS
    AC --> ACS
    AF --> AFS
    TR --> TRS
    BG --> BGS
```

**Key differences:**
- **Action** calls `spend_die` tool for dice mechanics (no structured output for dice)
- **Trauma** requires `traumaAssigned` (the scar gained)
- **Aftermath** has `costDescription` and `threatDescription` for echoing
- **Scene** has no stress/heat deltas (all consequences through dice)
- `continueScene` was removed from all schemas

**Source:** `Templates.hs` mood-specific output schemas

---

## Template Variable Consumption

What templates actually read from DMContext:

```mermaid
flowchart LR
    subgraph DMContext
        CP[ctxPlayer]
        CPR[ctxPrecarity]
        CSS[ctxSceneStyle]
        CMV[ctxMoodVariant]
        CD[ctxDice]
        CL[ctxLocation]
        CN[ctxPresentNpcs]
        CC[ctxVisibleClocks]
        CT[ctxActiveThreads]
        CRC[ctxRecentCosts]
        CUT[ctxUnresolvedThreats]
        CSE[ctxSceneEntry]
        CRR[ctxRelevantRumors]
        CF[ctxFactionsInPlay]
        CSG[ctxSessionGoals]
    end

    subgraph PlayerDerived
        CP --> |".stress"| STHRESH[Stress Thresholds]
        CP --> |".heat"| HTHRESH[Heat Thresholds]
        CP --> |".wanted"| WTHRESH[Wanted Thresholds]
        CP --> |".trauma"| THAND[Trauma Handlers]
    end

    subgraph StyleDerived
        CSS --> |".ssAtmosphere"| ATMOS[Atmosphere Prose]
        CSS --> |".ssPressure"| PRESS[Pressure Prose]
        CSS --> |".ssClass"| CLASS[Class Prose]
    end

    CPR --> PRECPRSE[Precarity Prose]
    CMV --> MVPRSE[Variant-Specific Content]
    CD --> DICEPRSE[Dice Display]
    CSE --> ENTRYPRSE[Entry Context Block]
    CRC --> ECHOPROCE[Echo: "Still rippling"]
    CUT --> SHADOWPRSE[Shadow: "Still looming"]
```

### Stress Thresholds (affects prose)

| Stress | Label | Effect |
|--------|-------|--------|
| ≥ 8 | Fraying | Short sentences, physical symptoms, "one bad moment from breaking" |
| ≥ 5 | Wearing | Cracks visible, NPCs notice |
| ≥ 3 | Functional | Occasional tells |
| < 3 | Clean | Confident prose |

### Heat Thresholds (affects narrative)

| Heat | Label | Effect |
|------|-------|--------|
| ≥ 8 | Hunting | NPCs nervous, surveillance present |
| ≥ 5 | Watching | Strangers linger, prices higher |
| ≥ 3 | Ripples | Rumors spreading |

### Trauma Handlers

Templates have explicit prose guidance for each trauma type:
- **Paranoid**: Threats that may/may not be real
- **Cold**: Clinical observation, emotional distance
- **Obsessed**: Fixation bleeds into everything
- **Reckless**: Safe options feel like cowardice
- **Haunted**: Ghosts, memories, faces in crowds
- **Vicious**: Violence as first solution
- **Volatile**: Emotions spike without warning
- **Soft**: Every victim is personal

**Source:** `templates/scene/main.jinja`, `templates/action/main.jinja`

---

## Scene Entry Context

Templates know HOW you arrived at the current scene:

```mermaid
flowchart TD
    subgraph Previous["Previous Phase"]
        AFT[Aftermath]
        TRM[Trauma]
        FRESH[Fresh]
    end

    subgraph Entry["SceneEntryContext"]
        EAFT["EntryFromAftermath<br/>transitionNote, unresolvedThreats, recentCosts"]
        ETRM["EntryFromTrauma<br/>traumaGained, adrenalineActive"]
        EFRESH["EntryFresh"]
    end

    AFT --> EAFT
    TRM --> ETRM
    FRESH --> EFRESH
```

Note: `EntryFromDowntime` was removed - downtime phase not implemented.

Templates render this as `<how_they_arrived>` block with phase-specific prose.

**Source:** `State.hs` SceneEntryContext, `templates/scene/main.jinja`

---

## Filtering Logic

Not all world state reaches templates:

| Source | Filter | Result |
|--------|--------|--------|
| Clocks | `clockVisible` | ctxVisibleClocks vs ctxHiddenClocks |
| Threads | `tension != Simmering` | ctxActiveThreads |
| Rumors | `spread > Whisper` | ctxRelevantRumors |
| NPCs | Present in scene | Enriched with disposition, currentWant |
| Factions | All | Summarized with attitude, currentGoal |

**Source:** `Context.hs` buildDMContext filtering logic
