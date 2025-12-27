# Context Window Architecture

How the Tidepool DM constructs what gets sent to the LLM at each phase.

## Overview

```
┌─────────────────────────────────────────────────────────────┐
│                      API CALL STRUCTURE                      │
├─────────────────────────────────────────────────────────────┤
│  System Prompt: [rules + mood guidance + world state]       │
│                 (dynamic per turn, ~2-4k tokens)            │
├─────────────────────────────────────────────────────────────┤
│  History:       [action₁, response₁, action₂, response₂...] │
│                 (lean pairs only, no system prompts)        │
├─────────────────────────────────────────────────────────────┤
│  User Message:  "I punch the bartender"                     │
│                 (player action ONLY)                        │
├─────────────────────────────────────────────────────────────┤
│  Tools:         [think_as_dm, speak_as_npc, engage, ...]    │
│  Output Schema: TurnOutput { narration, stressDelta, ... }  │
└─────────────────────────────────────────────────────────────┘
```

## 1. System Prompt Construction

The system prompt is **dynamic per turn**, built from:

```haskell
-- DM/Loop.hs
let systemPrompt = renderForMood mood context
```

### What Goes Into It

| Component | Source | Content |
|-----------|--------|---------|
| Identity | Jinja template | "You are the Dungeon Master for Blades in the Dark..." |
| Mood Guidance | Per-mood template | Scene: "use engage for risky actions", Action: "use spend_die" |
| World State | DMContext | Location, NPCs, clocks, threads, stress, coin |
| Voice Modulation | Precarity score | HangingByThread = terse; RoomToManeuver = noir cool |
| Consequence Echo | Recent costs/threats | "Still echoing: The Lampblacks noticed you..." |

### Template Selection

```haskell
-- DM/Templates.hs
renderForMood :: DMMood -> DMContext -> Text
renderForMood mood ctx = case mood of
  MoodScene _     -> render sceneTemplate ctx    -- templates/scene/main.jinja
  MoodAction _ _  -> render actionTemplate ctx   -- templates/action/main.jinja
  MoodAftermath _ -> render aftermathTemplate ctx
  MoodDowntime _  -> render downtimeTemplate ctx
```

### DMContext Fields (what templates can access)

```
ctxPlayer          -- stress, coin, heat, wanted level
ctxPrecarity       -- OperatingFromStrength | RoomToManeuver | WallsClosingIn | HangingByThread
ctxMood            -- Current mood enum
ctxMoodVariant     -- Rich variant data (encounter source, opportunity catch, etc.)
ctxDice            -- Available dice pool, locked outcome if any
ctxLocation        -- Name, description, who controls it
ctxPresentNpcs     -- NPCs with disposition labels and current wants
ctxSceneBeats      -- Rendered history of this scene
ctxStakes          -- "What's at stake?"
ctxVisibleClocks   -- Progress clocks player can see
ctxActiveThreads   -- Non-simmering narrative threads
ctxRecentCosts     -- Last 3 costly outcomes (for echoing)
ctxUnresolvedThreats -- Complications not yet addressed
```

## 2. User Message Construction

The user message is **only the player's action**:

```haskell
-- DM/Loop.hs
runMoodAwareTurn input.piActionText

-- Tidepool/Effect.hs
let actionMsg = Message User [TextBlock userAction]
```

Examples:
- `"I punch the bartender"`
- `"I look around for exits"`
- `"I try to pick his pocket"`

**Not** the giant template. Just the action.

## 3. History Management

History stores **only action/response pairs**:

```haskell
-- Tidepool/Effect.hs (after turn completes)
let actionMsg = Message User [TextBlock userAction]
    assistantMsg = Message Assistant finalContent
appendMessages [actionMsg, assistantMsg]
```

**What's stored:**
```
[
  User: "I approach the bar"
  Assistant: "Telda looks up from polishing a glass..."
  User: "I order a drink"
  Assistant: "She pours you something amber..."
]
```

**What's NOT stored:**
- System prompts (rebuilt fresh each turn)
- Tool use blocks (ephemeral)
- Thinking blocks (internal)

## 4. Tool Continuation

When the LLM calls tools, messages accumulate:

```haskell
-- Tidepool/Effect.hs (tool loop)
let assistantMsg = Message Assistant content  -- includes ToolUseBlock
    userMsg = Message User (map ToolResultBlock toolResults)
    newMessages = msgs ++ [assistantMsg, userMsg]
```

**Example mid-turn:**
```
[...prior history...]
User: "I punch the bartender"
Assistant: [ToolUseBlock "engage" {position: "Risky", effect: "Standard"}]
User: [ToolResultBlock {result: "Transitioning to Action mood"}]
```

Tool breaks (`engage`, `resolve`, `accept`) cause the turn to restart with a new mood's template.

## 5. Per-Phase Context

### SCENE MODE

**System Prompt:**
- Identity (Encounter/Opportunity/Discovery variant)
- World state (location, NPCs with wants, visible clocks)
- Scene history (prior beats)
- **Mandatory:** "BEFORE YOU WRITE ANYTHING, check: Is the player attempting a risky action?"

**Tools:** think_as_dm, speak_as_npc, ask_player, engage, accept

### ACTION MODE

**System Prompt:**
- Position/Effect context
- Locked outcome (if die already chosen)
- Stress room calculation
- Domain context (infiltration/social/violence)

**Tools:** think_as_dm, speak_as_npc, spend_die, resolve

### AFTERMATH MODE

**System Prompt:**
- Outcome type (Clean/Costly/Setback/Disaster)
- What was achieved/what went wrong
- Costs paid/complications

**Tools:** think_as_dm, speak_as_npc, accept

### DOWNTIME MODE

**System Prompt:**
- Downtime type (Recovery/Project/Entanglement)
- Time available, activities, NPCs
- "Cannot engage from downtime"

**Tools:** think_as_dm, speak_as_npc (no engage!)

## 6. Compression

Triggered when scene beats exceed threshold (20):

```haskell
-- DM/Loop.hs
let systemPrompt = renderCompression ctx
    userAction = "Compress this scene."
outcome <- runTurn systemPrompt userAction compressionOutputSchema.schemaJSON []
```

**Output:** Summary, key moments, consequence seeds, stress/coin deltas

**Effect:** Clears scene beats, stores summary for future echoing

## Key Design Decisions

1. **Dynamic System Prompts** - Each turn gets fresh context with current world state
2. **Lean History** - No bloat from repeated templates
3. **Precarity = Voice** - Stress level modulates narrative tone
4. **Consequence Echoing** - Recent costs bubble into future scenes
5. **Locked Outcomes** - Once die chosen, outcome tier is fixed
6. **Tool Breaks** - Mood transitions restart the turn with new template

## File References

| File | Role |
|------|------|
| `src/DM/Loop.hs` | Orchestrates turn flow, calls runTurn |
| `src/DM/Context.hs` | Builds DMContext from WorldState |
| `src/DM/Templates.hs` | Renders mood-specific templates |
| `src/Tidepool/Effect.hs` | LLM effect, history management, API calls |
| `templates/scene/main.jinja` | Scene mood template |
| `templates/action/main.jinja` | Action mood template |
