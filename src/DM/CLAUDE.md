# DM Agent - Exhaustive System Documentation

This document provides complete context for the Blades in the Dark-style Dungeon Master agent. For the Tidepool library (effects, GUI, templates), see the root `CLAUDE.md`.

## Core Loop Flow

```
Player Input → dmTurn
  │
  ├─1. Record input as scene beat
  ├─2. Build DMContext from WorldState
  ├─3. Select template by current mood
  ├─4. Render Jinja template → system prompt
  ├─5. Call LLM with tools + output schema
  ├─6. Dispatch any tool calls (may modify state)
  ├─7. Parse structured output (TurnOutput)
  ├─8. Apply deltas (stress, heat, coin, dice)
  ├─9. Handle dice selection if diceAction present
  ├─10. Check clock completions → dispatch consequences
  ├─11. Check for mood transitions
  ├─12. Compress if scene ended
  └─13. Return narrative response
```

## Mood State Machine

The game operates in distinct **moods**, each with its own template, tools, and output schema.

```
                    ┌──────────────┐
                    │    Scene     │◄────────────────────┐
                    │  (The Weaver)│                     │
                    └──────┬───────┘                     │
                           │ engage tool                 │
                           ▼                             │
                    ┌──────────────┐                     │
                    │    Action    │                     │
                    │(The Crucible)│                     │
                    └──────┬───────┘                     │
                           │ spend_die / resolve         │
                           ▼                             │
                    ┌──────────────┐                     │
              ┌─────│  Aftermath   │─────┐               │
              │     │  (The Echo)  │     │               │
              │     └──────────────┘     │               │
              │                          │               │
     stress < 9                    stress = 9            │
              │                          │               │
              ▼                          ▼               │
       ┌────────────┐            ┌────────────┐          │
       │  Downtime  │            │   Trauma   │          │
       │ (The Tide) │            │(The Crack) │          │
       └─────┬──────┘            └─────┬──────┘          │
             │                         │                 │
             │ hook pulls back         │ adrenaline or   │
             └─────────────────────────┴─────────────────┘
```

### Mood Types (State.hs)

```haskell
data DMMood
  = MoodScene SceneMoodVariant           -- Exploration, dialogue, discovery
  | MoodAction ActionVariant (Maybe PendingOutcome)  -- Dice about to roll
  | MoodAftermath AftermathVariant       -- Consequences landing
  | MoodDowntime DowntimeMoodVariant     -- Rest, recovery, projects
  | MoodTrauma TraumaMoodVariant         -- Breaking point
  | MoodBargain BargainMoodVariant       -- Devil's bargain (no dice left)
  | MoodBetweenScenes                    -- Inter-scene transition

-- Each variant carries phase-specific context
data SceneMoodVariant
  = SvEncounter { smvSource :: Text, smvUrgency :: SceneUrgency, smvEscapable :: Bool }
  | SvOpportunity { smvOfferedBy :: Maybe Text, smvNature :: Text, smvCatch :: Text }
  | SvDiscovery { smvWhat :: Text, smvImplications :: [Text] }
  | SvOpen
```

### Mood Transitions (Loop.hs)

Transitions happen via:
1. **Tool calls** - `engage` → Action, `resolve` → Aftermath
2. **Output flags** - `continueScene = False` → compression → Scene/Downtime
3. **State thresholds** - stress = 9 → Trauma
4. **Clock interrupts** - `pendingInterrupt` forces Action

## Player State

```haskell
data PlayerState = PlayerState
  { stress :: Int        -- 0-9, trauma at 9
  , coin :: Int          -- Currency
  , heat :: Int          -- 0-10, attention from authorities
  , wanted :: Int        -- 0-4, active warrants
  , trauma :: [Trauma]   -- Permanent scars (Cold, Haunted, Reckless, etc.)
  , recovering :: Bool   -- Currently healing
  , hunted :: Bool       -- Actively pursued
  }
```

### Precarity (Context.hs)

Precarity scales narrative tone based on player state:

```haskell
data Precarity
  = OperatingFromStrength  -- precarity < 5: expansive, bold
  | RoomToManeuver         -- 5-9: noir cool, professional
  | WallsClosingIn         -- 10-14: tense, options narrowing
  | HangingByThread        -- >= 15: desperate, urgent

-- Calculation:
-- stress + heat + (wanted * 2) + (hunted ? 3 : 0) + (recovering ? 2 : 0)
```

## Dice Mechanics

### Pool Management

```haskell
data DicePool = DicePool
  { poolDice :: [Int]      -- Current dice values (1-6)
  , poolMaxSize :: Int     -- Usually 6
  }
```

- Dice are **spent** on actions (removed from pool)
- Dice **recover** during downtime
- When pool is empty → MoodBargain (devil's bargain)

### Position & Effect

```haskell
data Position = Controlled | Risky | Desperate
data Effect = Limited | Standard | Great
```

Position determines consequence severity:
- **Controlled**: Failure = reduced effect, no harm
- **Risky**: Failure = consequence + reduced effect
- **Desperate**: Failure = severe consequence

Effect determines success magnitude:
- **Limited**: Partial progress
- **Standard**: Expected result
- **Great**: Exceptional result

### Outcome Tiers

```haskell
data OutcomeTier
  = TierCritical   -- 6,6 (two sixes)
  | TierSuccess    -- 6
  | TierPartial    -- 4-5
  | TierBad        -- 1-3 (Controlled/Risky)
  | TierDisaster   -- 1-3 (Desperate)
```

### DiceAction (Output.hs)

LLM precommits outcomes for each die in the pool:

```haskell
data DiceAction = DiceAction
  { situation :: Text           -- What's at stake
  , position :: Position        -- Risk level
  , outcomes :: [DieOutcome]    -- One per die in pool
  }

data DieOutcome = DieOutcome
  { dieValue :: Int       -- Which die (must match pool)
  , hint :: Text          -- 3-8 words shown before choice
  , stressCost :: Int     -- Applied after choice
  , heatCost :: Int       -- Applied after choice
  , coinDelta :: Int      -- Applied after choice
  , narrative :: Text     -- Revealed after choice
  }
```

Player sees hints + costs, chooses die, then costs are applied and narrative revealed.

## Clocks

```haskell
data Clock = Clock
  { clockName :: Text
  , clockSegments :: Int      -- 4, 6, or 8
  , clockFilled :: Int        -- 0 to segments
  , clockVisible :: Bool      -- Shown to player?
  , clockType :: ClockType    -- Threat, Goal, or Faction
  , clockConsequence :: Consequence  -- What happens when full
  }

data ClockType = ThreatClock | GoalClock | FactionClock
```

### Clock Ticking

Clocks tick via LLM structured output:

```haskell
-- In TurnOutput (downtime/aftermath)
clocksToTick :: [ClockTick]

data ClockTick = ClockTick
  { clockId :: Text
  , ticks :: Int
  }
```

Loop.hs processes these at lines 257-262.

### Consequences

When a clock fills, its consequence dispatches:

```haskell
data Consequence
  = FactionMoves FactionId FactionAction    -- Faction takes action
  | RevealSecret Secret                     -- Hidden info surfaces
  | Escalate Escalation                     -- Situation worsens
  | SpawnThread Thread                      -- New narrative thread
  | GainCoin Int                            -- Player reward
  | GainRep FactionId Int                   -- Faction attitude change
  | RemoveThreat ClockId                    -- Delete another clock
  | OpenOpportunity Text                    -- New option available
  | GainAsset Text                          -- Acquire item/resource
  | NoConsequence                           -- Marker only
```

## Tools (Tools.hs)

9 tools available to the LLM:

| Tool | Effect | Used In |
|------|--------|---------|
| `think` | Internal DM reasoning (not shown to player) | All moods |
| `speak_as_npc` | NPC dialogue with voice notes | Scene, Action |
| `ask_player` | Request clarification | Scene |
| `choose` | Present 2-4 options to player | Scene, Aftermath |
| `engage` | Initiate dice action → transitions to Action | Scene |
| `spend_die` | Player selects die from pool | Action |
| `resolve` | Conclude action → transitions to Aftermath | Action |
| `accept` | Accept bargain terms | Bargain |
| `set_scene_style` | Modify atmosphere/pressure/class | Scene |

### Tool Dispatch

```haskell
class Tool t where
  type ToolInput t
  type ToolOutput t
  toolName :: Proxy t -> Text
  toolSchema :: Proxy t -> Value
  executeTool :: ToolInput t -> Eff ToolEffects (ToolOutput t)
```

Tools emit `DMEvent`s for state changes.

## Events (Tools.hs)

14 event types for state change notifications:

```haskell
data DMEvent
  = StressChanged Int Int Text      -- old, new, reason
  | HeatChanged Int Int Text
  | CoinChanged Int Int Text
  | WantedChanged Int Int Text
  | TraumaGained Trauma
  | DieSpent Int Int                -- value, remaining pool size
  | DiceRecovered Int               -- count recovered
  | ClockTicked ClockId Int Int     -- id, old, new
  | ClockCompleted ClockId Consequence
  | FactionAttitudeChanged FactionId Attitude Attitude
  | NpcDispositionChanged NpcId Disposition Disposition
  | ThreadSpawned Thread
  | RumorSpread Rumor
  | LocationChanged LocationId LocationId
```

## Context (Context.hs)

`DMContext` is built from `WorldState` for template rendering:

```haskell
data DMContext = DMContext
  { ctxPlayer :: PlayerState
  , ctxPrecarity :: Precarity
  , ctxLocation :: Location
  , ctxPresentNpcs :: [NpcWithDisposition]
  , ctxVisibleClocks :: [Clock]
  , ctxHiddenClocks :: [Clock]           -- DM-only
  , ctxActiveThreads :: [Thread]
  , ctxRelevantRumors :: [Rumor]         -- Wired to scene template
  , ctxSessionGoals :: [Text]            -- Wired to scene template
  , ctxFactionsInPlay :: [FactionState]
  , ctxMood :: DMMood
  , ctxMoodLabel :: Text                 -- "scene", "action", etc.
  , ctxMoodVariant :: MoodVariantContext -- Phase-specific data
  , ctxSceneBeats :: [Text]              -- What happened this scene
  , ctxRecentCosts :: [Text]             -- Consequence echoing
  , ctxUnresolvedThreats :: [Text]       -- Pending complications
  , ctxSceneStyle :: SceneStyle          -- Atmosphere, pressure, class
  , ctxStakes :: Text
  , ctxTone :: Text
  , ctxDicePool :: DicePool
  , ctxPendingInterrupt :: Maybe ClockInterrupt
  , ctxSceneEntry :: Maybe SceneEntryContext  -- How they got here
  }
```

## Templates

Templates live in `templates/` and use Jinja2 syntax (Ginger library).

### Template Selection by Mood

```
MoodScene     → templates/scene/main.jinja      (The Weaver)
MoodAction    → templates/action/main.jinja     (The Crucible)
MoodAftermath → templates/aftermath/main.jinja  (The Echo)
MoodDowntime  → templates/downtime/main.jinja   (The Tide)
MoodTrauma    → templates/trauma/main.jinja     (The Crack)
MoodBargain   → templates/bargain/main.jinja    (The Hungry City)
```

### Template Structure

Each template defines:
1. **Voice/persona** - Who is speaking (The Weaver, The Crucible, etc.)
2. **What they see** - Context from `ctxLocation`, `ctxPresentNpcs`, etc.
3. **Rules/constraints** - Phase-specific behavior
4. **Output format** - What to include in response

### Key Template Variables

All templates receive the full `DMContext`. Common patterns:

```jinja
{% set stress = ctxPlayer.stress %}
{% set precarity = ctxPrecarity %}
{% set style = ctxSceneStyle %}

{% for npc in ctxPresentNpcs %}
  {{ npc.nwdNpc.npcName }} - {{ npc.nwdDispositionLabel }}
{% endfor %}

{% for clock in ctxVisibleClocks %}
  {{ clock.clockName }} [{{ clock.clockFilled }}/{{ clock.clockSegments }}]
{% endfor %}
```

## Output Types (Output.hs)

### TurnOutput

Main structured output from LLM:

```haskell
data TurnOutput = TurnOutput
  { narration :: Text                    -- Prose response
  , narrativeConnector :: Maybe NarrativeConnector  -- Therefore/But/Meanwhile
  , stressDelta :: Int                   -- -9 to +9
  , coinDelta :: Int
  , heatDelta :: Int                     -- 0 to +4
  , continueScene :: Bool                -- False → compression
  , costDescription :: Maybe Text        -- For consequence echoing
  , threatDescription :: Maybe Text      -- For unresolved threats
  , suggestedActions :: [Text]           -- 2-3 player options
  , traumaAssigned :: Maybe Text         -- If trauma turn
  , diceAction :: Maybe DiceAction       -- Required in Action mood
  , diceRecovered :: Int                 -- Downtime only
  , hookDescription :: Maybe Text        -- What pulls back from downtime
  , timeElapsed :: Maybe Text            -- "three days", etc.
  , clocksToTick :: [ClockTick]          -- Clocks to advance
  }
```

### CompressionOutput

Scene summarization:

```haskell
data CompressionOutput = CompressionOutput
  { summary :: Text              -- One paragraph
  , keyMoments :: [Text]         -- 3-5 significant beats
  , consequenceSeeds :: Text     -- Comma-separated future hooks
  , stressChange :: Int          -- Net scene delta
  , coinChange :: Int
  , newRumors :: [RumorInit]     -- Extracted gossip
  }
```

## World Entities

### Factions

```haskell
data Faction = Faction
  { factionName :: Text
  , factionDescription :: Text
  , factionGoal :: Maybe Text
  , factionResources :: [Text]
  , factionEnemies :: [FactionId]
  , factionAllies :: [FactionId]
  }

data FactionState = FactionState
  { fsFaction :: Faction
  , fsAttitude :: Attitude      -- Hostile → War → Cold → Neutral → Friendly → Allied
  , fsCurrentGoal :: Maybe Text
  }
```

### NPCs

```haskell
data NPC = NPC
  { npcName :: Text
  , npcRole :: Text
  , npcFaction :: Maybe FactionId
  , npcVoiceNotes :: Text       -- How to portray them
  , npcSecrets :: [Secret]
  , npcWants :: [Text]
  }

data NpcWithDisposition = NpcWithDisposition
  { nwdNpc :: NPC
  , nwdDisposition :: Disposition   -- Hostile → Suspicious → Neutral → Friendly → Loyal
  , nwdCurrentWant :: Maybe Text
  }
```

### Threads

```haskell
data Thread = Thread
  { threadId :: ThreadId
  , threadHook :: Text           -- What's interesting
  , threadInvolved :: [Either FactionId NpcId]
  , threadTension :: TensionLevel  -- Background → Simmering → Active → Critical
  , threadDeadline :: Maybe ClockId
  }
```

### Rumors

```haskell
data Rumor = Rumor
  { rumorId :: RumorId
  , rumorContent :: Text
  , rumorSource :: RumorSource    -- OverheardFrom NPC | PublicKnowledge
  , rumorTruthValue :: TruthValue -- TrueRumor | FalseRumor | Unknown
  , rumorSpread :: SpreadLevel    -- Whisper | Tavern | CommonKnowledge | Universal
  }
```

## File Map

| File | Purpose |
|------|---------|
| `State.hs` | All domain types (WorldState, PlayerState, Faction, NPC, Clock, etc.) |
| `Output.hs` | TurnOutput, CompressionOutput, DiceAction, apply functions |
| `Context.hs` | DMContext type, buildDMContext, precarity calculation |
| `Tools.hs` | Tool typeclass, 9 tool implementations, DMEvent type |
| `Templates.hs` | Template loading, schema definitions, mood→template mapping |
| `Loop.hs` | dmTurn, state transitions, clock checking, compression |
| `Effect.hs` | PlayingState effect for type-safe mood access |
| `Tarot.hs` | Scene/NPC seed generation via tarot metaphor |
| `CharacterCreation.hs` | Initial character generation flow |

## Common Patterns

### Adding a New Tool

1. Define input/output types in `Tools.hs`
2. Implement `Tool` instance with schema
3. Add to `allToolsList` for the relevant mood
4. Update template to describe when to use it

### Adding a New Event

1. Add constructor to `DMEvent` in `Tools.hs`
2. Emit it where state changes
3. Handle in GUI if needed (Events.hs)

### Adding a New Mood

1. Add constructor to `DMMood` in `State.hs`
2. Add variant type if needed
3. Create template in `templates/<mood>/main.jinja`
4. Add case to `renderForMood` in `Templates.hs`
5. Add transition logic in `Loop.hs`

### Debugging Tips

- Events log to debug panel in GUI
- `ctxSceneBeats` shows what happened this scene
- `ctxRecentCosts` / `ctxUnresolvedThreats` show echoing context
- Check `phase` field in WorldState for current mood
