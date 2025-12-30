# DM Agent - Exhaustive System Documentation

This is a **Blades in the Dark-style Dungeon Master agent** - an LLM that runs a noir heist RPG. The player is a scoundrel in a haunted industrial city. They accumulate stress, heat, and coin while dodging consequences via dice.

## What is Blades in the Dark?

A tabletop RPG where:
- **Stress** (0-9) is your buffer against consequences. Hit 9 = trauma (permanent scar)
- **Heat** (0-10) measures attention from authorities. Too high = wanted levels
- **Dice pools** are spent on actions. You see your dice, choose which to use
- **Position** (Controlled/Risky/Desperate) determines consequence severity
- **Clocks** are progress trackers that tick toward inevitable events

The DM doesn't punish through narration - **all consequences flow through dice rolls**.

---

## Core Architectural Insight

LLMs don't need raw IO. They need:
1. **Typed state they can read** (via Jinja templates → system prompt)
2. **Typed mutations they can express** (via structured JSON output)
3. **Tools for state changes** (that emit events and modify state)

```
WorldState → buildDMContext → render template → system prompt
                                                      ↓
                                              LLM call with:
                                                - system prompt
                                                - user input
                                                - tool definitions
                                                - output JSON schema
                                                      ↓
                                              Parse TurnOutput + tool calls
                                                      ↓
                                              Apply mutations to WorldState
```

The LLM sees rich context (NPCs, clocks, stress levels, narrative tone) and outputs deltas (`+2 stress`, not `stress = 5`), ensuring it can't corrupt state.

---

## How A Turn Actually Works

```haskell
dmTurn :: PlayerInput -> Eff es Response
```

1. **Record input as scene beat** - Append to `sceneBeats` for history
2. **Check pending interrupts** - Clock may have forced an action
3. **Extract scene + mood from GamePhase** - Must be `PhasePlaying`
4. **Build DMContext** - Gather all context from WorldState
5. **Render template by mood** - `MoodScene` → `scene/main.jinja`, etc.
6. **Call LLM** with:
   - System prompt (rendered template)
   - User message (player's action text)
   - Tool definitions (vary by mood)
   - Output schema (TurnOutput as JSON schema)
7. **Dispatch tool calls** - Tools modify state, may trigger mood transitions
8. **Parse structured output** - TurnOutput with narration + deltas
9. **Apply deltas** - stress/heat/coin changes via `applyTurnOutput`
10. **Handle dice if action mode** - Player picks die, costs applied
11. **Check clock completions** - Fire consequences
12. **Check trauma trigger** - stress = 9 → MoodTrauma
13. **Compress if needed** - 20+ beats → summarize scene
14. **Return response** - Narrative + state changes for GUI

---

## GamePhase vs DMMood

**GamePhase** is the *outer* state machine (lifecycle phases):

```haskell
data GamePhase
  = PhaseCharacterCreation           -- Initial setup
  | PhaseScenarioInit CharacterChoices  -- Has character, generating scenario
  | PhasePlaying ActiveScene DMMood  -- Active gameplay (scene + current mood)
  | PhaseBetweenScenes BetweenScenesContext  -- Choosing next activity
  | PhaseSessionEnded                -- Player quit
```

**DMMood** is the *inner* state machine (during `PhasePlaying`):

```haskell
data DMMood
  = MoodScene SceneVariant      -- Exploration, dialogue, discovery
  | MoodAction ActionVariant    -- Dice about to be spent
  | MoodAftermath AftermathVariant  -- Consequences landing
  | MoodTrauma TraumaVariant    -- Breaking point (stress = 9)
  | MoodBargain BargainVariant  -- No dice left, must bargain
```

Moods transition via tool calls:
- `engage` → MoodAction
- `spend_die` + `resolve` → MoodAftermath
- stress = 9 → MoodTrauma
- dice pool empty → MoodBargain

---

## Template Personas (The DM's Voices)

Each mood has a distinct narrative voice speaking to the LLM:

| Mood | Template | Persona | Role |
|------|----------|---------|------|
| Scene | `scene/main.jinja` | **The Weaver** | Shows possibilities, offers doors, never punishes |
| Action | `action/main.jinja` | **The Precipice** | The edge of the moment, presents dice choices |
| Aftermath | `aftermath/main.jinja` | **The Echo** | What comes after, consequences as physics |
| Trauma | `trauma/main.jinja` | **The Crack** | Breaking point, assigns permanent scar |
| Bargain | `bargain/main.jinja` | **The Hungry City** | Offers dark deals when dice run out |

Each persona has distinct prose style and constraints. The Weaver never adds stress/heat directly - only through `engage` tool → dice → outcomes.

---

## The Precommitted Outcome Pattern (Novel Dice Mechanic)

In Action mood, the LLM **precommits outcomes for each die** before the player chooses:

```haskell
data DiceAction = DiceAction
  { situation :: Text        -- "Dodging the Bluecoat's blade"
  , position :: Position     -- Risky
  , outcomes :: [DieOutcome] -- One per die in pool
  }

data DieOutcome = DieOutcome
  { dieValue :: Int     -- Must match a die in pool
  , hint :: Text        -- "Clean escape" or "Barely..." (shown to player)
  , stressCost :: Int   -- Applied AFTER choice
  , heatCost :: Int
  , coinDelta :: Int
  , narrative :: Text   -- Revealed AFTER choice
  }
```

**What the player sees:**
```
Pool: [4] [2] [6]

[4] +1 stress, +0 heat — "Scrape through, bruised"
[2] +2 stress, +1 heat — "Barely, and they saw your face"
[6] +0 stress, +0 heat — "Clean break"
```

**Key insight:** The LLM writes outcomes for ALL dice. The player chooses which future to spend. High dice = good outcomes but cost opportunity. Low dice = bad outcomes but save high dice for later. This is the core tension.

---

## Effect Stack

The game loop uses `effectful` with these effects:

```haskell
dmTurn
  :: ( State WorldState :> es   -- Read/write game state
     , Random :> es              -- Dice rolling
     , LLM :> es                 -- Call Claude with tools
     , Emit DMEvent :> es        -- Emit events for GUI
     , RequestInput :> es        -- Ask player for input
     , Log :> es                 -- Debug logging
     )
  => PlayerInput
  -> Eff es Response
```

### PlayingState Effect

Type-safe access to scene/mood *only* during `PhasePlaying`:

```haskell
data PlayingState :: Effect where
  GetScene :: PlayingState m ActiveScene
  GetMood :: PlayingState m DMMood
  PutScene :: ActiveScene -> PlayingState m ()
  PutMood :: DMMood -> PlayingState m ()
```

Tools that need scene/mood access require `PlayingState :> es` in their type signature. The interpreter is only provided after pattern matching on `PhasePlaying`, making it impossible to access scene/mood in other phases.

---

## WorldState Structure

```haskell
data WorldState = WorldState
  { phase :: GamePhase              -- Current lifecycle phase
  , player :: PlayerState           -- Stress/coin/heat/wanted/trauma
  , factions :: HashMap FactionId Faction
  , clocks :: HashMap ClockId Clock
  , locations :: HashMap LocationId Location
  , npcs :: HashMap NpcId Npc
  , rumors :: [Rumor]
  , threads :: [Thread]
  , tone :: Tone
  , sessionHistory :: Seq SceneSummary  -- Compressed past scenes
  , sessionGoals :: [Text]
  , dicePool :: DicePool
  , pendingOutcome :: Maybe PendingOutcome
  -- Consequence echoing
  , recentCosts :: [Text]           -- Last 3 costly outcomes (narrative continuity)
  , unresolvedThreats :: [Text]     -- Complications not yet addressed
  , pendingInterrupt :: Maybe ClockInterrupt  -- Clock forcing action
  , sceneEntryContext :: Maybe SceneEntryContext  -- What led to this scene
  , suggestedActions :: [Text]      -- LLM's suggested next actions
  }
```

---

## Scene Beats & Compression

**Beats** are recorded during a scene:

```haskell
data Beat
  = PlayerAction Text [Tag]       -- What player did
  | DMNarration Text             -- DM's response
  | NPCSpeech NpcId Text Text    -- NPC dialogue
  | DiceRoll Int OutcomeTier     -- Die spent and result
  | ClockTick ClockId Int        -- Clock advanced
  | ...
```

**Compression** triggers at 20+ beats:
1. All beats sent to LLM with `compression.jinja` template
2. LLM outputs `CompressionOutput` (summary, key moments, rumors extracted)
3. Scene beats cleared, summary added to `sessionHistory`
4. New rumors appended to world state

This keeps context window manageable while preserving narrative continuity.

---

## Consequence Echoing

Templates surface recent consequences for narrative callbacks:

- **`ctxRecentCosts`** - Last 3 costly/setback outcomes (e.g., "bruised ribs from the fight", "owe Lyra a favor")
- **`ctxUnresolvedThreats`** - Complications not yet addressed (e.g., "the Spirit Wardens are searching for you")

The Echo (aftermath template) writes these. The Weaver (scene template) weaves them back in. This creates narrative continuity without explicit memory.

---

## Tools (9 total)

| Tool | Used In | Effect |
|------|---------|--------|
| `think` | All | DM internal reasoning (not shown to player) |
| `speak_as_npc` | Scene, Action | NPC dialogue with voice notes |
| `ask_player` | Scene | Request clarification |
| `choose` | Scene, Aftermath | Present 2-4 options |
| `engage` | Scene | Start dice action → **transitions to MoodAction** |
| `spend_die` | Action | Player picks die from pool |
| `resolve` | Action | Conclude action → **transitions to MoodAftermath** |
| `accept` | Bargain | Accept bargain terms |
| `set_scene_style` | Scene | Modify atmosphere/pressure/class |

**Transition tools** restart the turn loop with the new mood.

---

## Events

Events flow from game loop → GUI via `Emit DMEvent`:

```haskell
data DMEvent
  = RandomChoice Text Int              -- Random roll result
  | DieSpent Int OutcomeTier Text      -- Die used, outcome, context
  | ClockCompleted Text Text Text         -- clockId, clockName, consequence narrative
  | SceneCompressed Text               -- Summary when compressed
  | MoodTransition Text Text Text      -- Tool, fromMood, toMood
  | StressChanged Int Int Text         -- old, new, reason
  | TraumaTriggered Trauma Text Text   -- trauma, trigger, breaking point
  | HeatChanged Int Int Text
  | WantedChanged Int Int Text
  | CoinChanged Int Int Text
  | DicePoolDepleted Text              -- Out of dice
  | BargainOffered Text Bool           -- Context, canRetreat
  | NarrativeAdded Text                -- Narrative for GUI log
```

**NarrativeAdded** is the primary channel for DM prose flowing to the GUI.

---

## GUI Bridge Pattern

The game loop (effectful) and GUI (threepenny) run in same process, communicating via STM:

```haskell
data GUIBridge state = GUIBridge
  { gbState :: TVar state           -- WorldState for display
  , gbStateVersion :: TVar Int      -- Change detection
  , gbPendingRequest :: TVar (Maybe PendingRequest)
  , gbRequestResponse :: MVar RequestResponse  -- Blocks game loop
  , gbNarrativeLog :: TVar (Seq Text)
  , gbDebugLog :: TVar (Seq DebugEntry)
  , gbLLMActive :: TVar Bool        -- Loading spinner
  }
```

**Flow:**
1. Game loop posts request to `gbPendingRequest`
2. Game loop blocks on `takeMVar gbRequestResponse`
3. GUI polls every 100ms, sees request, renders widget
4. User interacts, GUI puts response via `tryPutMVar`
5. Game loop unblocks, continues

---

## Clocks & Consequences

```haskell
data Clock = Clock
  { clockName :: Text
  , clockSegments :: Int      -- 4, 6, or 8
  , clockFilled :: Int
  , clockVisible :: Bool      -- Shown to player?
  , clockType :: ClockType
  , clockConsequence :: Text  -- Narrative describing what happens when filled
  }
```

Clock consequences are narrative text (2-3 sentences) describing what happens when the clock fills. When a clock completes, `checkClockConsequences` emits a `ClockCompleted` event with the narrative. The LLM interprets this at runtime and decides appropriate mechanical effects (stress, heat, coin, etc.).

---

## File Map

| File | Purpose |
|------|---------|
| `State.hs` | All domain types (WorldState, GamePhase, DMMood, Faction, NPC, Clock, etc.) |
| `Output.hs` | TurnOutput, CompressionOutput, DiceAction, apply functions |
| `Context.hs` | DMContext type, buildDMContext, Precarity calculation |
| `Tools.hs` | Tool typeclass, 9 tool implementations, DMEvent type |
| `Templates.hs` | Template loading, schema definitions, mood→template mapping |
| `Loop.hs` | dmTurn, state transitions, clock checking, compression |
| `Effect.hs` | PlayingState effect for type-safe mood/scene access |
| `Tarot.hs` | Scene/NPC seed generation via tarot metaphor |
| `CharacterCreation.hs` | Initial character generation flow |

---

## Known Gaps / TODOs

### BetweenScenes Handler
`PhaseBetweenScenes` exists but **no handler is implemented**. The types exist:

```haskell
data BetweenScenesOption
  = BSLayLow        -- Reduce heat
  | BSRecover       -- Restore dice
  | BSWorkGoal      -- Advance goal clock
  | BSNewScene      -- Start new scene
  | BSEndSession    -- Quit
```

But `handleBetweenScenes` doesn't exist. Retreat tool transitions to BetweenScenes but nothing happens after.

### continueScene Flag
`TurnOutput.continueScene = False` is parsed but **never checked**. Scenes only end via compression (20+ beats), not LLM signal.

### Clock Consequences
Clock consequences are now narrative text. When a clock fills, the LLM receives the narrative description and decides appropriate mechanical effects at runtime. The `Consequence` sum type in State.hs is dead code (kept for potential future use).

---

## Adding a New Mood

1. Add constructor to `DMMood` in `State.hs`
2. Add variant type if needed
3. Create template in `templates/<mood>/main.jinja`
4. Add case to `renderForMood` in `Templates.hs`
5. Add transition logic in `Loop.hs`

## Adding a New Tool

1. Define input/output types in `Tools.hs`
2. Implement `Tool` instance with schema
3. Add to `dmToolList` for relevant mood(s)
4. Update template to describe when to use it

---

## Removed Dead Code (2025-12-29)

### MoodDowntime (~450 lines)
- Full downtime phase that was never triggered
- `templates/downtime/` deleted
- Related state machine transitions removed

### ClockAdvanced Event
- Never emitted; handlers removed from Events.hs and Main.hs

### clocksToTick (TurnOutput)
- `clocksToTick :: [ClockTick]` parsed but never processed
- Clocks now only tick via completion consequences

### ctxPendingInterrupt (DMContext)
- Built but never used in templates
- Note: `pendingInterrupt` in WorldState IS still used in Loop.hs

---

## Verified In Use (NOT Dead)

**NarrativeAdded event** - Emitted by Loop.hs whenever narrative text should appear in the GUI log. This is the primary channel for DM prose, NPC dialogue, and outcome descriptions to flow from the game loop to the narrative pane.

**ctxHiddenClocks** - Surfaced to templates so the DM knows about clocks the player can't see. Used in `dm_turn.jinja` to let the DM reference hidden threats in narration without revealing clock state. Example: a hidden "Guard Patrol" clock at 5/6 lets the DM write tension about footsteps approaching.
