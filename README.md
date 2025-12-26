# Tidepool Sketch

> Type-safe LLM agent loops with structured state and templates

## What Is This?

A sketch of Tidepool's core architecture: **effectful** for sandboxed effects, **mustache** templates validated at compile time, and **structured output** for LLM → state mutations.

The key insight: LLMs don't need raw IO access. They need:
1. **Typed state** they can read (via templates)
2. **Typed mutations** they can express (via structured output)
3. **Typed tools** for mid-turn capabilities

The Haskell code controls what's possible. The LLM controls what happens.

## Architecture

```
┌─────────────────────────────────────────────────────┐
│                    Game Loop                        │
│                                                     │
│  1. Build context (Haskell: state → DMContext)     │
│  2. Render template (mustache: context → prompt)    │
│  3. Call LLM (API: prompt + tools → response)      │
│  4. Parse output (JSON: response → TurnOutput)      │
│  5. Apply changes (Haskell: output → state')       │
│  6. Repeat                                          │
└─────────────────────────────────────────────────────┘
```

### Effects (what the LLM can do)

```haskell
type GameEffects s event =
  '[ State s          -- read/write game state
   , Random           -- dice rolls, weighted choices
   , LLM              -- template-based LLM calls
   , Emit event       -- observable events
   ]

-- Notably absent: IO, Network, FileSystem
-- The interpreter handles all real-world interaction
```

### Templates (what the LLM sees)

Mustache templates validated at compile time:

```haskell
mkTemplate ''DMContext "templates/dm_turn.mustache"
-- Compile error if template references nonexistent fields
```

### Structured Output (what the LLM can express)

Every mutation includes `because` for sleeptime learning:

```haskell
data ClockTick = ClockTick
  { tickClock :: ClockId
  , tickSegments :: Int
  , tickBecause :: Text  -- "Players ignored the warning"
  }
```

### Tools (mid-turn capabilities)

```haskell
data ThinkAsDM = ThinkAsDM  -- internal reasoning
data SpeakAsNPC = SpeakAsNPC  -- voice a character
data AskPlayer = AskPlayer  -- pause for input
data Choose = Choose  -- weighted random
```

## The DM Example

A Blades in the Dark-style dungeon master:

- **Factions** with attitudes, goals, resources
- **NPCs** with dispositions, wants, fears, voice notes
- **Clocks** that tick toward consequences
- **Threads** (dangling plot hooks) with tension levels
- **Rumors** that spread and may be true/false
- **Scenes** with beats that compress into state changes

### State

```haskell
data WorldState = WorldState
  { factions :: HashMap FactionId Faction
  , clocks :: HashMap ClockId Clock
  , npcs :: HashMap NpcId Npc
  , locations :: HashMap LocationId Location
  , rumors :: [Rumor]
  , threads :: [Thread]
  , scene :: Maybe ActiveScene
  , tone :: Tone
  }
```

### Turn Output

```haskell
data TurnOutput = TurnOutput
  { narration :: Text
  , clockTicks :: [ClockTick]
  , newClocks :: [NewClock]
  , attitudeShifts :: [AttitudeShift]
  , dispositionShifts :: [DispositionShift]
  , newThreads :: [NewThread]
  , spreadRumors :: [NewRumor]
  , sceneControl :: SceneControl
  }
```

### Compression

When scenes get long, they compress into durable state:

```haskell
data CompressionOutput = CompressionOutput
  { sceneOutcome :: SceneOutcome      -- summary, key beats
  , worldDeltas :: WorldDeltas        -- faction/npc changes
  , extracted :: Extractions          -- threads, rumors, promises
  , decay :: Decay                    -- what fades
  }
```

## Why This Shape?

### For the LLM
- Clear vocabulary of what it can express
- Typed mutations prevent impossible states
- `because` fields encourage reasoning

### For Sleeptime
- Observable events show how LLM thinks
- `because` fields are training data
- Can evolve: add fields to output, add state, add tools

### For Humans
- All state in Haskell, reviewable
- Templates are just mustache, editable
- Structured output is JSON schema, inspectable

## Building

```bash
cabal build
cabal run tidepool-dm
```

## Project Structure

```
src/
├── Tidepool/           # Core library
│   ├── Effect.hs       # Effect types (State, Random, LLM, Emit)
│   ├── Template.hs     # Template type + TH validation
│   ├── Tool.hs         # Tool typeclass
│   ├── Schema.hs       # JSON schema derivation
│   └── Loop.hs         # Generic game loop
├── DM/                 # DM example
│   ├── State.hs        # World state types
│   ├── Output.hs       # Turn + compression output
│   ├── Tools.hs        # DM-specific tools
│   ├── Context.hs      # State → template context
│   ├── Templates.hs    # Template definitions
│   └── Loop.hs         # DM game loop
templates/
├── dm_turn.mustache    # Main DM prompt
├── compression.mustache # Scene compression prompt
└── partials/           # Reusable template blocks
app/
└── Main.hs             # Example setup
```

## Next Steps

- [ ] Implement TH template validation (parse mustache, check paths)
- [ ] Wire up actual LLM calls (Anthropic API)
- [ ] Build interpreter for effects
- [ ] Add player I/O layer
- [ ] Sleeptime agent that evolves templates/state/output

## See Also

- [effectful](https://hackage.haskell.org/package/effectful) - Effect system
- [stache](https://hackage.haskell.org/package/stache) - Mustache templates
- Blades in the Dark - Inspiration for faction/clock mechanics
