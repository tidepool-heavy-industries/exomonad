# Tidepool DM Agent - Haskell Sketch

## What This Is

A type-safe LLM agent loop library (Tidepool) demonstrated via a Blades in the Dark-style Dungeon Master agent. The key insight: LLMs don't need raw IO - they need typed state they can read (via templates) and typed mutations they can express (via structured output).

**Status**: Compiles successfully. All logic is stubbed with `error "TODO: ..."`.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Game Loop (dmTurn)                     │
│                                                             │
│  1. Record player action as scene beat                      │
│  2. Build context (Haskell: WorldState → DMContext)         │
│  3. Render template (Jinja: DMContext → prompt)             │
│  4. Call LLM (runTurn: prompt + schema + tools → result)    │
│  5. Apply structured output (TurnOutput → WorldState')      │
│  6. Handle dice mechanics (RequestInput for player choice)  │
│  7. Check clock consequences                                │
│  8. Compress if needed                                      │
│  9. Return narrative response                               │
└─────────────────────────────────────────────────────────────┘
```

### Core Design Decisions

1. **effectful** for effects
   - MTL-equivalent performance, no INLINE pragma dance
   - Effects: `LLM`, `RequestInput`, `State`, `Emit`, `Random`

2. **Typed Jinja templates** (WIP - external package)
   - Compile-time validation against Haskell types
   - `$(typedTemplateFile ''DMContext "templates/dm_turn.jinja")`
   - LLMs know Jinja from training data

3. **Type-safe tool lists**
   - `ToolList` GADT: `TCons (Proxy @ThinkAsDM) $ TCons (Proxy @SpeakAsNPC) $ TNil`
   - Tools have access to: `State`, `Emit`, `RequestInput`, `Random`

4. **Delta fields for mutations**
   - LLM outputs `+2 stress`, not `stress = 5`
   - `because` fields on every mutation for training data

5. **FitD dice mechanics**
   - Player sees full dice pool, chooses which die to use
   - Position (Controlled/Risky/Desperate) + Effect (Limited/Standard/Great)
   - Precarity system scales narrative tone

## Effect System

```haskell
-- The effect stack for game loops
type GameEffects s event =
  '[ LLM
   , RequestInput
   , State s
   , Emit event
   , Random
   ]

-- LLM effect - interpreter handles API, tools, retries
data LLM :: Effect where
  RunTurnOp :: Text -> Value -> [Value] -> LLM m (TurnResult Value)

-- RequestInput - for tools/game logic needing player input
data RequestInput :: Effect where
  RequestChoice :: Text -> [(Text, a)] -> RequestInput m a
  RequestText :: Text -> RequestInput m Text
```

## Project Structure

```
src/
├── Tidepool/              # Core library (reusable)
│   ├── Effect.hs          # LLM, RequestInput, State, Emit, Random effects
│   ├── Template.hs        # TypedTemplate, Template (jinja + schema + tools)
│   ├── Tool.hs            # Tool typeclass, ToolList GADT
│   ├── Schema.hs          # JSON Schema derivation (deriveSchema)
│   └── Loop.hs            # TurnConfig, Compression abstractions
└── DM/                    # DM agent (domain-specific)
    ├── State.hs           # WorldState, PlayerState, dice mechanics, factions...
    ├── Output.hs          # TurnOutput with delta fields, applyTurnOutput
    ├── Tools.hs           # ThinkAsDM, SpeakAsNPC, AskPlayer, Choose
    ├── Context.hs         # DMContext, DiceContext, Precarity
    ├── Templates.hs       # dmTurnTemplate, compressionTemplate
    └── Loop.hs            # dmTurn, handleDiceSelection, runDMGame
app/
└── Main.hs                # Executable entry point
templates/                 # Jinja templates (to be created)
├── dm_turn.jinja
└── compression.jinja
```

## Key Types

### Dice Mechanics (FitD-inspired)
```haskell
data Position = Controlled | Risky | Desperate
data Effect = Limited | Standard | Great
data OutcomeTier = Critical | Success | Partial | Bad | Disaster

data PendingOutcome = PendingOutcome
  { outcomeContext :: Text
  , outcomePosition :: Position
  , outcomeEffect :: Effect
  , outcomeStakes :: Text
  , chosenDie :: Maybe Int
  , chosenTier :: Maybe OutcomeTier
  }
```

### Precarity (narrative tone scaling)
```haskell
data Precarity
  = OperatingFromStrength  -- < 5: expansive but careless
  | RoomToManeuver         -- 5-9: noir cool
  | WallsClosingIn         -- 10-14: tense
  | HangingByThread        -- >= 15: urgent

-- precarity = stress + heat + wanted*2 + (hunted?3) + (recovering?2)
```

### Delta Fields
```haskell
data PlayerDeltas = PlayerDeltas
  { stressDelta :: Int      -- +2, not "set to 5"
  , coinDelta :: Int
  , heatDelta :: Int
  , wantedDelta :: Int
  , deltaBecause :: Text    -- "caught by guard" - training data
  }
```

## Implementation Status

### Complete
- Effect system with all core effects defined
- Type-safe tool list (ToolList GADT)
- Template system ready for typed Jinja integration
- FitD dice mechanics types
- Precarity calculation
- Delta-based mutation types
- DM loop structure with RequestInput for dice selection

### Stubbed (TODOs)
- All effect interpreters (`runState`, `runLLM`, `runRequestInput`, etc.)
- Template rendering (waiting for jinja-th package)
- `applyTurnOutput` and mutation appliers
- `buildDMContext` and context enrichment
- JSON Schema derivation

## Running

```bash
cabal build              # Succeeds
cabal run tidepool-dm    # Runs but hits TODO errors
```

## Next Steps

1. **jinja-th integration** - Replace placeholder TypedTemplate with real TH splice
2. **Effect interpreters** - Implement `runState`, `runRandom`, `runEmit`
3. **Schema derivation** - `deriveSchema` via GHC.Generics
4. **Apply functions** - Wire up `applyTurnOutput` with optics
5. **LLM interpreter** - Anthropic API with tool dispatch loop

## What Sleeptime Would Evolve

The "sleeptime" learning agent reads run logs and evolves:

1. **State fields** - Add new fields to WorldState
2. **Output schema** - New mutation types in TurnOutput
3. **Apply logic** - How mutations affect state
4. **Templates** - What context surfaces to LLM
5. **Tool behavior** - How tools execute

The loop structure, effect types, and infrastructure stay stable.

## References

- effectful: https://hackage.haskell.org/package/effectful
- Anthropic tool use: https://docs.anthropic.com/en/docs/tool-use
- Blades in the Dark SRD: https://bladesinthedark.com/
- heist-engine (v1 reference): ~/dev/shoal-automat/machines/heist-engine
