# Tidepool DM Agent - Haskell Sketch

## What This Is

A type-safe LLM agent loop for running a Blades in the Dark-style Dungeon Master. The key insight: LLMs don't need raw IO - they need typed state they can read (via templates) and typed mutations they can express (via structured output).

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

### Key Design Decisions

1. **effectful** for effects, not polysemy or MTL
   - MTL-equivalent performance
   - No INLINE pragma dance
   - Good ecosystem integration

2. **Templates as values**, not typeclasses
   - First-class, composable
   - No orphan instances
   - Like Servant or lens

3. **Structured output with `because` fields**
   - Every mutation explains itself
   - Free training data for sleeptime
   - Typed sum types, not prose

4. **Sandboxed effects**
   - LLM code gets: State, Random, LLM, Emit
   - No: IO, Network, FileSystem
   - Interpreter handles reality

## Project Structure

```
src/
├── Tidepool/           # Core library (reusable)
│   ├── Effect.hs       # Effect types + interpreters
│   ├── Template.hs     # Template type + TH validation
│   ├── Tool.hs         # Tool typeclass
│   ├── Schema.hs       # JSON Schema derivation
│   └── Loop.hs         # Generic turn/compression loop
└── DM/                 # DM agent (domain-specific)
    ├── State.hs        # WorldState, Factions, NPCs, Clocks...
    ├── Output.hs       # TurnOutput, CompressionOutput, apply fns
    ├── Tools.hs        # ThinkAsDM, SpeakAsNPC, AskPlayer, Choose
    ├── Context.hs      # State → DMContext for templates
    ├── Templates.hs    # Template values, schemas, render fns
    └── Loop.hs         # DM game loop, scene management
templates/
├── dm_turn.mustache    # Main DM prompt template
├── compression.mustache # Scene collapse template
└── partials/           # Reusable template blocks
```

## Implementation Status

All files have type signatures and `error "TODO: ..."` stubs. Nothing compiles yet.

### Priority Order

1. **Get it building** - fix imports, add missing instances
2. **Effect interpreters** - `runState`, `runRandom`, `runEmit`
3. **Template rendering** - manual mustache first, TH later
4. **LLM integration** - Anthropic API with structured output
5. **Apply functions** - `applyTurnOutput`, `applyCompression`
6. **Main loop** - `dmTurn`, `runDMGame`

### Key TODOs by File

**Tidepool/Effect.hs**
- `runState` - use Effectful.State.Static.Local
- `runRandom` - use System.Random.Stateful
- `runLLM` - call Anthropic API, parse structured output
- `runEmit` - call handler in IO
- `runGame` - compose all interpreters

**Tidepool/Template.hs**
- `mkTemplate` TH - parse mustache, validate fields against type
- Start with manual rendering, add TH validation later

**Tidepool/Schema.hs**
- `deriveSchema` - use GHC.Generics to build JSON Schema
- `schemaToValue` - convert to Aeson Value

**DM/Output.hs**
- `applyTurnOutput` - compose all mutation appliers
- Each applier uses optics to modify WorldState

**DM/Context.hs**
- `buildDMContext` - extract scene, enrich NPCs, filter relevant data
- `enrichNpc` - lookup, render disposition, find urgent want

**DM/Templates.hs**
- `renderDMContext` - produce the prompt text
- Start with string concatenation, can use stache later

**DM/Loop.hs**
- `dmTurn` - the main loop body
- `checkClockConsequences` - find completed, execute, remove
- `runDMGame` - wire up effects, handle I/O

## Dependencies

```yaml
# Key dependencies in cabal file
effectful-core    # Effect system
effectful-th      # TH for effects
stache            # Mustache templates
aeson             # JSON
optics            # Lenses/prisms
hashable          # For HashMap keys
```

## Running

```bash
cabal build        # Will fail until TODOs filled
cabal run tidepool-dm
```

## Testing Strategy

1. **Unit tests** for apply functions (pure)
2. **Golden tests** for template rendering
3. **Mock LLM** for loop testing (return canned TurnOutput)
4. **Integration** with real API last

## API Integration Notes

Anthropic structured output:
```json
{
  "model": "claude-sonnet-4-20250514",
  "messages": [...],
  "tools": [...],  // ThinkAsDM, SpeakAsNPC, etc
  "tool_choice": {"type": "auto"},
  "response_format": {
    "type": "json_schema",
    "json_schema": { /* TurnOutput schema */ }
  }
}
```

Tool calls come back as `tool_use` blocks, need to execute and continue.
Final response includes structured output matching schema.

## What Sleeptime Would Evolve

1. **State fields** - add new fields to WorldState
2. **Output schema** - add new mutation types to TurnOutput
3. **Apply logic** - modify how mutations affect state
4. **Templates** - what context surfaces to LLM
5. **Tool implementations** - how tools behave

The loop itself, effect types, and infrastructure stay stable.

## References

- effectful docs: https://hackage.haskell.org/package/effectful
- stache (mustache): https://hackage.haskell.org/package/stache
- Anthropic tool use: https://docs.anthropic.com/en/docs/tool-use
- Blades in the Dark SRD: https://bladesinthedark.com/
