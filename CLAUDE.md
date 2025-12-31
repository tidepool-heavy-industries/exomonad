# Tidepool - Type-Safe LLM Agent Framework

## What This Is

A type-safe LLM agent loop library (Tidepool) with two demonstration agents:
1. **DM Agent** - Blades in the Dark-style Dungeon Master
2. **Tidying Agent** - Prosthetic executive function for tackling overwhelming spaces

The key insight: LLMs don't need raw IO - they need typed state they can read (via templates) and typed mutations they can express (via structured output).

**Status**: Compiles successfully. DM agent has stubbed logic. Tidying agent is functional with GUI.

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
   - Effects: `LLM`, `RequestInput`, `State`, `Emit`, `Random`, `Log`, `ChatHistory`, `Time`

2. **Typed Jinja templates** (via ginger library)
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

## GUI Architecture

The GUI uses threepenny-gui with a polling-based update model:

```
┌─────────────────────────────────────────────────────────────┐
│                      Single Process                          │
│                                                              │
│   ┌──────────────────┐         ┌──────────────────────────┐ │
│   │   Game Loop      │◄──MVar──│   Threepenny GUI         │ │
│   │   (effectful)    │         │   (browser)              │ │
│   │                  │         │                          │ │
│   │ RequestInput ────┼──TVar──►│ Polls for PendingRequest │ │
│   │ blocks on MVar   │         │ User clicks → MVar put   │ │
│   │                  │         │                          │ │
│   │ WorldState ──────┼──TVar──►│ Updates stats/narrative  │ │
│   │ Emit events ─────┼──TVar──►│ Debug log / UI updates   │ │
│   └──────────────────┘         └──────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

### Key GUI Types (Tidepool.GUI.Core)

```haskell
-- | Bridge between game loop and GUI (parameterized by state type)
data GUIBridge state = GUIBridge
  { gbState            :: TVar state           -- Game state for display
  , gbStateVersion     :: TVar Int             -- Version for change detection
  , gbPendingRequest   :: TVar (Maybe PendingRequest)
  , gbRequestResponse  :: MVar RequestResponse -- Blocks game loop
  , gbNarrativeLog     :: TVar (Seq Text)
  , gbNarrativeVersion :: TVar Int
  , gbDebugLog         :: TVar (Seq DebugEntry)
  , gbDebugVersion     :: TVar Int
  , gbLLMActive        :: TVar Bool            -- Loading spinner
  }

-- | Input requests posted by game loop
data PendingRequest
  = PendingChoice Text [(Text, Int)]  -- Choice cards
  | PendingText Text                  -- Free-form text
  | PendingDice Text [(Int, Int)]     -- Dice with (value, index)

-- | Responses from GUI
data RequestResponse
  = ChoiceResponse Int
  | TextResponse Text
```

### Communication Flow

1. **Game loop posts request**: `writeTVar gbPendingRequest (Just request)`
2. **Game loop blocks**: `takeMVar gbRequestResponse`
3. **GUI polls**: Timer reads `gbPendingRequest` every 100ms
4. **GUI renders**: Shows input widget (text field, cards, or dice)
5. **User interacts**: Click/enter triggers `tryPutMVar gbRequestResponse`
6. **Game loop unblocks**: Continues with response value

### Widget Update Pattern

All DM widgets follow a consistent create/update pattern:

```haskell
-- Create widget, returns element reference
clocksPanel :: GUIBridge WorldState -> UI Element
clocksPanel bridge = do
  panel <- UI.div #. "clocks-panel"
  updateClocksPanel panel bridge
  pure panel

-- Update existing element (for polling loop)
updateClocksPanel :: Element -> GUIBridge WorldState -> UI ()
updateClocksPanel panel bridge = do
  void $ element panel # set children []
  -- Rebuild contents from state...
```

### Polling and Version Detection

The polling loop uses version numbers for efficient change detection:

```haskell
-- In setupPolling (DM.GUI.App)
on UI.tick timer $ const $ do
  stateVersion <- liftIO $ atomically $ readTVar bridge.gbStateVersion
  prevStateVersion <- liftIO $ readIORef prevStateVersionRef

  when (stateVersion /= prevStateVersion) $ do
    liftIO $ writeIORef prevStateVersionRef stateVersion
    refreshStats bridge elements
    refreshMood bridge elements
    refreshClocks bridge elements
```

### Tab Switching (Show/Hide Pattern)

Tabs use show/hide rather than recreate to preserve element references:

```haskell
switchToGameTab :: GUIElements -> UI ()
switchToGameTab elements = do
  void $ element (geGameTab elements) #. "tab active"
  void $ element (geHistoryTab elements) #. "tab"
  -- Show/hide, don't recreate
  void $ element (geNarrativePane elements) # set style [("display", "block")]
  void $ element (geHistoryPane elements) # set style [("display", "none")]
```

### Accessibility

Interactive elements include full keyboard support:
- **Choice cards**: Tab navigation, Enter/Space to select, 1-9 shortcuts
- **Dice cards**: Same keyboard nav plus ARIA labels with tier info
- **Focus states**: Visual outlines on focus, active transforms

```haskell
-- Dice cards include ARIA labels
card <- UI.div #. "dice-card"
  # set (attr "tabindex") "0"
  # set (attr "role") "option"
  # set (attr "aria-label") (T.unpack $ tierLabel tier <> " - die showing " <> T.pack (show dieValue))
```

### Theme System

Themes define colors as CSS custom properties:

```haskell
-- Tidepool.GUI.Theme
data Theme = Theme
  { themeName    :: Text
  , themeColors  :: ColorPalette
  , themeFontMain :: Text
  , themeFontMono :: Text
  }

-- DM.GUI.Theme (noir aesthetic)
noirTheme = Theme
  { themeName = "noir"
  , themeColors = noirPalette  -- Deep charcoal, muted gold
  , ...
  }

-- Tier colors for dice
tierColor TierCritical = "#c9a227"  -- Gold
tierColor TierSuccess  = "#4a7c4a"  -- Muted green
tierColor TierPartial  = "#8b7420"  -- Amber
tierColor TierBad      = "#7c4a4a"  -- Muted red
tierColor TierDisaster = "#4a0000"  -- Deep red
```

## Effect System

### IO-Blind Architecture

Agents are **IO-blind**: they cannot use `IOE` directly. All IO happens in the runner via effect interpreters. This enables:
- Eventual WASM compilation
- Deterministic testing
- Clear separation of pure logic from IO

```haskell
-- BaseEffects: What agents see (no IOE!)
type BaseEffects s evt =
  '[ LLM, State s, Emit evt, RequestInput, Log, ChatHistory, Random, Time ]

-- RunnerEffects: What interpreters use (includes IOE)
type RunnerEffects s event =
  '[ LLM, RequestInput, Log, ChatHistory, State s, Emit event, Random, Time, IOE ]
```

### Core Effects

```haskell
-- LLM effect - interpreter handles API, tools, retries
data LLM :: Effect where
  RunTurnOp :: Text -> [ContentBlock] -> Value -> [Value] -> LLM m (TurnOutcome (TurnResult Value))

-- RequestInput - for tools/game logic needing player input
data RequestInput :: Effect where
  RequestChoice :: Text -> [(Text, a)] -> RequestInput m a
  RequestText :: Text -> RequestInput m Text
  RequestTextWithPhoto :: Text -> RequestInput m (Text, [(Text, Text)])  -- For photo attachments

-- Time effect - IO-blind time access
data Time :: Effect where
  GetCurrentTime :: Time m UTCTime

-- Interpreted by runner:
runTime :: IOE :> es => Eff (Time : es) a -> Eff es a
```

### QuestionUI Effect (Tidying-specific)

For mid-turn user interaction during tool execution:

```haskell
data QuestionUI :: Effect where
  AskQuestion :: Question -> QuestionUI m Answer

type QuestionHandler = Question -> IO Answer
```

## Project Structure (Monorepo)

```
tidepool-core/             # Core library (reusable)
├── src/
│   ├── Tidepool.hs        # Re-exports
│   ├── Tidepool/
│   │   ├── Effect.hs      # LLM, RequestInput, State, Emit, Random, Time effects
│   │   ├── Template.hs    # TypedTemplate, Template (jinja + schema + tools)
│   │   ├── Tool.hs        # Tool typeclass, ToolList GADT
│   │   ├── Schema.hs      # JSON Schema derivation (deriveSchema)
│   │   ├── Storage.hs     # SQLite persistence for game state
│   │   ├── Graph/         # Type-level DSL for agent graphs
│   │   └── GUI/           # Generic threepenny-gui components
│   └── Delta/
│       └── Agent.hs       # Example delta routing agent
├── docs/                  # Core library documentation
├── templates/             # Example templates
└── test/                  # Graph validation tests

tidepool-dm/               # DM agent (Blades in the Dark)
├── src/DM/
│   ├── State.hs           # WorldState, PlayerState, dice mechanics, factions
│   ├── Output.hs          # TurnOutput with delta fields, applyTurnOutput
│   ├── Tools.hs           # ThinkAsDM, SpeakAsNPC, AskPlayer, Choose
│   ├── Context.hs         # DMContext, DiceContext, Precarity
│   ├── Templates.hs       # dmTurnTemplate, compressionTemplate
│   ├── Loop.hs            # dmTurn, handleDiceSelection, runDMGame
│   └── GUI/               # DM-specific GUI (noir aesthetic)
├── app/
│   ├── Main.hs            # CLI entry point
│   ├── MainGUI.hs         # GUI entry point
│   └── TestSchema.hs      # Schema test
├── docs/                  # DM-specific docs
└── templates/             # DM prompt templates (jinja/mustache)

tidepool-tidying/          # Tidying agent (executive function prosthetic)
├── src/Tidying/
│   ├── Agent.hs           # Agent definition, TidyingM monad
│   ├── State.hs           # SessionState, Phase, Piles, Photo
│   ├── Loop.hs            # OODA loop: tidyingTurn, photo analysis, extraction
│   ├── Decide.hs          # Pure routing: (State, Extract) → (Action, Phase)
│   ├── Action.hs          # Action ADT (InstructTrash, InstructPlace, etc.)
│   ├── Tools.hs           # ProposeDisposition, AskSpaceFunction, ConfirmDone
│   └── GUI/               # Tidying-specific GUI (calm aesthetic)
├── app/
│   ├── MainTidyGUI.hs     # GUI entry point
│   └── TestTidying.hs     # Test harness
└── templates/tidying/     # Tidying prompt templates

deploy/                    # Cloudflare Worker Durable Object harness
├── src/                   # TypeScript harness for WASM state machines
└── wrangler.toml          # CF Worker configuration
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
- Template system (ginger library with TH compile-time validation)
- JSON Schema derivation via Template Haskell (`deriveJSONSchema`)
- FitD dice mechanics types
- Precarity calculation
- Delta-based mutation types
- DM loop structure with RequestInput for dice selection
- **GUI: Core bridge** - GUIBridge with TVar/MVar communication
- **GUI: Generic widgets** - textInput, choiceCards, narrativePane, debugPanel, loadingOverlay
- **GUI: DM widgets** - Stats, Mood, Clocks, History, Narrative (with NPC bubbles), Dice
- **GUI: Theme system** - ColorPalette, CSS generation, noir theme
- **GUI: Polling loop** - Version-based change detection, efficient updates
- **GUI: Accessibility** - Keyboard navigation, ARIA labels, focus states
- **GUI: Handler** - makeGUIHandler for choice/text, guiDice for dice selection

### Stubbed / Partial
- GUI: Clock face SVG (currently text circles)

## Running

```bash
# Build all packages
cabal build all

# DM Agent
cabal run tidepool-dm      # CLI mode (hits TODO errors)
cabal run tidepool-dm-gui  # GUI mode (opens browser at localhost:8023)

# Tidying Agent
cabal run tidepool-tidy-gui  # GUI mode (opens browser at localhost:8024)
                             # Requires ANTHROPIC_API_KEY env var

# Tests
cabal run test-llm         # LLM integration test (tidepool-core)
cabal run test-schema      # Schema derivation test (tidepool-dm)
cabal test all             # Run all test suites
```

## Next Steps

### GUI Integration
1. **Clock SVG rendering** - Replace text circles with pie charts

## What Sleeptime Would Evolve

The "sleeptime" learning agent reads run logs and evolves:

1. **State fields** - Add new fields to WorldState
2. **Output schema** - New mutation types in TurnOutput
3. **Apply logic** - How mutations affect state
4. **Templates** - What context surfaces to LLM
5. **Tool behavior** - How tools execute

The loop structure, effect types, and infrastructure stay stable.

## Cloudflare Worker Deployment (`deploy/`)

The `deploy/` directory contains a Cloudflare Worker Durable Object harness for hosting compiled WASM state machines. This enables long-lived, WebSocket-based agent sessions running on the edge.

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  Cloudflare Durable Object                                  │
│  ┌───────────────────────────────────────────────────────┐  │
│  │  TypeScript Harness (deploy/src/)                     │  │
│  │  - WebSocket connection management                    │  │
│  │  - Effect interpreter (CF AI, fetch, logging)         │  │
│  │  - WASM instance lifecycle                            │  │
│  └─────────────────────┬─────────────────────────────────┘  │
│                        │ FFI (JSON)                         │
│  ┌─────────────────────▼─────────────────────────────────┐  │
│  │  WASM Module (GHC 9.10 → wasm32-wasi)                 │  │
│  │  - Effectful-based state machine                      │  │
│  │  - Yields serialized effect descriptions              │  │
│  │  - Pure business logic, no IO                         │  │
│  └───────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
```

### WASM Standin

**Current status**: The WASM module (`tidepool.wasm`) comes from a separate standin implementation at `~/dev/tidepool`. This standin demonstrates the effect protocol but will be replaced by compiling the Haskell code in this repo once:

1. The effectful-based agent loop is WASM-ready
2. GHC WASM cross-compilation is integrated into the build

The protocol types in `deploy/src/protocol.ts` define the contract between TypeScript and Haskell.

### Files

- `src/index.ts` - Durable Object with WebSocket handling, effect loop
- `src/loader.ts` - GHC WASM loader with JSFFI and WASI polyfills
- `src/protocol.ts` - TypeScript types matching Haskell Serializable.hs
- `src/jsffi.ts` - GHC WASM JavaScript FFI implementation
- `wrangler.toml` - CF Worker configuration (AI binding, DO class)

### Running

```bash
cd deploy
pnpm install
pnpm dev          # Local dev server
pnpm typecheck    # Type check
pnpm lint         # ESLint
pnpm deploy       # Deploy to Cloudflare
```

### WebSocket Protocol

```typescript
// Client → Server
{ type: "init", input: { messageText: "..." } }
{ type: "resume", result: { type: "success", value: ... } }

// Server → Client
{ type: "progress", node: "classify", effect: "LlmComplete" }
{ type: "done", result: { responseText: "..." } }
{ type: "error", message: "..." }
```

## References

- effectful: https://hackage.haskell.org/package/effectful
- Anthropic tool use: https://docs.anthropic.com/en/docs/tool-use
- Blades in the Dark SRD: https://bladesinthedark.com/
- heist-engine (v1 reference): ~/dev/shoal-automat/machines/heist-engine
- GHC WASM: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html
