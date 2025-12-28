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

### Dice Integration Note

For full dice integration with the effect system, add to `InputHandler`:

```haskell
-- In Tidepool.Effect (TODO)
data InputHandler = InputHandler
  { ihChoice :: forall a. Text -> [(Text, a)] -> IO a
  , ihText   :: Text -> IO Text
  , ihDice   :: Text -> [(Int, Int)] -> IO Int  -- NEW
  }
```

Until then, use `guiDice` from `Tidepool.GUI.Handler` directly:

```haskell
import Tidepool.GUI.Handler (guiDice)

-- In game logic
let pool = [(4, 0), (2, 1), (6, 2)]  -- (value, index)
selectedIdx <- guiDice bridge "Choose a die:" pool
```

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
│   ├── Loop.hs            # TurnConfig, Compression abstractions
│   └── GUI/               # Generic threepenny-gui components
│       ├── Core.hs        # GUIBridge, PendingRequest, safeSubmitResponse
│       ├── Handler.hs     # makeGUIHandler, guiDice
│       ├── Server.hs      # startServer, threepenny config
│       ├── Theme.hs       # Theme type, ColorPalette, CSS generation
│       └── Widgets.hs     # textInput, choiceCards, narrativePane, debugPanel
└── DM/                    # DM agent (domain-specific)
    ├── State.hs           # WorldState, PlayerState, dice mechanics, factions...
    ├── Output.hs          # TurnOutput with delta fields, applyTurnOutput
    ├── Tools.hs           # ThinkAsDM, SpeakAsNPC, AskPlayer, Choose
    ├── Context.hs         # DMContext, DiceContext, Precarity
    ├── Templates.hs       # dmTurnTemplate, compressionTemplate
    ├── Loop.hs            # dmTurn, handleDiceSelection, runDMGame
    └── GUI/               # DM-specific GUI (Blades aesthetic)
        ├── App.hs         # Main layout, polling loop, widget wiring
        ├── Theme.hs       # Dark noir theme, tier colors
        └── Widgets/
            ├── Stats.hs   # stressBar, coinDisplay, heatBar, wantedPips
            ├── Mood.hs    # moodHeader, moodDisplay
            ├── Clocks.hs  # clocksPanel, clockWidget with hover tooltips
            ├── History.hs # historyTab, sceneBeatEntry, sceneSummaryEntry
            ├── Narrative.hs # dmNarrativePane with NPC speech bubbles
            └── Dice.hs    # diceChoice with tier colors, keyboard nav
app/
├── Main.hs                # Executable entry point
└── MainGUI.hs             # GUI demo executable
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
- **GUI: Core bridge** - GUIBridge with TVar/MVar communication
- **GUI: Generic widgets** - textInput, choiceCards, narrativePane, debugPanel, loadingOverlay
- **GUI: DM widgets** - Stats, Mood, Clocks, History, Narrative (with NPC bubbles), Dice
- **GUI: Theme system** - ColorPalette, CSS generation, noir theme
- **GUI: Polling loop** - Version-based change detection, efficient updates
- **GUI: Accessibility** - Keyboard navigation, ARIA labels, focus states
- **GUI: Handler** - makeGUIHandler for choice/text, guiDice for dice selection

### Stubbed / Partial
- Template rendering (waiting for jinja-th package)
- `applyTurnOutput` and mutation appliers
- `buildDMContext` and context enrichment
- JSON Schema derivation
- GUI: Clock face SVG (currently text circles)
- GUI: `ihDice` in InputHandler (use `guiDice` directly for now)

## Running

```bash
cabal build               # Succeeds
cabal run tidepool-dm     # Runs but hits TODO errors
cabal run tidepool-dm-gui # GUI demo (opens browser at localhost:8023)
```

## Next Steps

### Core Library
1. **jinja-th integration** - Replace placeholder TypedTemplate with real TH splice
2. **Schema derivation** - `deriveSchema` via GHC.Generics
3. **Apply functions** - Wire up `applyTurnOutput` with optics

### GUI Integration
4. **Add `ihDice` to InputHandler** - Full effect system integration for dice
5. **Wire GUI to real game loop** - Connect dmTurn to GUIBridge
6. **Clock SVG rendering** - Replace text circles with pie charts

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
