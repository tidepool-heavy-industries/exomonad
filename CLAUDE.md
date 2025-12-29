# Tidepool DM Agent - Haskell Sketch

## What This Is

A type-safe LLM agent loop library (Tidepool) demonstrated via a Blades in the Dark-style Dungeon Master agent. The key insight: LLMs don't need raw IO - they need typed state they can read (via templates) and typed mutations they can express (via structured output).

**Status**: Functional game loop with LLM integration. Core mechanics wired: dice selection, clock consequences, mood transitions, compression. See `src/DM/CLAUDE.md` for exhaustive DM system documentation.

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
│   ├── Effect.hs          # LLM, RequestInput, State, Emit, Random, ChatHistory effects
│   ├── Template.hs        # TypedTemplate, Template (jinja + schema + tools)
│   ├── Tool.hs            # Tool typeclass, ToolList GADT
│   ├── Schema.hs          # JSON Schema derivation (deriveSchema)
│   ├── Loop.hs            # TurnConfig, Compression abstractions
│   ├── Storage.hs         # SQLite persistence for games, runChatHistoryWithDB
│   ├── Anthropic/         # Claude API integration
│   │   ├── Http.hs        # Raw HTTP client (req-based)
│   │   └── Client.hs      # High-level client with tool dispatch
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
    ├── Tarot.hs           # Tarot card types for scene/NPC seeds
    ├── CharacterCreation.hs # Character generation flow
    └── GUI/               # DM-specific GUI (Blades aesthetic)
        ├── App.hs         # Main layout, polling loop, widget wiring
        ├── Theme.hs       # Dark noir theme, tier colors
        └── Widgets/
            ├── Stats.hs   # stressBar, coinDisplay, heatBar, wantedPips
            ├── Mood.hs    # moodHeader, moodDisplay
            ├── Clocks.hs  # clocksPanel, clockWidget with hover tooltips
            ├── History.hs # historyTab, sceneBeatEntry, sceneSummaryEntry
            ├── Narrative.hs # dmNarrativePane with NPC speech bubbles
            ├── Dice.hs    # diceChoice with tier colors, keyboard nav
            ├── State.hs   # WorldState debug view
            ├── Events.hs  # Game event log display
            ├── BetweenScenes.hs # Inter-scene transition UI
            └── CharacterCreation.hs # Character creation wizard
app/
├── Main.hs                # Main game executable (tidepool-dm)
├── MainGUI.hs             # GUI demo executable (tidepool-dm-gui)
└── TestLLM.hs             # LLM integration test (test-llm)
templates/                 # Jinja templates (ginger library)
├── dm_turn.jinja          # Main DM turn prompt
├── compression.jinja      # Scene compression prompt
├── scene/                 # Scene-phase templates
├── action/                # Action-phase templates
├── aftermath/             # Aftermath-phase templates
├── downtime/              # Downtime-phase templates
├── bargain/               # Devil's bargain templates
├── trauma/                # Trauma handling templates
├── partials/              # Reusable template blocks
└── _shared/               # Shared template fragments
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

### Fully Wired
- **Effect system** - LLM, State, Emit, RequestInput, Random, Log, ChatHistory
- **Game loop** - dmTurn with mood-based template selection, tool dispatch, output application
- **Dice mechanics** - Pool management, die selection UI, outcome tier calculation, cost application
- **Clock system** - Tick via LLM output, consequence dispatch on completion, visible/hidden split
- **Mood state machine** - Scene → Action → Aftermath → Downtime/Trauma, with interrupts
- **Compression** - Scene summarization, rumor extraction, thread spawning
- **Tools (9)** - All dispatchable: think, speak_as_npc, ask_player, choose, engage, spend_die, resolve, accept, set_scene_style
- **Events (14)** - All emitted and handled for state change notifications
- **Context building** - DMContext from WorldState with precarity, NPCs, clocks, rumors, threads
- **Template rendering** - Ginger (Jinja) with mood-specific templates
- **GUI** - Full threepenny-gui with stats, clocks, dice, narrative, history tabs

### Partial / WIP
- Template type-checking (runtime Ginger, not compile-time jinja-th)
- JSON Schema derivation (manual schemas)
- GUI: Clock SVG rendering (currently text circles)
- GUI: `ihDice` in InputHandler (use `guiDice` directly)

### Removed Dead Code (2024-12)
- `clockTriggers` system (Trigger, ActionPattern, Duration types) - clocks tick via LLM output only

## Development Commands

```bash
# Build
cabal build                    # Build all targets
cabal build tidepool-sketch    # Build library only

# Run executables
cabal run tidepool-dm          # Main game (CLI + GUI, opens localhost:8023)
cabal run tidepool-dm-gui      # GUI demo only
cabal run test-llm             # Test LLM integration

# Development workflow
cabal check                    # Validate cabal file
cabal clean                    # Clean build artifacts

# Environment
export ANTHROPIC_API_KEY=...   # Required for LLM calls
```

### GHC Extensions (enabled by default)

The cabal file enables GHC2021 plus: `DataKinds`, `GADTs`, `TypeFamilies`, `TypeOperators`, `OverloadedStrings`, `OverloadedRecordDot`, `NoFieldSelectors`, `DuplicateRecordFields`, `QuasiQuotes`, `TemplateHaskell`

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

## Code Organization: Quality Without a Name

These principles draw from Christopher Alexander's architectural philosophy. Good code, like good buildings, has a quality that's hard to name but unmistakable: it feels *alive*, coherent, like each part belongs where it is. The opposite—code that's merely correct—feels dead, arbitrary, fragile.

### One Path Through the Code

**Prefer N sources → 1 transformation → 1 output over N parallel implementations.**

When the same logic appears in multiple places, the code loses coherence. Each copy becomes a center that competes rather than strengthens.

```haskell
-- Bad: Two moods, two render paths
renderSceneNarrative :: SceneContext -> Text
renderSceneNarrative ctx = ...  -- 50 lines

renderActionNarrative :: ActionContext -> Text
renderActionNarrative ctx = ...  -- 50 similar lines with subtle drift

-- Good: Multiple moods flow through one render path
renderForMood :: DMMood -> DMContext -> Text
renderForMood mood ctx = case mood of
  MoodScene _     -> render sceneTemplate ctx
  MoodAction _ _  -> render actionTemplate ctx
  MoodAftermath _ -> render aftermathTemplate ctx
  ...
-- The mood selects WHICH template; the rendering path is unified
```

**In this codebase:**
- `renderForMood` unifies 6 mood-specific templates through one function
- GUI polling: multiple state sources (WorldState, narrative, requests) → single refresh loop → widgets
- Effect interpretation: one `runDMGame` wires all effect handlers

### Centers Strengthen Each Other

Good boundaries make each part more alive, not less. When you extract something, both the extraction and the remainder should feel more coherent.

```haskell
-- Bad: Extraction that weakens both sides
-- DMContext has 30 fields; Template has 30 fields; they mirror each other
data DMContext = DMContext { ... }  -- Everything the LLM might need
data Template = Template { ... }    -- Same fields, different order

-- Good: Each center has its own gravity
data DMContext = DMContext          -- What the world looks like NOW
  { ctxLocation, ctxPresentNpcs, ctxStakes, ctxPrecarity, ... }

data Template context output = Template  -- How to invoke the LLM
  { templateJinja :: TypedTemplate context
  , templateOutputSchema :: Schema output
  , templateTools :: ToolList
  }
-- DMContext knows what to show; Template knows how to render
```

### Structure Grows from Life

Don't anticipate abstractions. Let them emerge from repeated use. The right abstraction appears after you've written the same pattern three times and felt the friction.

```haskell
-- Premature: Abstracting before patterns emerge
class Renderable a where
  render :: a -> Text

-- Emerged: The actual pattern that revealed itself
-- After writing sceneTemplate, actionTemplate, aftermathTemplate...
-- the Template type emerged naturally:
data Template context output event state tools = Template
  { templateJinja :: TypedTemplate context SourcePos
  , templateOutputSchema :: Schema output
  , templateTools :: ToolList tools event state
  }
```

**Signs you're anticipating:**
- The abstraction has one use case
- You're adding type parameters "for flexibility"
- The concrete version would be simpler to read

**Signs it emerged:**
- You've felt the friction of duplication
- The abstraction deletes code rather than adding indirection
- Each use case fits naturally

### Roughness Over Perfection

Working code with character beats polished code that's fragile. Leave room for the structure to breathe.

```haskell
-- Over-polished: Every edge case handled, but brittle
handleEvent :: Event -> StateT World (ExceptT Error IO) Response
handleEvent = \case
  Click x y -> validateCoords x y >>= \case
    Valid coords -> processClick coords >>= \case
      ...  -- 50 lines of nested case handling

-- Rough but alive: Clear happy path, simple error boundary
handleEvent :: Event -> IO (Either Error Response)
handleEvent (Click x y) = runExceptT $ do
  coords <- validateCoords x y
  processClick coords
handleEvent _ = pure $ Right defaultResponse
-- Less "complete" but easier to read, extend, debug
```

**Roughness means:**
- Handling the common cases well, not all cases
- Preferring simple error messages over elaborate recovery
- Leaving TODOs for genuinely uncertain futures
- Three lines of similar code > one clever abstraction

### The Test: Does It Feel Alive?

When reviewing code, ask:
- Does each function have one clear purpose, or is it doing several things?
- Do the boundaries feel natural, or forced?
- Can you hold the structure in your head, or does it keep slipping away?
- Would adding a feature feel like growth, or surgery?

Code with the quality without a name invites contribution. Code without it resists every change.

## Haskell Design Principles

These principles guide architectural decisions in this codebase. Each explains what the type system enforces versus what requires programmer discipline.

### Make Invalid States Unrepresentable

Don't validate—make it impossible to construct bad data.

```haskell
-- Bad: runtime checks scattered everywhere
data GamePhase = Scene | Action | Aftermath | Downtime
data WorldState = WorldState { phase :: GamePhase, pendingOutcome :: Maybe PendingOutcome }
-- Every function must check: is pendingOutcome Nothing when phase /= Action?

-- Good: impossible to have pendingOutcome in wrong phase
data GamePhase
  = PhaseScene SceneContext
  | PhaseAction PendingOutcome  -- outcome MUST exist in action phase
  | PhaseAftermath AftermathContext
  | PhaseDowntime DowntimeContext
-- Type system enforces: PendingOutcome exists iff in Action phase
```

**Type system enforces**: Once modeled as a sum type with phase-specific payloads, illegal combinations won't compile.
**Discipline required**: Choosing to model it this way in the first place.

### Liberties Constrain; Constraints Liberate

The more polymorphic your function, the less it can do—and the more you know about it from the type alone.

```haskell
-- Very constrained type → almost no implementation freedom
mystery :: forall a. [a] -> [a]
-- Can only rearrange, duplicate, or drop elements. Cannot create new 'a's, or compare them for equality/ordering.
-- Parametricity gives you theorems for free. Lack of constraints provides documentation


-- Specifically constrained type
mystery :: forall a. Ord => [a] -> [a]
-- Can only compare elements, nothing else. Almost certainly a sorting function
-- Constraints provide guidance and documentation.

-- Very specific type → implementation could do anything
mystery' :: [Int] -> [Int]
-- Could return [42] always, could sum them, could do anything.
```

**Type system enforces**: Parametric polymorphism guarantees the function can't inspect or create values of type `a`.
**Discipline required**: Choosing to keep types polymorphic where possible.

### Parse, Don't Validate

Transform unstructured data into typed data at system boundaries. Then trust the types internally.

```haskell
-- Bad: validate repeatedly, forget to check somewhere
processInput :: Text -> IO ()
processInput t = do
  when (T.null t) $ throwIO EmptyInput  -- must remember every time
  ...

-- Good: parse once into a type that cannot be empty
newtype NonEmptyText = NonEmptyText Text  -- constructor not exported

mkNonEmptyText :: Text -> Maybe NonEmptyText
mkNonEmptyText t | T.null t  = Nothing
                 | otherwise = Just (NonEmptyText t)

processInput :: NonEmptyText -> IO ()  -- impossible to receive empty
processInput (NonEmptyText t) = ...  -- no check needed, type guarantees it
```

**Type system enforces**: Once you have a `NonEmptyText`, it cannot be empty.
**Discipline required**: Parse at boundaries; don't export raw constructors.

### Newtypes Prevent Primitive Obsession

Wrap domain concepts so the compiler catches misuse.

```haskell
-- Bad: which Int is which?
applyDamage :: Int -> Int -> Int -> WorldState -> WorldState
applyDamage stress heat wanted = ...
-- Easy to call: applyDamage heat stress wanted (wrong order!)

-- Good: distinct types for distinct concepts
newtype Stress = Stress { unStress :: Int }
newtype Heat = Heat { unHeat :: Int }
newtype Wanted = Wanted { unWanted :: Int }

applyDamage :: Stress -> Heat -> Wanted -> WorldState -> WorldState
-- applyDamage (Heat 2) (Stress 1) ... won't compile
```

**Type system enforces**: Cannot pass `Heat` where `Stress` expected.
**Discipline required**: Define newtypes; resist the urge to unwrap everywhere.

### Total Functions (Avoid Partiality)

Partial functions (`head`, `!!`, incomplete patterns) push errors to runtime. Total functions encode failure in types.

```haskell
-- Partial: crashes on empty list
firstElement :: [a] -> a
firstElement (x:_) = x  -- warning: incomplete patterns

-- Total: failure is in the type
firstElement :: [a] -> Maybe a
firstElement []    = Nothing
firstElement (x:_) = Just x
```

**Type system enforces**: Callers must handle `Nothing`—can't ignore possible failure.
**Discipline required**: Enable `-Wincomplete-patterns`, avoid `error`/`undefined` except for genuine impossibilities.

### Effects in the Types

Make side effects visible in type signatures. This is the core insight of effectful (and MTL, free monads, etc.).

```haskell
-- Type tells you exactly what effects this function can perform
dmTurn
  :: (LLM :> es, State WorldState :> es, Emit GameEvent :> es, RequestInput :> es)
  => Text
  -> Eff es Text

-- Compare to:
dmTurn :: Text -> IO Text  -- could do literally anything
```

**Type system enforces**: Function can only use effects listed in constraints.
**Discipline required**: Define small, focused effect sets; resist `IOE` escape hatch.

### Smart Constructors

Hide data constructors, expose validated creation functions. Maintain invariants by construction.

```haskell
module Clock (Clock, mkClock, tickClock, clockProgress) where

-- Constructor not exported
data Clock = Clock
  { clockName :: Text
  , clockSegments :: Int  -- invariant: 4, 6, or 8
  , clockFilled :: Int    -- invariant: 0 <= filled <= segments
  }

-- Only way to create a Clock
mkClock :: Text -> Int -> Maybe Clock
mkClock name segs
  | segs `elem` [4, 6, 8] = Just (Clock name segs 0)
  | otherwise = Nothing

-- Maintains invariants
tickClock :: Clock -> Clock
tickClock c = c { clockFilled = min (clockSegments c) (clockFilled c + 1) }
```

**Type system enforces**: External code cannot construct invalid `Clock` values.
**Discipline required**: Actually hide the constructor in the export list.

### Phantom Types for Compile-Time Tags

Add type parameters that exist only at compile time to track state or capabilities.

```haskell
data Validated
data Unvalidated

newtype UserId (status :: Type) = UserId Text

-- Only validated IDs can query the database
lookupUser :: UserId Validated -> IO User

-- Validation promotes Unvalidated → Validated
validateUserId :: UserId Unvalidated -> IO (Maybe (UserId Validated))
validateUserId (UserId t) = do
  exists <- checkUserExists t
  pure $ if exists then Just (UserId t) else Nothing
```

**Type system enforces**: Cannot call `lookupUser` with unvalidated ID.
**Discipline required**: Choosing to use phantom types; not using `coerce` to bypass.

### Summary: The Type System Is Your Proof Assistant

Each principle shifts invariant checking from runtime to compile time. The pattern is always:

1. **Model** the invariant in types (sum types, newtypes, GADTs, phantom types)
2. **Parse** at boundaries into the typed representation
3. **Trust** the types internally—no defensive checks

The compiler then verifies your program maintains invariants across all code paths. The cost is upfront modeling; the payoff is fearless refactoring.

## References

- effectful: https://hackage.haskell.org/package/effectful
- Anthropic tool use: https://docs.anthropic.com/en/docs/tool-use
- Blades in the Dark SRD: https://bladesinthedark.com/
- heist-engine (v1 reference): ~/dev/shoal-automat/machines/heist-engine
