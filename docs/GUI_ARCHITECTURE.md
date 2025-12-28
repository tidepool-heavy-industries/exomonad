# Tidepool GUI Architecture

## Overview

The GUI uses a polling-based architecture with threepenny-gui. The game loop and GUI run in the same process, communicating via TVar/MVar.

**Key Design Principles:**
1. External code updates the bridge state; GUI refreshes automatically
2. Version numbers enable efficient change detection
3. Safe submission prevents double-click blocking
4. Generic (Tidepool.GUI.*) vs domain-specific (DM.GUI.*) separation

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
│   └──────────────────┘         └──────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

## Module Structure

### Generic (Tidepool.GUI.*)

These modules are reusable for any Tidepool-based game:

| Module | Purpose |
|--------|---------|
| `Core` | GUIBridge, PendingRequest, RequestResponse, state helpers |
| `Handler` | InputHandler that bridges to GUI |
| `Server` | Threepenny server configuration |
| `Theme` | Theme type, CSS generation, base styles |
| `Widgets` | TextInput, ChoiceCards, NarrativePane, DebugPanel |

### Domain-Specific (DM.GUI.*)

These modules provide Blades in the Dark styling:

| Module | Purpose |
|--------|---------|
| `App` | Main layout, polling, element management |
| `Theme` | Dark noir color palette |
| `Widgets.Stats` | Stress, coin, heat, wanted, trauma |
| `Widgets.Mood` | DMMood header display |
| `Widgets.Dice` | Visual dice with tier colors |
| `Widgets.Clocks` | Progress clocks with hover tooltips |
| `Widgets.History` | Scene beat log |
| `Widgets.Narrative` | Rich narrative with NPC speech bubbles |

## Key Types

### GUIBridge

```haskell
data GUIBridge state = GUIBridge
  { gbState            :: TVar state        -- Game state
  , gbStateVersion     :: TVar Int          -- Increments on change
  , gbPendingRequest   :: TVar (Maybe PendingRequest)
  , gbRequestResponse  :: MVar RequestResponse
  , gbNarrativeLog     :: TVar (Seq Text)
  , gbNarrativeVersion :: TVar Int          -- Increments on change
  , gbDebugLog         :: TVar (Seq DebugEntry)
  , gbLLMActive        :: TVar Bool
  }
```

The version fields enable efficient change detection - the polling loop
compares versions instead of deep-comparing state.

### Request/Response

```haskell
data PendingRequest
  = PendingChoice Text [(Text, Int)]  -- Prompt + options
  | PendingText Text                   -- Prompt for text input

data RequestResponse
  = ChoiceResponse Int
  | TextResponse Text
```

## Integration Points

### 1. Creating the GUI

```haskell
main :: IO ()
main = do
  -- Create bridge with initial state
  bridge <- newGUIBridge initialWorldState

  -- Start server
  let config = defaultServerConfig { scTitle = "My Game" }
  startServer config (dmGUISetup defaultDMGUIConfig bridge)
```

### 2. Using InputHandler

```haskell
-- Create handler for the game loop
let handler = makeGUIHandler bridge

-- Use in your effect interpreter
runInputHandler :: InputHandler -> Eff '[RequestInput, ...] a -> IO a
runInputHandler handler = interpret $ \case
  RequestChoice prompt opts -> liftIO $ ihChoice handler prompt opts
  RequestText prompt -> liftIO $ ihText handler prompt
```

### 3. Updating State (Automatic Refresh)

```haskell
-- Update state - GUI refreshes automatically on next poll tick
updateState bridge $ \world -> world { player = newPlayer }

-- Add narrative - GUI refreshes narrative pane automatically
addNarrative bridge "The guard notices you..."

-- The version numbers are incremented automatically, and the
-- polling loop detects changes and refreshes the appropriate widgets.
```

**Important:** External code does NOT need to call refresh functions.
Just update the bridge state, and the GUI handles the rest.

### 4. LLM Activity Indicator

```haskell
-- Wrap LLM calls to show loading
result <- withLLMActive bridge $ do
  callAnthropicAPI request
```

### 5. Custom Narrative Rendering

For domain-specific narrative rendering, use `DM.GUI.Widgets.Narrative`:

```haskell
-- Instead of generic narrativePane:
dmNarrativePane :: GUIBridge WorldState -> UI Element
-- Renders SceneBeats with NPC speech bubbles, player actions, etc.
```

## Refresh Patterns

### Polling (Current)

The GUI polls TVars at a configurable interval:

```haskell
setupPolling config bridge elements
-- Checks: gbPendingRequest, gbLLMActive
-- Tracks previous state to avoid unnecessary DOM updates
```

### Manual Refresh (For State Changes)

When WorldState changes externally, call refresh functions:

```haskell
refreshStats bridge elements      -- Rebuild stats sidebar
refreshMood bridge elements       -- Update mood display
refreshNarrative bridge elements  -- Rebuild narrative pane
```

### Future: Event-Driven

For better performance, consider adding an event channel:

```haskell
data GUIEvent
  = StateChanged
  | NarrativeAdded
  | MoodChanged

-- GUI subscribes to events instead of polling
```

## CSS Customization

Themes are CSS custom properties:

```css
:root {
  --bg-primary: #1a1a1e;
  --accent: #c9a227;
  --text-primary: #e8e8e8;
  /* ... */
}
```

To create a custom theme:

```haskell
myTheme :: Theme
myTheme = Theme
  { themeName = "My Theme"
  , themeColors = ColorPalette { ... }
  , themeFontMain = "'Inter', sans-serif"
  , themeFontMono = "'JetBrains Mono', monospace"
  }

-- Apply in setup:
applyTheme myTheme window
```

## Keyboard Shortcuts

- **Text Input**: Enter to submit
- **Choice Cards**:
  - Tab between cards
  - Enter/Space to select focused card
  - 1-9 for quick selection

## Thread Safety

### Safe Response Submission

The widgets use `tryPutMVar` instead of `putMVar` to prevent blocking
on double-clicks:

```haskell
safeSubmitResponse :: GUIBridge state -> RequestResponse -> IO ()
safeSubmitResponse bridge response =
  void $ tryPutMVar bridge.gbRequestResponse response
-- If MVar is full (response already pending), silently ignore
```

### Version-Based Change Detection

The polling loop tracks version numbers to avoid unnecessary DOM updates:

```haskell
-- Only refresh if version changed
stateVersion <- atomically $ readTVar bridge.gbStateVersion
when (stateVersion /= prevStateVersion) $ do
  writeIORef prevStateVersionRef stateVersion
  refreshStats bridge elements
  refreshMood bridge elements
```

## Widget Layout Patterns

Widgets that combine a prompt with interactive elements use a two-level
structure to ensure proper layout:

```haskell
-- ❌ Don't: Mix prompt with flex items
container <- UI.div #. "flex-container"
promptEl <- UI.div #. "prompt" # set text prompt
cards <- mapM mkCard options
void $ element container #+ (element promptEl : map element cards)
-- Problem: prompt is in same flex flow as cards

-- ✅ Do: Separate prompt from flex container
wrapper <- UI.div #. "wrapper"
promptEl <- UI.div #. "prompt" # set text prompt
container <- UI.div #. "flex-container"
cards <- mapM mkCard options
void $ element container #+ map element cards
void $ element wrapper #+ [element promptEl, element container]
```

CSS for the wrapper uses `flex-direction: column`:

```css
.wrapper {
  display: flex;
  flex-direction: column;
  gap: 12px;
}
```

## TODO / Future Work

- [ ] Event-driven updates using threepenny-gui's FRP capabilities
- [ ] Debug panel collapse/expand toggle
- [ ] Debug log filtering by level
- [ ] Mobile responsive styles with media queries
- [ ] Sound effects for events
- [ ] CSS animation for state transitions
- [ ] Connection status indicator (websocket health)
- [ ] Error toast/notification system
- [ ] ARIA labels for accessibility
