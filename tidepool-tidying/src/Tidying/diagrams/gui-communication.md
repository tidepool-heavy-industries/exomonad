# GUI Communication

Two-thread architecture and TVar/MVar synchronization patterns.

## Two-Thread Architecture

```mermaid
flowchart TB
    subgraph GameThread["Game Loop Thread"]
        G1["tidyingGameLoopWithGUI"]
        G2["tidyingRun (effectful)"]
        G3["Blocks on takeMVar"]
    end

    subgraph GUIThread["GUI Browser Thread"]
        U1["Threepenny event loop"]
        U2["setupPolling (100ms)"]
        U3["renderQuestion"]
        U4["User interaction"]
    end

    subgraph Bridge["GUIBridge (Thread-Safe)"]
        B1["TVars (STM)"]
        B2["gbState: SessionState"]
        B3["gbStateVersion: Int"]
        B4["gbPendingRequest: Maybe PendingRequest"]
        B5["gbNarrativeLog: Seq Text"]
        B6["gbLLMActive: Bool"]
        B7["MVars (blocking)"]
        B8["gbRequestResponse: MVar RequestResponse"]
    end

    G2 --> B4
    G3 --> B8
    U2 --> B4
    U2 --> B2
    U4 --> B8
    G2 --> B2
```

## GUIBridge Type

```haskell
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
```

## Request/Response Sequence

```mermaid
sequenceDiagram
    participant GL as Game Loop
    participant BR as GUIBridge
    participant GUI as Browser

    GL->>BR: writeTVar gbPendingRequest (Just req)
    GL->>BR: takeMVar gbRequestResponse (BLOCKS)

    loop Every 100ms
        GUI->>BR: readTVar gbPendingRequest
        GUI->>GUI: Detect change (!=)
    end

    GUI->>GUI: renderQuestion / renderInput
    GUI->>GUI: User clicks/types
    GUI->>BR: tryPutMVar gbRequestResponse
    BR->>GL: Unblock with response
    GL->>BR: writeTVar gbPendingRequest Nothing
```

## State Sync Pattern

```mermaid
sequenceDiagram
    participant Agent as Agent Code
    participant State as State Effect
    participant Bridge as GUIBridge
    participant GUI as Browser Poll

    Agent->>State: Put newState
    State->>State: runState (local)
    State->>Bridge: updateState (TVar)
    State->>Bridge: gbStateVersion += 1

    Note over GUI: 100ms poll tick
    GUI->>Bridge: readTVar gbStateVersion
    GUI->>GUI: Detect version change
    GUI->>Bridge: readTVar gbState
    GUI->>GUI: updateUI(state)
```

## Request Types

```haskell
data PendingRequest
  = PendingText Text                    -- Text input (with photo option)
  | PendingChoice Text [(Text, Int)]    -- Choice buttons
  | PendingPhoto Text                   -- Photo upload
  | PendingDice Text [(Int, Int, Text)] -- Dice selection (DM agent)
  | PendingCustom Text Value            -- JSON (for Question DSL)
```

## Response Types

```haskell
data RequestResponse
  = ChoiceResponse Int                      -- Button index
  | TextResponse Text                       -- Text input
  | TextWithPhotoResponse Text Text Text    -- text + base64 + mime
  | PhotoResponse Text Text                 -- base64 + mime
  | CustomResponse Value                    -- JSON (for Answer)
```

## Polling Loop

The GUI polls for changes every 100ms:

```mermaid
flowchart TD
    A["Timer tick (100ms)"]
    B{"gbPendingRequest<br/>changed?"}
    C["updateInputArea"]
    D{"gbLLMActive<br/>changed?"}
    E["Show/hide spinner"]
    F{"gbNarrativeVersion<br/>changed?"}
    G["updateChatPane"]
    H{"gbStateVersion<br/>changed?"}
    I["Update phase label"]
    J{"gbDebugVersion<br/>changed?"}
    K["updateDebugPanel"]

    A --> B
    B -->|yes| C
    B -->|no| D
    D -->|yes| E
    D -->|no| F
    F -->|yes| G
    F -->|no| H
    H -->|yes| I
    H -->|no| J
    J -->|yes| K
```

## Double-Click Prevention

`safeSubmitResponse` uses `tryPutMVar` to never block:

```haskell
safeSubmitResponse :: GUIBridge state -> RequestResponse -> IO Bool
safeSubmitResponse bridge response = tryPutMVar bridge.gbRequestResponse response
-- Returns False if MVar already full (double-click ignored)
```

## Key Files

- `Tidepool/GUI/Core.hs` - GUIBridge, PendingRequest, RequestResponse
- `Tidepool/GUI/Handler.hs` - makeGUIHandler, guiChoice, guiText
- `Tidying/GUI/App.hs` - setupPolling, updateInputArea
- `Tidying/GUI/Runner.hs` - tidyingGameLoopWithGUI
