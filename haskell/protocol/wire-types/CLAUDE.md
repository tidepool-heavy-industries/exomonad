# Wire Types - Native GUI Protocol

WebSocket protocol types for the native server.

## When to Read This

Read this if you're:
- Modifying the native GUI state
- Adding new UI input types
- Debugging WebSocket communication
- Understanding the DM game state

## Protocol Overview

```
Server                          Client
        │                                │
        │◄───── UserAction ──────────────│  (user typed, clicked, uploaded)
        │                                │
        │────── UIState ────────────────►│  (chat, inputs, DM state)
        │                                │
```

**Format**: JSON over WebSocket

## Key Types

### UIState (Server → Client)

Complete UI state sent after each graph step:

```haskell
data UIState = UIState
  { usMessages :: [ChatMessage]         -- Chat history
  , usTextInput :: Maybe TextInputConfig    -- Text input field
  , usPhotoUpload :: Maybe PhotoUploadConfig -- Photo upload UI
  , usChoices :: Maybe ChoiceConfig     -- Choice buttons
  , usGraphNode :: Text                 -- Current node name
  , usThinking :: Bool                  -- Loading indicator
  -- DM-specific fields:
  , usDMStats :: Maybe DMStats          -- Player stats
  , usDMClocks :: [Clock]               -- Progress clocks
  , usDMDicePool :: Maybe DicePool      -- Available dice
  , usDMMood :: Maybe DMMood            -- DM mood indicator
  , usDMCharCreation :: Maybe CharacterCreation
  , usDMHistory :: [HistoryEntry]       -- Session history
  }
```

### UserAction (Client → Server)

User interactions:

```haskell
data UserAction
  = UATextInput Text            -- User typed and submitted
  | UAChoice Text               -- User clicked choice button
  | UAPhotoUpload ByteString    -- User uploaded image
```

### ChatMessage

```haskell
data ChatMessage = ChatMessage
  { cmRole :: MessageRole       -- User | Assistant | System
  , cmContent :: Text           -- Message text
  , cmTimestamp :: Maybe UTCTime
  }
```

### Choice Configuration

```haskell
data ChoiceConfig = ChoiceConfig
  { ccOptions :: [ChoiceOption]  -- Available choices
  , ccPrompt :: Maybe Text       -- Prompt text
  }

data ChoiceOption = ChoiceOption
  { coLabel :: Text              -- Button text
  , coValue :: Text              -- Value sent on click
  , coDescription :: Maybe Text  -- Tooltip
  }
```

## DM Game Types

For the Dungeon Master game mode, additional types:

| Type | Purpose |
|------|---------|
| `DMStats` | Player precarity, position, effects |
| `Clock` | Progress clocks (ticking danger) |
| `DicePool` | Available dice for rolls |
| `TarotCard`, `TarotSpread` | Character creation |
| `HistoryEntry` | Session narrative log |

## JSON Encoding

All types use Aeson's generic derivation with lowercase field names:

```haskell
instance ToJSON UIState where
  toJSON s = object
    [ "messages" .= usMessages s
    , "textInput" .= usTextInput s
    , "choices" .= usChoices s
    -- ...
    ]
```

## Related Documentation

- [protocol/CLAUDE.md](../CLAUDE.md) - Protocol overview