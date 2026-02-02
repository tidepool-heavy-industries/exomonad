# Protocol Packages

Serialization formats for ExoMonad's wire protocols.

## Structure

| Package | Purpose | Key Types |
|---------|---------|-----------|
| `wire-types/` | Native GUI protocol → see wire-types/CLAUDE.md | `UIState`, `UserAction` |

## Protocol Overview

ExoMonad has one wire protocol:

### Native Protocol (wire-types)

For the native WebSocket server:

```
Server → Client:  UIState (chat messages, input configs, DM state)
Client → Server:  UserAction (text input, choice selection, photo upload)
```

**Format**: JSON over WebSocket
**Lifecycle**: See `native-server/PROTOCOL.md`

## Key Types

### UIState (Server → Client)

```haskell
data UIState = UIState
  { usMessages :: [ChatMessage]    -- Chat history
  , usTextInput :: Maybe TextInputConfig  -- Text input UI
  , usChoices :: Maybe ChoiceConfig       -- Choice buttons
  , usGraphNode :: Text                   -- Current node
  , usThinking :: Bool                    -- Loading state
  -- DM-specific fields (dice, clocks, etc.)
  }
```

### UserAction (Client → Server)

```haskell
data UserAction
  = UATextInput Text            -- User typed text
  | UAChoice Text               -- User selected choice
  | UAPhotoUpload ByteString    -- User uploaded photo
```

## Related Documentation

- [native-server/CLAUDE.md](../native-server/CLAUDE.md) - WebSocket server