# Protocol Packages

Serialization formats for ExoMonad's wire protocols.

## Structure

| Package | Purpose | Key Types |
|---------|---------|-----------|
| `wire-types/` | Native GUI protocol → see wire-types/CLAUDE.md | `UIState`, `UserAction` |
| `generated-ts/` | WASM TypeScript codegen → see generated-ts/CLAUDE.md | Generated from Haskell |

## Protocol Overview

ExoMonad has two wire protocols:

### Native Protocol (wire-types)

For the native WebSocket server ↔ Solid.js frontend:

```
Server → Client:  UIState (chat messages, input configs, DM state)
Client → Server:  UserAction (text input, choice selection, photo upload)
```

**Format**: JSON over WebSocket
**Lifecycle**: See `native-server/PROTOCOL.md`

### WASM Protocol (generated-ts)

For WASM graphs ↔ TypeScript harness (Cloudflare Workers):

```
WASM yields:    SerializableEffect (effect to execute)
Harness returns: EffectResult (result from TypeScript)
```

**Format**: JSON across WASM FFI boundary
**Generation**: Run `cabal run generate-ts-package`

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

## TypeScript Codegen

The `generated-ts` package generates TypeScript from Haskell metadata:

1. Protocol types matching Haskell wire types
2. Effect routing tables (internal vs yielded)
3. WASM loader with GHC RTS setup
4. Type-safe dispatcher

**To regenerate:**
```bash
cd exomonad-wasm && cabal run generate-ts-package -- ../deploy/exomonad-generated-ts
```

## Related Documentation

- [native-server/CLAUDE.md](../native-server/CLAUDE.md) - WebSocket server
- [deploy/CLAUDE.md](../../deploy/CLAUDE.md) - Cloudflare Worker using these types
- [typescript/native-gui/CLAUDE.md](../../typescript/native-gui/CLAUDE.md) - Solid.js frontend
