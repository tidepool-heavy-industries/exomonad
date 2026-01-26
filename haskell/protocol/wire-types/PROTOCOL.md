# ExoMonad Native GUI WebSocket Protocol

This document describes the WebSocket protocol between the Haskell server and the Solid frontend.

## Connection

The frontend connects to the server via WebSocket:

```
ws://localhost:{PORT}
```

Default port is `8080`. Configure via `EXOMONAD_PORT` environment variable.

## Message Flow

```
┌─────────────────────┐                    ┌─────────────────────┐
│  Haskell Server     │                    │  Solid Frontend     │
│                     │                    │                     │
│  Graph execution    │─── UIState ───────>│  Renders UI         │
│  Effect execution   │                    │  (messages, inputs, │
│                     │                    │   buttons, spinner) │
│                     │                    │                     │
│  Receives action    │<── UserAction ─────│  User interacts     │
│  Resumes graph      │                    │  (type, click, etc) │
│                     │                    │                     │
└─────────────────────┘                    └─────────────────────┘
```

## Protocol

### Server → Client: `UIState`

After each graph step that produces UI changes, the server sends the complete UI state as JSON:

```json
{
  "messages": [
    {
      "role": "assistant",
      "content": "Hello! How can I help you today?",
      "timestamp": "2024-01-15T10:30:00Z"
    }
  ],
  "textInput": {
    "placeholder": "Type your message..."
  },
  "photoUpload": null,
  "buttons": null,
  "graphNode": "greeting",
  "thinking": false
}
```

#### Fields

| Field | Type | Description |
|-------|------|-------------|
| `messages` | `ChatMessage[]` | Full conversation history |
| `textInput` | `TextInputConfig \| null` | Show text input if non-null |
| `photoUpload` | `PhotoUploadConfig \| null` | Show photo upload if non-null |
| `buttons` | `ButtonConfig[] \| null` | Show buttons if non-null |
| `graphNode` | `string` | Current graph node (for observability) |
| `thinking` | `boolean` | Show loading spinner |

#### ChatMessage

```json
{
  "role": "user" | "assistant" | "system",
  "content": "Message text",
  "timestamp": "2024-01-15T10:30:00Z"
}
```

#### TextInputConfig

```json
{
  "placeholder": "Enter text..."
}
```

#### PhotoUploadConfig

```json
{
  "prompt": "Take a photo of the item"
}
```

#### ButtonConfig

```json
{
  "id": "confirm",
  "label": "Confirm"
}
```

### Client → Server: `UserAction`

When the user interacts with the UI, the frontend sends one of:

#### Text Action

```json
{
  "type": "text",
  "content": "User's message"
}
```

#### Button Action

```json
{
  "type": "button",
  "id": "confirm"
}
```

#### Photo Action

```json
{
  "type": "photo",
  "data": "base64-encoded-image-data",
  "mimeType": "image/jpeg"
}
```

## Lifecycle

### Connection Establishment

1. Frontend opens WebSocket connection to `ws://localhost:{PORT}`
2. Server accepts connection, initializes graph session
3. Server sends initial `UIState` with greeting/initial prompt
4. Frontend renders state

### Normal Flow

1. User interacts → Frontend sends `UserAction`
2. Server receives action, feeds to graph
3. Server sets `thinking: true`, sends `UIState`
4. Server executes graph step (may include LLM calls)
5. Server sets `thinking: false`, sends updated `UIState`
6. Frontend renders new state
7. Repeat

### Affordance Visibility

The server controls which input affordances are visible:

- **Text input only**: `textInput` non-null, others null
- **Photo upload**: `photoUpload` non-null (may also have `textInput`)
- **Buttons only**: `buttons` non-null, `textInput` null
- **Mixed**: Multiple affordances visible (e.g., text + buttons)
- **None** (thinking): All null, `thinking: true`

The frontend must not show affordances for null fields and should disable interaction while `thinking: true`.

### Error Handling

On WebSocket errors:

1. Frontend shows reconnection UI
2. Attempts reconnection with exponential backoff
3. On reconnect, server may send current state or require fresh session

The server may send an error state:

```json
{
  "messages": [...],
  "textInput": null,
  "photoUpload": null,
  "buttons": [
    {"id": "retry", "label": "Try Again"},
    {"id": "reset", "label": "Start Over"}
  ],
  "graphNode": "error",
  "thinking": false
}
```

### Session Termination

Either party may close the connection:

- **Client closes**: Server cleans up session
- **Server closes**: Client shows disconnection UI

## State Ownership

**Important**: The server owns all state. The frontend is a pure render layer.

- Frontend does NOT maintain conversation history
- Frontend does NOT track graph position
- Frontend receives complete state on each update
- Frontend renders what it receives

This simplifies the frontend and enables server-side session recovery.

## Message Ordering

Messages are processed in order of receipt. The server will not send a new `UIState` until the previous graph step completes. The frontend should:

1. Disable input while `thinking: true`
2. Queue user actions during thinking (optional, may also reject)
3. Handle rapid state updates gracefully

## Example Session

```
→ Client connects
← Server: UIState (greeting, textInput enabled)
→ Client: TextAction "I need to tidy my desk"
← Server: UIState (thinking: true)
← Server: UIState (assistant message, photoUpload enabled)
→ Client: PhotoAction (base64 image)
← Server: UIState (thinking: true)
← Server: UIState (assistant with analysis, buttons for disposal options)
→ Client: ButtonAction "trash"
← Server: UIState (thinking: true)
← Server: UIState (confirmation, next item prompt)
...
```

## Versioning

This is version 1 of the protocol. Future versions may add:

- `protocolVersion` field for negotiation
- Additional affordance types
- Streaming message support
- Binary message support for images

Breaking changes will increment the major version.
