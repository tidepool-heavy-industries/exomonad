# Anemone - Solid.js Frontend

Debug/diagnostic frontend for Tidepool agents. Connects to either:
- Cloudflare Worker (`deploy/`) via WebSocket
- Native server (`tidepool-native-gui/server/`) via WebSocket

**Note**: This is the frontend UI, not the consuming repo at `~/dev/anemone` which contains actual agent definitions.

## Tech Stack

- **Solid.js** - Fine-grained reactivity for real-time WebSocket updates
- **TypeScript** - Full type safety
- **Vite** - Fast dev server with HMR
- **Tailwind CSS v4** - Utility-first styling with CSS custom properties

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                         Anemone Frontend                            │
│                                                                     │
│  ┌───────────────┐  ┌─────────────────────┐  ┌──────────────────┐  │
│  │  Debug Pane   │  │    Chat Pane        │  │  History Pane    │  │
│  │               │  │                     │  │                  │  │
│  │ - Phase       │  │ - Message list      │  │ - Event timeline │  │
│  │ - Nodes       │  │ - Chat input        │  │ - Filters        │  │
│  │ - Effect      │  │ - Connection status │  │ - Expandable     │  │
│  │ - Logs        │  │                     │  │                  │  │
│  └───────────────┘  └─────────────────────┘  └──────────────────┘  │
│                              │                                      │
│              ┌───────────────┴───────────────┐                      │
│              │         Solid Stores           │                      │
│              │ connection | graph | chat | events                   │
│              └───────────────┬───────────────┘                      │
│                              │                                      │
│              ┌───────────────┴───────────────┐                      │
│              │      WebSocket Service        │                      │
│              │  GraphWebSocket (reconnects)  │                      │
│              └───────────────┬───────────────┘                      │
└──────────────────────────────┼──────────────────────────────────────┘
                               │
                               ▼ ws://localhost:8787/session/:id
┌──────────────────────────────────────────────────────────────────────┐
│                  deploy/ Cloudflare Worker                           │
└──────────────────────────────────────────────────────────────────────┘
```

## Project Structure

```
anemone/
├── src/
│   ├── index.tsx              # Entry point
│   ├── index.css              # Tailwind + theme tokens
│   ├── App.tsx                # Root with providers
│   │
│   ├── types/
│   │   ├── protocol.ts        # Wire types (matches deploy/)
│   │   └── events.ts          # UI-specific types
│   │
│   ├── services/
│   │   └── websocket.ts       # GraphWebSocket class
│   │
│   ├── stores/
│   │   ├── connectionStore.tsx  # WS connection state
│   │   ├── graphStore.tsx       # Execution phase, nodes
│   │   ├── chatStore.tsx        # Messages
│   │   └── eventStore.tsx       # History timeline
│   │
│   ├── hooks/
│   │   ├── useWebSocket.ts    # Connection + message handling
│   │   ├── useAutoScroll.ts   # Auto-scroll to bottom
│   │   └── useEventFilter.ts  # Filter timeline events
│   │
│   └── components/
│       ├── layout/            # AppLayout, panes
│       ├── debug/             # Phase, NodeList, ActiveEffect, LogStream
│       ├── chat/              # MessageList, ChatInput, ConnectionStatus
│       ├── history/           # EventTimeline, EventItem, EventFilter
│       └── shared/            # Badge, Spinner, JsonViewer
│
├── vite.config.ts             # Dev proxy to Worker
└── index.html
```

## Running

```bash
# Install dependencies
pnpm install

# Development (with HMR)
pnpm dev                    # Runs on localhost:5173

# In another terminal, run the Worker
cd ../deploy && pnpm dev    # Runs on localhost:8787

# Production build
pnpm build                  # Outputs to dist/
```

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `VITE_API_URL` | `/api` | Base URL for the Worker API |

**Development:** Uses Vite proxy, no env var needed.

**Production:** Create `.env.production`:
```bash
VITE_API_URL=https://tidepool.your-subdomain.workers.dev
```

## Vite Proxy Configuration

In development, Vite proxies requests to the Worker:

```ts
proxy: {
  "/api": {
    target: "http://localhost:8787",
    rewrite: (path) => path.replace(/^\/api/, ""),
  },
  "/ws": {
    target: "ws://localhost:8787",
    ws: true,
    rewrite: (path) => path.replace(/^\/ws/, ""),
  },
}
```

The `useWebSocket` hook handles URL construction:
- **Dev mode**: HTTP via `/api/*`, WebSocket via `/ws/*`
- **Production**: Full URLs with `https://` → `wss://` conversion

## WebSocket Protocol

Anemone connects to the Worker via WebSocket and exchanges these messages:

**Client → Server:**
- `{ type: "init", input: { messageText: "..." } }` - Start graph
- `{ type: "resume", result: { type: "success", value: ... } }` - Resume after effect

**Server → Client:**
- `{ type: "progress", node: "...", effect: "LlmComplete" }` - Node executing
- `{ type: "suspend", effect: { type: "LlmComplete", ... } }` - Awaiting effect
- `{ type: "done", result: { responseText: "..." } }` - Graph finished
- `{ type: "error", message: "..." }` - Execution failed

## Known Limitations

1. **NodeList shows "No graph info available"**
   - The Worker's WASM machine has `getGraphInfo()` but this isn't exposed via WebSocket
   - Until the Worker protocol adds a `graphInfo` message type, the node list won't populate
   - The UI will still show execution phase and active effects

2. **useWebSocket singleton pattern**
   - Only one component should use `useWebSocket()` at a time
   - Currently only `ChatInput` uses it - this is intentional

## Theme

Dark theme with noir palette, defined in `src/index.css`:

```css
--color-bg-primary: #0d0d0d      /* Main background */
--color-bg-secondary: #1a1a1a    /* Panel backgrounds */
--color-accent: #c9a227          /* Gold highlights */
--color-phase-active: #3b82f6    /* Blue - executing */
--color-phase-completed: #22c55e /* Green - done */
--color-phase-failed: #ef4444    /* Red - error */
```

## Key Patterns

### Solid Stores

State is managed via context + createStore:

```tsx
const [state, setState] = createStore<ChatStoreState>({
  messages: [],
  isLoading: false,
});

// Update with produce for immutability
setState(produce((s) => {
  s.messages.push(newMessage);
}));
```

### Auto-Scroll Hook

Scrolls to bottom when new items appear (only if already at bottom):

```tsx
const { scrollToBottom, checkScroll } = useAutoScroll(() => containerRef);

createEffect(() => {
  items().length; // React to changes
  scrollToBottom();
});
```

### WebSocket with Auto-Reconnect

GraphWebSocket handles connection lifecycle with exponential backoff:

```tsx
const ws = new GraphWebSocket({
  onProgress(node, effect) { /* update state */ },
  onDone(result) { /* show result */ },
  onServerError(msg) { /* show error */ },
});
await ws.connect(url);
ws.init({ messageText: input });
```
