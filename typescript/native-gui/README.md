# ExoMonad Native GUI

Native Haskell build with localhost GUI for fast iteration on exomonad graphs. Bypasses CF Workers deployment, uses Anthropic API directly. Full observability via Grafana.

## Structure

```
exomonad-native-gui/
├── wire-types/               # Agent 3 - UIState, UserAction, protocol
│   ├── src/ExoMonad/Wire/Types.hs
│   └── exomonad-wire-types.cabal
├── solid-frontend/           # Agent 2 - SolidJS frontend
│   ├── src/App.tsx
│   ├── src/components/
│   ├── src/lib/socket.ts
│   └── package.json
├── ui-interpreter/              # Agent 4 - interpreter for UI effects
│   ├── src/ExoMonad/UI/Interpreter.hs
│   └── exomonad-ui-interpreter.cabal
├── habitica-interpreter/        # Agent 5 - native HTTP client
│   ├── src/ExoMonad/Habitica/Interpreter.hs
│   └── exomonad-habitica-interpreter.cabal
├── observability-interpreter/   # Agent 6 - Loki push
│   ├── src/ExoMonad/Observability/Interpreter.hs
│   └── exomonad-observability-interpreter.cabal
├── llm-interpreter/             # Agent 7 - Anthropic native client
│   ├── src/ExoMonad/LLM/Interpreter.hs
│   └── exomonad-llm-interpreter.cabal
└── server/                   # Agent 1 - ties it together
    ├── app/Main.hs
    ├── src/ExoMonad/Server.hs
    ├── src/ExoMonad/Server/EffectRunner.hs
    └── exomonad-native-server.cabal
```

## Key Principle

Effect *types* live in `exomonad-core/src/ExoMonad/Effects/`, shared by WASM and native. Effect *interpreters* are platform-specific and live here.

## Dependencies

```
wire-types (Agent 3) ──────► ui-interpreter (Agent 4)
                       ├───► server (Agent 1)
                       └───► solid-frontend (Agent 2)

habitica-interpreter (Agent 5) ───► server
observability-interpreter (Agent 6) ───► server
llm-interpreter (Agent 7) ───► server
```

## Effect Types (in exomonad-core)

- `ExoMonad.Effects.UI` - ShowText, RequestTextInput, RequestPhotoInput, RequestChoice, SetThinking
- `ExoMonad.Effects.Observability` - PublishEvent, WithSpan + ExoMonadEvent types
- `ExoMonad.Effects.LLMProvider` - LLMComplete with type-level Anthropic/OpenAI switch
- `ExoMonad.Effects.Issue` - FileIssue for resident-initiated GitHub issues

## Building

```bash
# Haskell packages (from repo root)
cabal build all

# Solid frontend
cd exomonad-native-gui/solid-frontend
npm install
npm run build     # Production build
npm run dev       # Dev server on port 3000
```

Set `VITE_WS_URL` to override the WebSocket URL (default: `ws://localhost:8080`).

## Status

**All components implemented and building:**

- Agent 1 (server): Complete - WebSocket server with effect composition
- Agent 2 (solid-frontend): Complete - SolidJS + Tailwind CSS frontend with WebSocket, chat rendering, text/photo input, button groups
- Agent 3 (wire-types): Complete - UIState, UserAction, protocol types
- Agent 4 (ui-interpreter): Complete - UI effect interpreter with wire bridging
- Agent 5 (habitica-interpreter): Complete - Native Habitica HTTP client
- Agent 6 (observability-interpreter): Complete - Loki push API client
- Agent 7 (llm-interpreter): Complete - Anthropic/OpenAI native HTTP clients
