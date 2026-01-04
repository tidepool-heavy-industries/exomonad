# Tidepool Native GUI

Native Haskell build with localhost GUI for fast iteration on tidepool graphs. Bypasses CF Workers deployment, uses Anthropic API directly. Full observability via Grafana.

## Structure

```
tidepool-native-gui/
├── wire-types/               # Agent 3 - UIState, UserAction, protocol
│   ├── src/Tidepool/Wire/Types.hs
│   └── tidepool-wire-types.cabal
├── ui-executor/              # Agent 4 - interpreter for UI effects
│   ├── src/Tidepool/UI/Executor.hs
│   └── tidepool-ui-executor.cabal
├── habitica-executor/        # Agent 5 - native HTTP client
│   ├── src/Tidepool/Habitica/Executor.hs
│   └── tidepool-habitica-executor.cabal
├── observability-executor/   # Agent 6 - Loki push
│   ├── src/Tidepool/Observability/Executor.hs
│   └── tidepool-observability-executor.cabal
├── llm-executor/             # Agent 7 - Anthropic native client
│   ├── src/Tidepool/LLM/Executor.hs
│   └── tidepool-llm-executor.cabal
└── server/                   # Agent 1 - ties it together
    ├── app/Main.hs
    ├── src/Tidepool/Server.hs
    ├── src/Tidepool/Server/EffectRunner.hs
    └── tidepool-native-server.cabal
```

## Key Principle

Effect *types* live in `tidepool-core/src/Tidepool/Effects/`, shared by WASM and native. Effect *executors* are platform-specific and live here.

## Dependencies

```
wire-types (Agent 3) ──────► ui-executor (Agent 4)
                       ├───► server (Agent 1)
                       └───► [Solid frontend, Agent 2]

habitica-executor (Agent 5) ───► server
observability-executor (Agent 6) ───► server
llm-executor (Agent 7) ───► server
```

## Effect Types (in tidepool-core)

- `Tidepool.Effects.UI` - ShowText, RequestTextInput, RequestPhotoInput, RequestChoice, SetThinking
- `Tidepool.Effects.Observability` - PublishEvent, WithSpan + TidepoolEvent types
- `Tidepool.Effects.LLMProvider` - LLMComplete with type-level Anthropic/OpenAI switch
- `Tidepool.Effects.Issue` - FileIssue for resident-initiated GitHub issues

## Building

```bash
# From repo root
cabal build all
```

## Status

**All effect executors implemented and building:**

- Agent 3 (wire-types): Complete - UIState, UserAction, protocol types
- Agent 4 (ui-executor): Complete - UI effect interpreter with wire bridging
- Agent 5 (habitica-executor): Complete - Native Habitica HTTP client
- Agent 6 (observability-executor): Complete - Loki push API client
- Agent 7 (llm-executor): Complete - Anthropic/OpenAI native HTTP clients
- Agent 1 (server): Complete - Effect runner with SSE streaming
