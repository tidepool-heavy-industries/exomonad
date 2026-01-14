# Native Tidepool Server

Servant + WebSocket server for running tidepool agents natively (without WASM/CF Workers).

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Native Server                             │
│                                                              │
│  ┌──────────────┐    ┌─────────────────────────────────────┐│
│  │ REST API     │    │ WebSocket Handler                   ││
│  │ /health      │    │                                     ││
│  │ /sessions    │    │  Session ──► UICallback ──► Agent   ││
│  └──────────────┘    │      ▲            │                 ││
│                      │      │            ▼                 ││
│                      │   TVars ◄── runEffects ──► IO       ││
│                      └─────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────┘
```

## Effect Stack

The server composes these effect interpreters via `runEffects`:

| Effect | Interpreter | Purpose |
|--------|----------|---------|
| UI | ui-interpreter | WebSocket ↔ UIState/UserAction bridging |
| Habitica | habitica-interpreter | Habitica API calls (optional) |
| LLMComplete | llm-interpreter | Anthropic/OpenAI API calls |
| Session | session-interpreter | Dockerized Claude Code sessions via mantle |
| Observability | observability-interpreter | Loki logs + OTLP traces |

Composition order (EffectRunner.hs):

```haskell
runEffects :: InterpreterEnv -> UIContext -> UICallback
           -> Eff '[UI, Habitica, LLMComplete, Session, Observability, IO] a
           -> IO a
runEffects env ctx callback action = do
  traceCtx <- newTraceContext
  result <- runM
    . runObservabilityWithContext traceCtx (ecLokiConfig $ eeConfig env)
    . runSessionIO (ecSessionConfig $ eeConfig env)
    . runLLMComplete (eeLLMEnv env)
    . runHabitica (eeHabiticaEnv env)
    . runUI ctx callback
    $ action
  -- Flush traces to OTLP if configured
  case ecOTLPConfig (eeConfig env) of
    Just otlpConfig -> flushTraces otlpConfig (ecServiceName $ eeConfig env) traceCtx
    Nothing -> pure ()
  pure result
```

Effects are peeled from the outside in:
1. **UI** (first) - handles user interaction via WebSocket
2. **Habitica** - makes Habitica API calls
3. **LLMComplete** - makes LLM API calls
4. **Session** - executes ClaudeCode nodes via dockerized mantle sessions
5. **Observability** (last) - records events to Loki, spans to Tempo

## Running

```bash
just native
# Or directly:
TIDEPOOL_DIST=tidepool-native-gui/solid-frontend/dist cabal run tidepool-native
```

**Environment variables:**

| Variable | Required | Purpose |
|----------|----------|---------|
| `ANTHROPIC_API_KEY` | Yes (for LLM) | Anthropic Claude API key |
| `OPENAI_API_KEY` | Alternative | OpenAI API key |
| `HABITICA_USER_ID` | No | Habitica user ID |
| `HABITICA_API_TOKEN` | No | Habitica API token |
| `LOKI_URL` | No | Loki push endpoint (default: localhost:3100) |
| `LOKI_USER` | No | Grafana Cloud user ID for Loki |
| `LOKI_TOKEN` | No | Grafana Cloud API token for Loki |
| `OTLP_ENDPOINT` | No | OTLP traces endpoint (enables tracing) |
| `OTLP_USER` | No | Grafana Cloud user ID for Tempo |
| `OTLP_TOKEN` | No | Grafana Cloud API token for Tempo |
| `SERVICE_NAME` | No | Service name for traces (default: tidepool-native) |

## REST Endpoints

| Endpoint | Response |
|----------|----------|
| `GET /health` | `{"status":"ok","version":"0.1.0"}` |
| `GET /sessions` | List of active WebSocket sessions |
| `GET /sessions/:id` | Session details by UUID |

## WebSocket Protocol

Connect to `ws://localhost:8080/` for agent communication.

**Server → Client**: `UIState` (JSON)
```json
{
  "messages": [{"role": "assistant", "content": "..."}],
  "textInput": {"placeholder": "You:"},
  "graphNode": "chat"
}
```

**Client → Server**: `UserAction` (JSON)
```json
{"textAction": "Hello, how are you?"}
```

## Wiring an Agent

Agents must use effects from the stack. Example:

```haskell
myAgent :: (Member UI effs, Member LLMComplete effs, Member Observability effs)
        => Eff effs ()
myAgent = do
  publishEvent $ GraphTransition "entry" "greeting" "start"
  showText "Hello!"
  forever $ do
    input <- requestTextInput "You:"
    response <- complete SAnthropic config input Nothing
    showText (extractText response)
```

Wire in `Server.hs:handleConnection`:

```haskell
runEffects env ctx callback myAgent
```

## Session Management

Each WebSocket connection creates a `Session` stored in an STM `SessionMap`:

```haskell
data Session = Session
  { sId        :: UUID
  , sActionVar :: TVar (Maybe UserAction)  -- incoming actions
  , sStateVar  :: TVar (Maybe UIState)     -- outgoing state
  , sCreatedAt :: UTCTime
  , sGraphNode :: TVar Text                -- current graph node
  }
```

Sessions are automatically cleaned up on disconnect.

**TODO**: Persist sessions to SQLite for durability across restarts.

## Files

| File | Purpose |
|------|---------|
| `Server.hs` | Main server, WebSocket handler, REST handlers |
| `EffectRunner.hs` | Effect composition (`runEffects`) |
| `Session.hs` | STM-based session management |
| `SimpleAgent.hs` | Demo agent using full effect stack |
| `API.hs` | REST endpoint type definitions |
