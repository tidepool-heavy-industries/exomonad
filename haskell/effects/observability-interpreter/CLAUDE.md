# Observability Interpreter - Grafana Loki & Tempo Integration

Interprets the `Observability` effect by pushing logs to Loki and traces to Tempo via OTLP HTTP.

## When to Read This

Read this if you're:
- Setting up agent observability
- Debugging missing traces in Grafana
- Understanding how agent events are logged
- Configuring Grafana Cloud integration

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ Agent Effects                                                        │
│   publishEvent (GraphTransition "entry" "classify" "user_input")    │
│   withSpan "llm:call" SpanClient [...] $ complete ...               │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ Observability effect
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ Observability Interpreter                                               │
│   ├─ pushToLoki (structured JSON logs)                              │
│   └─ pushToOTLP (OpenTelemetry spans)                               │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ HTTP
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ Grafana Cloud                                                        │
│   ├─ Loki (logs)  → query via LogQL                                 │
│   └─ Tempo (traces) → query via TraceQL                             │
└─────────────────────────────────────────────────────────────────────┘
```

## Usage

```haskell
import ExoMonad.Effects.Observability
import ExoMonad.Observability.Interpreter

main :: IO ()
main = do
  let config = grafanaCloudConfig
        { lcUser = "your-user-id"
        , lcToken = "glc_..."
        }
  ctx <- newTraceContext
  runM $ runObservabilityWithContext ctx config $ do
    -- Structured events → Loki
    publishEvent $ GraphTransition "entry" "classify" "user_input"
    publishEvent $ LLMCallEvent "claude-3" 100 50 250

    -- Traced spans → Tempo
    withSpan "graph:example" SpanServer [] $ do
      withSpan "llm:call" SpanClient [("model", "claude-3")] $ do
        -- LLM call here
        pure ()
```

## Event Types

Events published to Loki as structured JSON:

| Event | Fields | Purpose |
|-------|--------|---------|
| `GraphTransition` | `from`, `to`, `trigger` | Node-to-node moves |
| `LLMCallEvent` | `model`, `prompt_tokens`, `completion_tokens`, `latency_ms` | LLM API calls |
| `UserActionEvent` | `action_type`, `node_context` | User interactions |
| `EffectExecutionEvent` | `effect_type`, `success`, `latency_ms` | Effect timing |
| `ErrorEvent` | `message`, `context` | Failures |

## Span Types

OpenTelemetry spans for distributed tracing:

```haskell
data SpanKind
  = SpanServer    -- Incoming request (graph entry)
  | SpanClient    -- Outgoing call (LLM, subprocess)
  | SpanInternal  -- Internal processing
  | SpanProducer  -- Async message send
  | SpanConsumer  -- Async message receive
```

## Configuration

### Local Development

```haskell
defaultLokiConfig :: LokiConfig
defaultLokiConfig = LokiConfig
  { lcEndpoint = "http://localhost:3100"
  , lcUser = Nothing
  , lcToken = Nothing
  }
```

### Grafana Cloud

```haskell
grafanaCloudConfig :: LokiConfig
grafanaCloudConfig = LokiConfig
  { lcEndpoint = "https://logs-prod-us-central1.grafana.net"
  , lcUser = Just "your-user-id"
  , lcToken = Just "glc_..."
  }
```

### Environment Variables

```bash
export GRAFANA_LOKI_URL="https://logs-prod-us-central1.grafana.net"
export GRAFANA_LOKI_USER="123456"
export GRAFANA_LOKI_TOKEN="glc_..."

export OTLP_ENDPOINT="https://otlp-gateway-prod-us-central-0.grafana.net/otlp"
export OTLP_USER="123456"
export OTLP_TOKEN="glc_..."
```

## LLM Tracing via Interposition

Transparently trace LLM calls without modifying effect stack:

```haskell
import ExoMonad.Observability.Interpreter (interposeWithLLMTracing)

-- Wrap any LLM computation with automatic tracing
tracedProgram = interposeWithLLMTracing myProgram

-- LLM calls now automatically emit spans to Tempo
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `Interpreter.hs` | Effect interpreter, HTTP clients |
| `Types.hs` | Configuration types, TraceContext |

## Querying in Grafana

### Loki (Logs)

```logql
# All graph transitions
{app="exomonad"} |= "graph_transition" | json

# LLM calls with high latency
{app="exomonad"} |= "llm_call" | json | latency_ms > 2000

# Errors in specific session
{app="exomonad", session_id="abc123"} |= "error" | json
```

### Tempo (Traces)

```traceql
# All spans for a session
{ resource.service.name = "exomonad" && span.session_id = "abc123" }

# Slow LLM calls
{ name = "llm:call" && duration > 3s }
```

## Related Documentation

- [tools/sleeptime/CLAUDE.md](../../tools/sleeptime/CLAUDE.md) - CLI for querying these logs
- [effects/CLAUDE.md](../CLAUDE.md) - Effect interpreter pattern
- [haskell/dsl/core/CLAUDE.md](../../dsl/core/CLAUDE.md) - Graph execution that emits events
