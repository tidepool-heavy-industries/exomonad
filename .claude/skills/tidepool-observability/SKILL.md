---
name: tidepool-observability
description: Use when setting up Grafana Cloud observability for Cloudflare Workers, or debugging trace/log export issues.
---

# Tidepool Observability

Grafana Cloud integration for both Cloudflare Workers and native server.

## Quick Setup

1. **Grafana Cloud Integration Page**:
   https://inannamalick.grafana.net/connections/add-new-connection/cloudflare-workers

2. **Enable in wrangler.toml**:
   ```toml
   [observability.traces]
   enabled = true
   destinations = [ "grafana-traces" ]

   [observability.logs]
   enabled = true
   destinations = [ "grafana-logs" ]
   ```

3. **Deploy**:
   ```bash
   cd deploy
   pnpm deploy
   ```

## What Gets Exported

**Traces**:
- Durable Object invocations
- Fetch requests
- Nested spans for effect execution

**Logs**:
- `console.log/error/warn` statements
- Structured logs from effect handlers
- Exception traces

## Querying Logs (LogQL)

```logql
# All tidepool-worker logs
{service_name="tidepool-worker"}

# Logs from a specific graph
{service_name="tidepool-worker"} | json | graphId="example-graph"

# Error logs
{service_name="tidepool-worker"} | json | level="error"

# Specific effect type
{service_name="tidepool-worker"} | json | effectType="LlmComplete"

# Time range with count
count_over_time({service_name="tidepool-worker"}[5m])
```

## Structured Logging Pattern

From `deploy/src/structured-log.ts`:

```typescript
import { logStructured, LogLevel } from './structured-log';

logStructured(LogLevel.INFO, 'Effect executing', {
  effectType: effect.type,
  node: effect.eff_node,
  sessionId
});
```

**Available levels**: DEBUG, INFO, WARN, ERROR

## Trace Correlation

Logs include trace context automatically:
- `trace_id` - Links logs to traces
- `span_id` - Links to specific span
- `service_name` - Always "tidepool-worker"

## Common Issues

1. **No logs appearing** - Check `observability.logs.enabled = true` in wrangler.toml
2. **No traces** - Check `observability.traces.enabled = true`
3. **Wrong service name** - Should be "tidepool-worker" (from package.json name)
4. **Rate limits** - Grafana Cloud free tier has limits, check quota

## Grafana Stack

Stack: `inannamalick.grafana.net`

Access:
- **Logs**: Explore → Loki data source
- **Traces**: Explore → Tempo data source
- **Dashboards**: Create from queries above

## Documentation

Full setup: `deploy/docs/GRAFANA_CLOUD_SETUP.md`
Query examples: `deploy/docs/GRAFANA_QUERIES.md`

---

# Native Server Traces (OTLP)

The native server (`tidepool-native`) can export OpenTelemetry traces to Grafana Tempo via OTLP HTTP.

## Quick Setup

1. **Set environment variables**:
   ```bash
   # Required: OTLP endpoint for Grafana Tempo
   export OTLP_ENDPOINT="https://otlp-gateway-prod-us-west-0.grafana.net/otlp/v1/traces"

   # Required: Grafana Cloud credentials
   export OTLP_USER="123456"  # Your Grafana Cloud instance ID
   export OTLP_TOKEN="glc_..."  # API token with push scope

   # Optional: Service name (default: tidepool-native)
   export SERVICE_NAME="my-agent"
   ```

2. **Run the server**:
   ```bash
   just native
   # Or: cabal run tidepool-native
   ```

3. **View traces in Grafana**:
   - Go to Explore → Tempo
   - Search by service.name = "tidepool-native" (or your custom SERVICE_NAME)

## Instrumented Graph Execution

Graph handlers emit spans automatically when using instrumented dispatch:

```haskell
import Tidepool.Graph.Execute.Instrumented

-- Wrap graph execution with a root span
result <- withGraphSpan "agent:my-request" $
  runGraphWithSpans handlers inputValue

-- Each node transition creates a child span:
-- agent:my-request
-- └── node:classify
-- └── node:route
-- └── node:respond
```

## Span Hierarchy

```
graph:MyGraph (root span, SpanServer)
├── node:entry (first handler)
├── node:process (subsequent handlers)
└── node:exit (final handler)
```

Each node span includes attributes:
- `node.name` - Handler field name
- `node.type` - "logic" or "llm"

## Manual Spans

For custom spans within handlers:

```haskell
import Tidepool.Effects.Observability

myHandler input = do
  _ <- startSpan "custom-operation" SpanInternal
    [AttrText "input.type" (typeof input)]

  result <- doWork input

  endSpan False [AttrInt "result.count" (length result)]
  pure result
```

## Environment Variables

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `OTLP_ENDPOINT` | For traces | none | OTLP HTTP traces endpoint |
| `OTLP_USER` | For Grafana Cloud | none | Instance ID for basic auth |
| `OTLP_TOKEN` | For Grafana Cloud | none | API token for basic auth |
| `SERVICE_NAME` | No | tidepool-native | Service name in traces |
| `LOKI_URL` | For logs | localhost:3100 | Loki push endpoint |
| `LOKI_USER` | For Grafana Cloud | none | Instance ID for Loki |
| `LOKI_TOKEN` | For Grafana Cloud | none | API token for Loki |

## Getting Grafana Cloud Credentials

1. Go to: https://grafana.com/orgs/<your-org>/api-keys
2. Create a new API key with "Push" scope
3. Copy the instance ID from the OTLP gateway URL (the number in the URL)

## Querying Traces (TraceQL)

```traceql
# All traces from native server
{service.name="tidepool-native"}

# Traces with specific node
{service.name="tidepool-native"} | node.name="classify"

# Error traces
{service.name="tidepool-native" && status=error}
```
