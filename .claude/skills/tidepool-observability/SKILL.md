---
name: tidepool-observability
description: Use when setting up Grafana Cloud observability for Cloudflare Workers, or debugging trace/log export issues.
---

# Tidepool Observability

Grafana Cloud native integration for Cloudflare Workers (works on any plan).

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
