# Grafana Cloud Setup for ExoMonad

This guide configures Cloudflare Workers to export OpenTelemetry traces and logs to Grafana Cloud for historical query and analysis.

## Why Grafana Cloud?

- **Native CF Workers integration** - OTLP export built into the runtime, zero code changes
- **Works on any plan** - No Enterprise required (unlike Logpush)
- **Forever free tier** - Generous limits for personal projects
- **Loki + Tempo** - Logs and distributed traces in one place
- **Automatic instrumentation** - Subrequests, KV calls, console.log() all captured

## Prerequisites

1. Cloudflare Workers account (any plan - Free, Paid, or Enterprise)
2. Grafana Cloud account ([sign up for free tier](https://grafana.com/auth/sign-up/create-user))

## inannamalick Stack Configuration

**Stack URL**: `https://inannamalick.grafana.net`
**Region**: `us-west-0`

**OTLP Gateway**:
```
https://otlp-gateway-prod-us-west-0.grafana.net/otlp
```

**Datasource UIDs** (for queries/dashboards):
| Type | UID | Direct URL |
|------|-----|------------|
| Loki (logs) | `grafanacloud-logs` | `https://logs-prod-021.grafana.net` |
| Tempo (traces) | `grafanacloud-traces` | `https://tempo-prod-15-prod-us-west-0.grafana.net/tempo` |
| Prometheus | `grafanacloud-prom` | `https://prometheus-prod-36-prod-us-west-0.grafana.net/api/prom` |

**Cloudflare Destination Endpoints**:
- Traces: `https://otlp-gateway-prod-us-west-0.grafana.net/otlp/v1/traces`
- Logs: `https://otlp-gateway-prod-us-west-0.grafana.net/otlp/v1/logs`

## Setup Steps

### 1. Configure Cloudflare Workers Integration

The easiest way to set this up is via Grafana Cloud's Cloudflare Workers integration page:

**For inannamalick stack**: https://inannamalick.grafana.net/connections/add-new-connection/cloudflare-workers

**For other stacks**: Go to your Grafana Cloud → **Connections** → **Add new connection** → search "Cloudflare Workers"

This integration page will:
- Provide your OTLP endpoint and authentication token
- Walk you through creating destinations in Cloudflare Dashboard
- Give you the exact `wrangler.toml` configuration

**Destination names to create:**
- `grafana-traces` (for traces)
- `grafana-logs` (for logs)

### 2. Enable Telemetry Export in wrangler.toml

Configure the observability section in `deploy/wrangler.toml`:

```toml
[observability.traces]
enabled = true
destinations = [ "grafana-traces" ]

[observability.logs]
enabled = true
destinations = [ "grafana-logs" ]
```

**Important**: The destination names must match exactly what you created in the CF Dashboard.

### 3. Deploy

```bash
cd deploy
pnpm deploy
```

Telemetry will start flowing immediately. **Note**: It may take 2-5 minutes for data to appear in Grafana.

## Verifying It Works

### Check Traces in Tempo

1. In Grafana Cloud, go to **Explore**
2. Select **Tempo** data source
3. Click **Search** and look for recent traces
4. You should see traces for Worker invocations with:
   - Subrequest spans (fetch to external APIs)
   - KV operation spans
   - Custom spans from your code

### Check Logs in Loki

1. In Grafana Cloud, go to **Explore**
2. Select **Loki** data source
3. Run a query:
   ```logql
   {service_name="exomonad"} | json
   ```
4. You should see structured logs from `console.log(JSON.stringify(...))` in your Worker

## What Gets Logged

With the structured logging in `src/structured-log.ts`, you'll see:

### Effect Execution Logs
- **session_id**: Durable Object ID
- **graph_id**: Which graph is running (e.g., "habitica-helper")
- **effect_type**: LlmComplete, Habitica, etc.
- **latency_ms**: How long the effect took
- **result_type**: success or error

### Graph Context
- **node_name**: Which node executed the effect
- **node_type**: llm, logic, entry, exit
- **turn_number**: 0-indexed turn counter

### LLM Metrics
- **llm_model**: Model used
- **llm_prompt_tokens**: Input tokens
- **llm_completion_tokens**: Output tokens
- **llm_cache_hit**: Whether prompt cache was hit

### Habitica-Specific
- **habitica_operation**: GetTasks, ScoreTask, CreateTask, etc.
- **habitica_item_id**: Task ID if applicable

## Example Queries

See [GRAFANA_QUERIES.md](GRAFANA_QUERIES.md) for LogQL query examples.

## Pricing

**Cloudflare:**
- OpenTelemetry export is **free during beta** (current)
- Starting **January 15, 2026**, tracing will be billed as part of Workers Paid plan usage
- Requires Workers Paid plan to export to external destinations

**Grafana Cloud:**
- **Forever free tier**: 50 GB logs, 50 GB traces, 10k series metrics per month
- Auto-expires old data after 14 days (configurable)
- Paid tiers available if you exceed free limits

## Troubleshooting

### No data appearing in Grafana

1. **Wait 5 minutes** - Initial telemetry can take time to process
2. **Check destination config** - Verify Authorization header is correct
3. **Check wrangler.toml** - Destination names must match exactly
4. **Redeploy Worker** - `pnpm deploy` after config changes
5. **Check Cloudflare logs** - `wrangler tail` to see if Worker is running

### Traces but no logs

- Verify you're using `console.log(JSON.stringify({...}))` format
- Check Loki query syntax: `{service_name="exomonad"}` should show all logs

### Logs but no traces

- Traces require subrequests or manual span creation
- Check Tempo for trace IDs in your logs

## References

- [Cloudflare: Export to Grafana Cloud](https://developers.cloudflare.com/workers/observability/exporting-opentelemetry-data/grafana-cloud/)
- [Grafana: CF Workers Integration](https://grafana.com/docs/grafana-cloud/monitor-infrastructure/integrations/integration-reference/integration-cloudflare-workers/)
- [Grafana Blog: Send OTLP from CF Workers](https://grafana.com/blog/2025/12/04/send-opentelemetry-traces-and-logs-from-cloudflare-workers-to-grafana-cloud/)
