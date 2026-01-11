# Sleeptime Logs - Agent Evolution Analytics

CLI tool for querying agent logs from Grafana/Loki to support the sleeptime evolution workflow.

## When to Read This

Read this if you're:
- Analyzing agent run patterns for improvement
- Debugging production agent failures
- Understanding the sleeptime evolution workflow
- Building automation that queries agent logs

**Note**: For full usage details, see `README.md` in this directory.

## What is Sleeptime?

Sleeptime is the evolution pattern for Tidepool agents:

```
1. Agents run in production → logs to Loki
2. Sleeptime analyzes logs → identifies patterns
3. Patterns drive improvements → PRs filed
4. Deploy + observe → repeat
```

This CLI is the query layer for step 2.

## Architecture

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│  Agent Runtime  │────▶│  Grafana/Loki   │◀────│  sleeptime-logs │
│  (observability)│     │  (log storage)  │     │  (this CLI)     │
└─────────────────┘     └─────────────────┘     └─────────────────┘
                              │
                              ▼
                        LogQL queries
```

## Key Commands

```bash
# Graph transitions
sleeptime-logs transitions --since 24h

# Slow LLM calls
sleeptime-logs llm-calls --min-latency 2000 --since 1h

# Session debugging
sleeptime-logs session abc123-session-id

# Errors
sleeptime-logs errors --since 1h
```

## Event Types

| Event | Purpose | Example Query |
|-------|---------|---------------|
| `graph_transition` | Node-to-node moves | Which transitions fail? |
| `llm_call` | LLM API timing | Which calls are slow? |
| `user_action` | User interactions | What do users do most? |
| `effect_execution` | Effect timing | Which effects are slow? |
| `error` | Failures | What errors repeat? |

## Example: Finding Improvement Targets

```bash
# Find slow LLM calls by model
sleeptime-logs llm-calls --min-latency 3000 --since 7d --json | \
  jq 'group_by(.event.model) | map({model: .[0].event.model, count: length})'

# Find frequent error patterns
sleeptime-logs errors --since 7d --json | \
  jq 'group_by(.event.message) | map({msg: .[0].event.message, count: length}) | sort_by(-.count)'
```

## Configuration

```bash
# Grafana Cloud
export GRAFANA_LOKI_URL="https://logs-prod-us-central1.grafana.net"
export GRAFANA_LOKI_USER="your-user-id"
export GRAFANA_LOKI_TOKEN="your-api-token"

# Or use .tidepool.env in project root
```

## Integration with Micro-Gas Town

The sleeptime CLI integrates with micro-gastown polecats:
1. **Log Analyzer** queries patterns via this CLI
2. **Conditional Refiner** drafts improvements
3. **PR Filer** commits with log evidence

See `tools/micro-gastown/` for automation.

## Related Documentation

- [effects/observability-executor/](../../effects/observability-executor/) - Produces the logs we query
- [tools/CLAUDE.md](../CLAUDE.md) - Tools overview
- `README.md` in this directory - Full usage reference
