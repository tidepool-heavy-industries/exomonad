# Sleeptime Logs CLI

CLI tool for querying Grafana/Loki logs during sleeptime evolution workflow.

## Installation

```bash
cabal build sleeptime-logs
cabal install sleeptime-logs
```

## Configuration

The tool reads configuration from environment variables:

```bash
# Required for Grafana Cloud
export GRAFANA_LOKI_URL="https://logs-prod-us-central1.grafana.net"
export GRAFANA_LOKI_USER="your-user-id"
export GRAFANA_LOKI_TOKEN="your-api-token"

# For local Loki (default)
export GRAFANA_LOKI_URL="http://localhost:3100"
```

Or use `.tidepool.env` in the project root (automatically loaded).

## Usage

### Query Commands

```bash
# Fetch recent graph transitions
sleeptime-logs transitions --since 24h

# Fetch LLM calls with latency > threshold
sleeptime-logs llm-calls --min-latency 2000 --since 1h

# Fetch errors
sleeptime-logs errors --since 1h

# Fetch all events for a session
sleeptime-logs session abc123-session-id
```

### Output Formats

```bash
# Human-readable (default)
sleeptime-logs transitions --since 1h

# JSON for piping
sleeptime-logs transitions --since 24h --json | jq '.[] | select(.event.to == "error")'
```

### Common Options

- `--since DURATION` - Time range (e.g., `24h`, `1h`, `30m`, `7d`)
- `--limit N` - Maximum results (default: 100)
- `--json` - Output as JSON array
- `--verbose` - Verbose output

## Sleeptime Evolution Workflow

The sleeptime-logs CLI supports the sleeptime evolution workflow:

```
1. Run agent sessions → events logged to Loki
2. Query logs to find patterns:
   - Which transitions fail most?
   - Which LLM calls are slow?
   - What errors occur repeatedly?
3. Analyze patterns → identify DSL improvements
4. Make targeted changes → file PR
5. Deploy and observe improvement
```

### Example Workflow: Finding Slow LLM Calls

```bash
# Find LLM calls taking > 3 seconds
sleeptime-logs llm-calls --min-latency 3000 --since 7d --json > slow_calls.json

# Analyze by model
cat slow_calls.json | jq 'group_by(.event.model) | map({model: .[0].event.model, count: length, avg_latency: (map(.event.latency_ms) | add / length)})'

# Find specific patterns
cat slow_calls.json | jq '.[] | select(.event.prompt_tokens > 4000)'
```

### Example Workflow: Tracking Transition Failures

```bash
# Get transitions for the past day
sleeptime-logs transitions --since 24h --json > transitions.json

# Count transitions by destination
cat transitions.json | jq 'group_by(.event.to) | map({node: .[0].event.to, count: length}) | sort_by(-.count)'

# Find transitions to error states
cat transitions.json | jq '.[] | select(.event.to | contains("error"))'
```

### Example Workflow: Session Debugging

```bash
# Get all events for a specific session
sleeptime-logs session abc123 --json > session.json

# Timeline view
cat session.json | jq 'sort_by(.timestamp) | .[] | "\(.timestamp): \(.event.type)"'

# Find errors in session
cat session.json | jq '.[] | select(.event.type == "error")'
```

## Event Types

The CLI queries TidepoolEvent structured logs:

| Event Type | Description | Key Fields |
|------------|-------------|------------|
| `graph_transition` | Node-to-node transition | `from`, `to`, `trigger` |
| `llm_call` | LLM API call | `model`, `prompt_tokens`, `completion_tokens`, `latency_ms` |
| `user_action` | User interaction | `action_type`, `node_context` |
| `effect_execution` | Effect execution | `effect_type`, `success`, `latency_ms` |
| `error` | Error event | `message`, `context` |

## LogQL Queries

The CLI generates LogQL queries automatically. For advanced queries, use Grafana directly:

```logql
# All transitions in the last hour
{app="tidepool"} |= "graph_transition" | json

# LLM calls with high latency
{app="tidepool"} |= "llm_call" | json | latency_ms > 2000

# Errors with context
{app="tidepool"} |= "error" | json | line_format "{{.message}}: {{.context}}"

# Specific session
{app="tidepool", session_id="abc123"} | json
```

## Integration with Micro-Gas Town

The sleeptime-logs CLI integrates with the micro-Gas Town sleeptime system:

1. **Log Analyzer Polecat** uses this CLI to query patterns
2. **Conditional Refiner Polecat** receives patterns and drafts changes
3. **PR Filer Polecat** commits changes with log evidence

See `tools/micro-gastown/` for the sleeptime automation setup.

## Development

```bash
# Build
cabal build sleeptime-logs

# Run locally
cabal run sleeptime-logs -- transitions --since 1h

# Test with local Loki
docker run -d --name loki -p 3100:3100 grafana/loki:latest
cabal run sleeptime-logs -- errors --since 1h
```
