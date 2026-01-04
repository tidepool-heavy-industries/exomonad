# Query Patterns Skill

Skill for querying production logs and identifying patterns for DSL evolution.

## Overview

The Log Analyzer polecat uses this skill to query Grafana/Loki via the
`sleeptime-logs` CLI. It identifies patterns that suggest improvements
to the Tidepool graph DSL.

## Usage

```bash
# Full analysis (all event types)
micro-gastown analyze --since 24h

# Focus on slow LLM calls
micro-gastown analyze --since 24h --query-type llm-calls

# Custom thresholds
micro-gastown analyze --since 7d --min-occurrences 5
```

## Pattern Types

### Slow LLM Calls

**Query**: `sleeptime-logs llm-calls --min-latency 3000 --since {since} --json`

**Indicators**:
- Latency > 3000ms (warning) or > 5000ms (critical)
- High token counts relative to output
- Repeated calls to same node

**Recommendations**:
- Split long prompts into smaller chunks
- Reduce context by removing unused fields
- Consider caching for repeated queries

### Failed Transitions

**Query**: `sleeptime-logs transitions --since {since} --json | jq '.[] | select(.event.to | contains("error"))'`

**Indicators**:
- Transitions to error states
- High failure rate on specific edges
- Timeout patterns

**Recommendations**:
- Add retry logic for transient failures
- Improve error handling in logic nodes
- Add fallback paths for common failures

### Repeated Errors

**Query**: `sleeptime-logs errors --since {since} --json`

**Indicators**:
- Same error message appearing > N times
- Errors clustered in specific nodes
- Error rate exceeding thresholds

**Recommendations**:
- Fix root cause if possible
- Add defensive validation
- Improve error messages for debugging

### Dead Paths

**Query**: Analyze transition matrix for nodes with zero incoming edges.

**Indicators**:
- Nodes never reached in production
- Goto targets that are never selected
- Unreachable branches in logic nodes

**Recommendations**:
- Remove dead code
- Fix routing logic if path should be reachable
- Document intentionally unused paths (feature flags)

## Output Schema

```json
{
  "patterns": [
    {
      "id": "slow-llm-dmScene",
      "type": "slow_llm",
      "severity": "warning|critical|info",
      "occurrences": 12,
      "details": {
        "node": "dmScene",
        "avg_latency_ms": 4200,
        "max_latency_ms": 7500,
        "prompt_tokens_avg": 3800,
        "completion_tokens_avg": 450
      },
      "recommendation": "Consider splitting prompt or reducing context",
      "grafana_query": "{app=\"tidepool\"} |= \"llm_call\" | json | node == \"dmScene\" | latency_ms > 3000"
    }
  ],
  "summary": {
    "total_patterns": 3,
    "critical": 0,
    "warning": 2,
    "info": 1,
    "query_range": "2026-01-02T00:00:00Z to 2026-01-03T00:00:00Z",
    "events_analyzed": 1547
  }
}
```

## Integration with Observability

This skill consumes `TidepoolEvent` types published by the Observability effect:

| Event Type | Fields Used |
|------------|-------------|
| `graph_transition` | from, to, trigger |
| `llm_call` | model, prompt_tokens, completion_tokens, latency_ms |
| `effect_execution` | effect_type, success, latency_ms |
| `error` | message, context |

See `Tidepool.Effects.Observability` for event definitions.

## Example Session

```
$ micro-gastown analyze --since 24h --output patterns.json

Querying Loki for last 24 hours...
  - Transitions: 523 events
  - LLM calls: 891 events
  - Errors: 47 events

Analyzing patterns...
  - Slow LLM calls: 2 patterns found
  - Failed transitions: 1 pattern found
  - Repeated errors: 0 patterns found

Pattern Report:
  [WARNING] slow_llm at dmScene: avg 4200ms, 12 occurrences
  [WARNING] slow_llm at dmAftermath: avg 3100ms, 8 occurrences
  [INFO] failed_transition dmAction->dmScene: 3 failures

Wrote patterns.json (3 patterns)
```
