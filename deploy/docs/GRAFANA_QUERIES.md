# Grafana LogQL Query Examples for Tidepool

Quick reference for querying tidepool execution logs via Grafana Cloud (Loki).

## Setup Complete

**Enriched logging now captures:**
- ✅ Graph context: `node_name`, `node_type`, `turn_number`
- ✅ Full effect payloads and results
- ✅ LLM metrics: `llm_model`, `llm_prompt_tokens`, `llm_completion_tokens`, `llm_cache_hit`
- ✅ Habitica operations: `habitica_operation`, `habitica_item_id`

## LogQL Basics

Loki uses LogQL, which has two main parts:
- **Log stream selector**: `{label="value"}` - Filters log streams
- **Log pipeline**: `| json | line_format ...` - Parses and transforms logs

Our logs are JSON, so we always start with `{service_name="tidepool"} | json`.

## Common Queries

### See Recent Sessions

```logql
{service_name="tidepool"}
| json
| graph_id = "habitica-helper"
| msg = "graph:start"
| line_format "{{.ts}} {{.session_id}}"
```

### Replay Single Session Timeline

```logql
{service_name="tidepool"}
| json
| session_id = "YOUR_SESSION_ID"
| line_format "{{.ts}} {{.msg}} effect={{.effect_type}} node={{.node_name}} turn={{.turn_number}} latency={{.latency_ms}}ms result={{.result_type}}"
```

### What LLM Calls Are Happening?

```logql
{service_name="tidepool"}
| json
| effect_type = "LlmComplete"
| line_format "{{.ts}} node={{.node_name}} model={{.llm_model}} prompt_tokens={{.llm_prompt_tokens}} completion_tokens={{.llm_completion_tokens}} latency={{.latency_ms}}ms"
```

### Token Usage by Node (Metrics Query)

For aggregations, use LogQL metrics queries (query type: **Metrics**, not **Logs**):

```logql
sum by (node_name) (
  sum_over_time(
    {service_name="tidepool"}
    | json
    | effect_type = "LlmComplete"
    | unwrap llm_prompt_tokens [5m]
  )
)
```

Or for total tokens (prompt + completion):

```logql
sum by (node_name) (
  sum_over_time(
    {service_name="tidepool"}
    | json
    | effect_type = "LlmComplete"
    | unwrap llm_prompt_tokens [5m]
  )
)
+
sum by (node_name) (
  sum_over_time(
    {service_name="tidepool"}
    | json
    | effect_type = "LlmComplete"
    | unwrap llm_completion_tokens [5m]
  )
)
```

### Which Habitica Operations Are Most Common?

```logql
sum by (habitica_operation) (
  count_over_time(
    {service_name="tidepool"}
    | json
    | effect_type = "Habitica" [5m]
  )
)
```

### Error Patterns

**Count by effect and node:**
```logql
sum by (effect_type, node_name) (
  count_over_time(
    {service_name="tidepool"}
    | json
    | result_type = "error" [1h]
  )
)
```

**Sample error messages:**
```logql
{service_name="tidepool"}
| json
| result_type = "error"
| line_format "{{.effect_type}} @ {{.node_name}}: {{.error}}"
```

### Execution Flow by Turn

```logql
{service_name="tidepool"}
| json
| session_id = "YOUR_SESSION_ID"
| turn_number >= 0
| line_format "Turn {{.turn_number}}: {{.effect_type}}"
```

**Note**: Loki doesn't have `make_list()` aggregation like Axiom. For turn-by-turn flow, use dashboard panels with table visualization.

### LLM Cache Hit Rate

```logql
sum(
  count_over_time(
    {service_name="tidepool"}
    | json
    | effect_type = "LlmComplete"
    | llm_cache_hit = "true" [1h]
  )
)
/
sum(
  count_over_time(
    {service_name="tidepool"}
    | json
    | effect_type = "LlmComplete"
    | llm_cache_hit != "" [1h]
  )
)
* 100
```

### Slow Effects

```logql
{service_name="tidepool"}
| json
| unwrap latency_ms
| latency_ms > 1000
| line_format "{{.ts}} {{.effect_type}} @ {{.node_name}}: {{.latency_ms}}ms (session: {{.session_id}})"
```

**Top 20 slowest:**
```logql
topk(20,
  max_over_time(
    {service_name="tidepool"}
    | json
    | unwrap latency_ms [5m]
  ) by (effect_type, node_name, session_id)
)
```

## Iteration Workflow

1. **View recent sessions** to pick one to analyze
2. **Replay timeline** to understand execution flow
3. **Identify patterns** - which effects fail, where time is spent
4. **Modify graph** based on insights
5. **Deploy and repeat**

## Using via Grafana Cloud UI

1. Go to Grafana Cloud → **Explore**
2. Select **Loki** data source
3. Paste query in query editor
4. Click **Run query**
5. Use **Table** or **Logs** visualization depending on query type

## What to Look For

**Good signs:**
- Consistent execution paths (same nodes in same order)
- Low LLM latency (<2s for most calls)
- High cache hit rates (>50% after warmup)
- No errors in happy path

**Red flags:**
- Errors clustering in specific nodes
- Exponential token growth (prompt bloat)
- Missing turn numbers (graph stuck?)
- High latency on simple operations

## LogQL Tips

### Filtering
```logql
| effect_type = "LlmComplete"     # Exact match
| effect_type != "Log"            # Not equal
| effect_type =~ "Llm.*"          # Regex match
| latency_ms > 1000               # Numeric comparison
```

### Parsing JSON
```logql
| json                            # Parse JSON fields
| json effect_payload="payload"   # Rename nested field
```

### Line Formatting
```logql
| line_format "{{.ts}} {{.msg}}"                    # Template
| line_format "{{.node_name | ToUpper}}"            # Transform
| line_format "tokens={{.llm_prompt_tokens | printf \"%d\"}}"  # Format numbers
```

### Metrics vs Logs
- **Logs mode**: Returns log lines (for reading events)
- **Metrics mode**: Returns numbers (for aggregations, dashboards)
- Use `| unwrap field_name` to convert log field to metric

## Next Steps

1. Set up Grafana Cloud (see [GRAFANA_CLOUD_SETUP.md](GRAFANA_CLOUD_SETUP.md))
2. Configure OTLP export in wrangler.toml
3. Deploy and verify data flow
4. Query recent sessions to baseline current behavior
5. Use insights to evolve graph

## References

- [LogQL Documentation](https://grafana.com/docs/loki/latest/query/)
- [Cloudflare Workers OTLP Export](https://developers.cloudflare.com/workers/observability/exporting-opentelemetry-data/)
- [Grafana Cloud Free Tier](https://grafana.com/pricing/)
