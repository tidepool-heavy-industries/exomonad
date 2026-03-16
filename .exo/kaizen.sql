-- Kaizen: DuckDB views for swarm analysis
-- Load: duckdb -init .exo/kaizen.sql
-- Or use: .exo/bin/kaizen

-- Base: typed event table (globs all per-agent logs)
-- Uses explicit JSON column type to avoid DuckDB STRUCT union conflicts
-- when different agents emit different data shapes for the same event type.
CREATE VIEW events AS
SELECT
  ts::TIMESTAMP AS ts,
  id,
  type,
  agent_id,
  data
FROM read_json('.exo/logs/*.jsonl', format='newline_delimited', columns={ts: 'VARCHAR', id: 'VARCHAR', type: 'VARCHAR', agent_id: 'VARCHAR', data: 'JSON'});

-- Per-type materialized subqueries avoid DuckDB predicate pushdown bug:
-- DuckDB pushes data->> predicates before type filtering, evaluating ->>
-- on rows with incompatible data shapes. Subqueries force type filter first.

-- Agent lifecycles: spawn → complete, with duration
CREATE VIEW agent_lifecycles AS
WITH spawns AS (
  SELECT ts, agent_id AS parent_id,
    data->>'child_agent' AS child_agent,
    data->>'agent_type' AS agent_type,
    data->>'spawn_type' AS spawn_type,
    data->>'branch' AS branch,
    data->>'slug' AS slug
  FROM events WHERE type = 'agent.spawned'
),
completions AS (
  SELECT ts, agent_id,
    data->>'status' AS status,
    data->>'message' AS message,
    data->>'pr_number' AS pr_number
  FROM events WHERE type = 'agent.completed'
)
SELECT
  s.child_agent AS agent_id,
  s.agent_type,
  s.spawn_type,
  s.branch,
  s.slug,
  s.ts AS spawned_at,
  c.ts AS completed_at,
  EXTRACT(EPOCH FROM (c.ts - s.ts)) * 1000 AS duration_ms,
  COALESCE(c.status, 'running') AS status,
  c.message AS failure_message,
  TRY_CAST(c.pr_number AS INTEGER) AS pr_number
FROM spawns s
LEFT JOIN completions c ON c.agent_id = s.child_agent
WHERE s.child_agent IS NOT NULL;

-- Agent tree: parent-child hierarchy
CREATE VIEW agent_tree AS
SELECT
  agent_id AS parent_id,
  data->>'child_agent' AS child_id,
  data->>'agent_type' AS agent_type,
  data->>'spawn_type' AS spawn_type
FROM events
WHERE type = 'agent.spawned';

-- PR pipeline: filed → merged, with duration
CREATE VIEW pr_pipeline AS
WITH filed AS (
  SELECT ts, agent_id,
    data->>'pr_number' AS pr_number,
    data->>'head_branch' AS head_branch
  FROM events WHERE type = 'pr.filed'
),
merged AS (
  SELECT ts, data->>'pr_number' AS pr_number
  FROM events WHERE type = 'pr.merged'
),
failed AS (
  SELECT ts, data->>'pr_number' AS pr_number, data->>'error' AS error
  FROM events WHERE type = 'pr.merge_failed'
)
SELECT
  TRY_CAST(f.pr_number AS INTEGER) AS pr_number,
  f.head_branch AS branch,
  f.agent_id,
  f.ts AS filed_at,
  m.ts AS merged_at,
  EXTRACT(EPOCH FROM (m.ts - f.ts)) * 1000 AS duration_ms,
  mf.error AS merge_error,
  CASE WHEN m.ts IS NOT NULL THEN 'merged'
       WHEN mf.ts IS NOT NULL THEN 'failed'
       ELSE 'open' END AS status
FROM filed f
LEFT JOIN merged m ON m.pr_number = f.pr_number
LEFT JOIN failed mf ON mf.pr_number = f.pr_number;

-- Tool usage: call counts and duration by tool and agent
CREATE VIEW tool_usage AS
WITH tool_events AS (
  SELECT agent_id,
    data->>'tool_name' AS tool_name,
    data->>'duration_ms' AS duration_ms
  FROM events WHERE type = 'tool.called'
)
SELECT
  tool_name,
  agent_id,
  COUNT(*) AS call_count,
  AVG(TRY_CAST(duration_ms AS DOUBLE)) AS avg_duration_ms
FROM tool_events
GROUP BY tool_name, agent_id;

-- Tool summary: aggregated across agents
CREATE VIEW tool_summary AS
SELECT
  tool_name,
  SUM(call_count) AS total_calls,
  COUNT(DISTINCT agent_id) AS agent_count,
  AVG(avg_duration_ms) AS avg_duration_ms
FROM tool_usage
GROUP BY tool_name
ORDER BY total_calls DESC;

-- Delivery audit: message delivery chain
CREATE VIEW delivery_audit AS
SELECT
  ts,
  agent_id AS sender,
  data->>'recipient' AS recipient,
  data->>'method' AS method,
  data->>'outcome' AS outcome,
  data->>'detail' AS detail
FROM events
WHERE type = 'message.delivery';

-- Copilot reviews: structured review data
CREATE VIEW copilot_reviews AS
SELECT
  data->>'branch' AS branch,
  data->>'status' AS status,
  data->>'message' AS message,
  ts AS review_ts,
  json_array_length(COALESCE(data->'comments', '[]'::JSON)) AS comment_count,
  json_array_length(COALESCE(data->'reviews', '[]'::JSON)) AS review_count
FROM events
WHERE type = 'copilot.review';

-- Swarm timeline: flat interleaved timeline with human-readable summaries
CREATE VIEW swarm_timeline AS
SELECT
  ts,
  agent_id,
  type,
  CASE
    WHEN type = 'agent.spawned' THEN 'Spawned ' || COALESCE(data->>'child_agent', '?') || ' (' || COALESCE(data->>'agent_type', '?') || ')'
    WHEN type = 'agent.completed' THEN 'Completed: ' || COALESCE(data->>'status', '?')
    WHEN type = 'tool.called' THEN COALESCE(data->>'tool_name', '?') || ' (' || COALESCE(data->>'duration_ms', '?') || 'ms)'
    WHEN type = 'message.delivery' THEN '→ ' || COALESCE(data->>'recipient', '?') || ' via ' || COALESCE(data->>'method', '?')
    WHEN type = 'pr.filed' THEN 'Filed PR #' || COALESCE(data->>'pr_number', '?')
    WHEN type = 'pr.merged' THEN 'Merged PR #' || COALESCE(data->>'pr_number', '?')
    WHEN type = 'hook.stop' THEN 'Stop hook: ' || COALESCE(data->>'decision', '?')
    WHEN type = 'event.dispatched' THEN 'Event: ' || COALESCE(data->>'event_type', '?') || ' → ' || COALESCE(data->>'action', '?')
    WHEN type = 'agent.notify_parent' THEN 'Notify parent: ' || COALESCE(data->>'status', '?')
    ELSE type
  END AS summary
FROM events
ORDER BY ts;

-- Swarm summary: one-row overview
CREATE VIEW swarm_summary AS
WITH counts AS (
  SELECT
    type,
    data->>'status' AS status
  FROM events
)
SELECT
  (SELECT COUNT(*) FROM counts WHERE type = 'agent.spawned') AS total_agents,
  (SELECT COUNT(*) FROM counts WHERE type = 'agent.completed' AND status = 'success') AS succeeded,
  (SELECT COUNT(*) FROM counts WHERE type = 'agent.completed' AND status = 'failure') AS failed,
  (SELECT COUNT(*) FROM counts WHERE type = 'pr.filed') AS total_prs,
  (SELECT COUNT(*) FROM counts WHERE type = 'pr.merged') AS merged_prs,
  (SELECT COUNT(*) FROM counts WHERE type = 'pr.merge_failed') AS failed_merges,
  (SELECT COUNT(*) FROM counts) AS total_events,
  (SELECT MIN(ts) FROM events) AS first_event,
  (SELECT MAX(ts) FROM events) AS last_event;
