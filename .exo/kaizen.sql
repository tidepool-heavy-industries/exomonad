-- Kaizen: DuckDB views for swarm analysis
-- Load: duckdb -init .exo/kaizen.sql
-- Or use: .exo/bin/kaizen

-- Base: typed event table
CREATE VIEW events AS
SELECT
  ts::TIMESTAMP AS ts,
  id,
  type,
  agent_id,
  data
FROM read_json_auto('.exo/events.jsonl');

-- Agent lifecycles: spawn → complete, with duration
CREATE VIEW agent_lifecycles AS
SELECT
  s.data->>'child_agent' AS agent_id,
  s.data->>'agent_type' AS agent_type,
  s.data->>'spawn_type' AS spawn_type,
  s.data->>'branch' AS branch,
  s.data->>'slug' AS slug,
  s.ts AS spawned_at,
  c.ts AS completed_at,
  EXTRACT(EPOCH FROM (c.ts - s.ts)) * 1000 AS duration_ms,
  COALESCE(c.data->>'status', 'running') AS status,
  c.data->>'message' AS failure_message,
  CAST(c.data->>'pr_number' AS INTEGER) AS pr_number
FROM events s
LEFT JOIN events c
  ON c.type = 'agent.completed'
  AND c.agent_id = s.data->>'child_agent'
WHERE s.type = 'agent.spawned';

-- PR pipeline: filed → merged, with duration
CREATE VIEW pr_pipeline AS
SELECT
  CAST(f.data->>'pr_number' AS INTEGER) AS pr_number,
  f.data->>'head_branch' AS branch,
  f.agent_id,
  f.ts AS filed_at,
  m.ts AS merged_at,
  EXTRACT(EPOCH FROM (m.ts - f.ts)) * 1000 AS duration_ms,
  mf.data->>'error' AS merge_error,
  CASE WHEN m.ts IS NOT NULL THEN 'merged'
       WHEN mf.ts IS NOT NULL THEN 'failed'
       ELSE 'open' END AS status
FROM events f
LEFT JOIN events m
  ON m.type = 'pr.merged'
  AND CAST(m.data->>'pr_number' AS INTEGER) = CAST(f.data->>'pr_number' AS INTEGER)
LEFT JOIN events mf
  ON mf.type = 'pr.merge_failed'
  AND CAST(mf.data->>'pr_number' AS INTEGER) = CAST(f.data->>'pr_number' AS INTEGER)
WHERE f.type = 'pr.filed';

-- Tool usage: call counts by tool and agent
CREATE VIEW tool_usage AS
SELECT
  data->>'tool_name' AS tool_name,
  data->>'agent_id' AS agent_id,
  COUNT(*) AS call_count
FROM events
WHERE type = 'tool.called'
GROUP BY data->>'tool_name', data->>'agent_id';

-- Tool summary: aggregated across agents
CREATE VIEW tool_summary AS
SELECT
  tool_name,
  SUM(call_count) AS total_calls,
  COUNT(DISTINCT agent_id) AS agent_count
FROM tool_usage
GROUP BY tool_name
ORDER BY total_calls DESC;

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

-- Swarm summary: one-row overview
CREATE VIEW swarm_summary AS
SELECT
  (SELECT COUNT(*) FROM events WHERE type = 'agent.spawned') AS total_agents,
  (SELECT COUNT(*) FROM events WHERE type = 'agent.completed' AND data->>'status' = 'success') AS succeeded,
  (SELECT COUNT(*) FROM events WHERE type = 'agent.completed' AND data->>'status' = 'failure') AS failed,
  (SELECT COUNT(*) FROM events WHERE type = 'pr.filed') AS total_prs,
  (SELECT COUNT(*) FROM events WHERE type = 'pr.merged') AS merged_prs,
  (SELECT COUNT(*) FROM events WHERE type = 'pr.merge_failed') AS failed_merges,
  (SELECT COUNT(*) FROM events) AS total_events,
  (SELECT MIN(ts) FROM events) AS first_event,
  (SELECT MAX(ts) FROM events) AS last_event;
