-- Kaizen: ClickHouse views for swarm analysis from OTel traces
-- The OTel Collector ClickHouse exporter creates the 'otel.otel_traces' table.

CREATE VIEW IF NOT EXISTS otel.agent_events AS
SELECT
    event_ts AS ts,
    event_name AS type,
    event_attrs['agent_id'] AS agent_id,
    event_attrs AS data,
    TraceId,
    SpanId
FROM otel.otel_traces
ARRAY JOIN
    Events.Timestamp AS event_ts,
    Events.Name AS event_name,
    Events.Attributes AS event_attrs;

-- Agent lifecycles: spawn → complete, with duration
CREATE VIEW IF NOT EXISTS otel.agent_lifecycles AS
WITH spawns AS (
  SELECT ts, agent_id AS parent_id,
    data['child_agent'] AS child_agent,
    data['agent_type'] AS agent_type,
    data['spawn_type'] AS spawn_type,
    data['branch'] AS branch,
    data['slug'] AS slug
  FROM otel.agent_events WHERE type = 'agent.spawned'
),
completions AS (
  SELECT ts, agent_id,
    data['status'] AS status,
    data['message'] AS message,
    data['pr_number'] AS pr_number
  FROM otel.agent_events WHERE type = 'agent.completed'
)
SELECT
  s.child_agent AS agent_id,
  s.agent_type,
  s.spawn_type,
  s.branch,
  s.slug,
  s.ts AS spawned_at,
  c.ts AS completed_at,
  dateDiff('ms', s.ts, c.ts) AS duration_ms,
  if(c.status = '', 'running', c.status) AS status,
  c.message AS failure_message,
  toNullable(toInt32OrNull(c.pr_number)) AS pr_number
FROM spawns s
LEFT JOIN completions c ON c.agent_id = s.child_agent
WHERE s.child_agent != '';

-- Agent tree: parent-child hierarchy
CREATE VIEW IF NOT EXISTS otel.agent_tree AS
SELECT
  agent_id AS parent_id,
  data['child_agent'] AS child_id,
  data['agent_type'] AS agent_type,
  data['spawn_type'] AS spawn_type
FROM otel.agent_events
WHERE type = 'agent.spawned';

-- PR pipeline: filed → merged, with duration
CREATE VIEW IF NOT EXISTS otel.pr_pipeline AS
WITH filed AS (
  SELECT ts, agent_id,
    data['pr_number'] AS pr_number,
    data['head_branch'] AS head_branch
  FROM otel.agent_events WHERE type = 'pr.filed'
),
merged AS (
  SELECT ts, data['pr_number'] AS pr_number
  FROM otel.agent_events WHERE type = 'pr.merged'
),
failed AS (
  SELECT ts, data['pr_number'] AS pr_number, data['error'] AS error
  FROM otel.agent_events WHERE type = 'pr.merge_failed'
)
SELECT
  toInt32OrNull(f.pr_number) AS pr_number,
  f.head_branch AS branch,
  f.agent_id,
  f.ts AS filed_at,
  m.ts AS merged_at,
  dateDiff('ms', f.ts, m.ts) AS duration_ms,
  mf.error AS merge_error,
  CASE WHEN m.ts > 0 THEN 'merged'
       WHEN mf.ts > 0 THEN 'failed'
       ELSE 'open' END AS status
FROM filed f
LEFT JOIN merged m ON m.pr_number = f.pr_number
LEFT JOIN failed mf ON mf.pr_number = f.pr_number;

-- Tool usage: call counts and duration by tool and agent
CREATE VIEW IF NOT EXISTS otel.tool_usage AS
WITH tool_events AS (
  SELECT agent_id,
    data['tool_name'] AS tool_name,
    data['duration_ms'] AS duration_ms
  FROM otel.agent_events WHERE type = 'tool.called'
)
SELECT
  tool_name,
  agent_id,
  COUNT(*) AS call_count,
  AVG(toFloat64OrNull(duration_ms)) AS avg_duration_ms
FROM tool_events
GROUP BY tool_name, agent_id;

-- Tool summary: aggregated across agents
CREATE VIEW IF NOT EXISTS otel.tool_summary AS
SELECT
  tool_name,
  SUM(call_count) AS total_calls,
  uniq(agent_id) AS agent_count,
  AVG(avg_duration_ms) AS avg_duration_ms
FROM otel.tool_usage
GROUP BY tool_name
ORDER BY total_calls DESC;

-- Delivery audit: message delivery chain
CREATE VIEW IF NOT EXISTS otel.delivery_audit AS
SELECT
  ts,
  agent_id AS sender,
  data['recipient'] AS recipient,
  data['method'] AS method,
  data['outcome'] AS outcome,
  data['detail'] AS detail
FROM otel.agent_events
WHERE type = 'message.delivery';

-- Copilot reviews: structured review data
CREATE VIEW IF NOT EXISTS otel.copilot_reviews AS
SELECT
  data['branch'] AS branch,
  data['status'] AS status,
  data['message'] AS message,
  ts AS review_ts,
  length(JSONExtractArrayRaw(data['comments'])) AS comment_count,
  length(JSONExtractArrayRaw(data['reviews'])) AS review_count
FROM otel.agent_events
WHERE type = 'copilot.review';

-- Swarm timeline: flat interleaved timeline with human-readable summaries
CREATE VIEW IF NOT EXISTS otel.swarm_timeline AS
SELECT
  ts,
  agent_id,
  type,
  CASE
    WHEN type = 'agent.spawned' THEN concat('Spawned ', if(data['child_agent'] != '', data['child_agent'], '?'), ' (', if(data['agent_type'] != '', data['agent_type'], '?'), ')')
    WHEN type = 'agent.completed' THEN concat('Completed: ', if(data['status'] != '', data['status'], '?'))
    WHEN type = 'tool.called' THEN concat(if(data['tool_name'] != '', data['tool_name'], '?'), ' (', if(data['duration_ms'] != '', data['duration_ms'], '?'), 'ms)')
    WHEN type = 'message.delivery' THEN concat('→ ', if(data['recipient'] != '', data['recipient'], '?'), ' via ', if(data['method'] != '', data['method'], '?'))
    WHEN type = 'pr.filed' THEN concat('Filed PR #', if(data['pr_number'] != '', data['pr_number'], '?'))
    WHEN type = 'pr.merged' THEN concat('Merged PR #', if(data['pr_number'] != '', data['pr_number'], '?'))
    WHEN type = 'hook.stop' THEN concat('Stop hook: ', if(data['decision'] != '', data['decision'], '?'))
    WHEN type = 'event.dispatched' THEN concat('Event: ', if(data['event_type'] != '', data['event_type'], '?'), ' → ', if(data['action'] != '', data['action'], '?'))
    WHEN type = 'agent.notify_parent' THEN concat('Notify parent: ', if(data['status'] != '', data['status'], '?'))
    ELSE type
  END AS summary
FROM otel.agent_events
ORDER BY ts;

-- Swarm summary: one-row overview
CREATE VIEW IF NOT EXISTS otel.swarm_summary AS
SELECT
  (SELECT COUNT(*) FROM otel.agent_events WHERE type = 'agent.spawned') AS total_agents,
  (SELECT COUNT(*) FROM otel.agent_events WHERE type = 'agent.completed' AND data['status'] = 'success') AS succeeded,
  (SELECT COUNT(*) FROM otel.agent_events WHERE type = 'agent.completed' AND data['status'] = 'failure') AS failed,
  (SELECT COUNT(*) FROM otel.agent_events WHERE type = 'pr.filed') AS total_prs,
  (SELECT COUNT(*) FROM otel.agent_events WHERE type = 'pr.merged') AS merged_prs,
  (SELECT COUNT(*) FROM otel.agent_events WHERE type = 'pr.merge_failed') AS failed_merges,
  (SELECT COUNT(*) FROM otel.agent_events) AS total_events,
  (SELECT MIN(ts) FROM otel.agent_events) AS first_event,
  (SELECT MAX(ts) FROM otel.agent_events) AS last_event;
