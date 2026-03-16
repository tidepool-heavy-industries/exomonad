-- Kaizen: ClickHouse views for swarm analysis from OTel traces
-- The OTel Collector ClickHouse exporter creates the 'otel.otel_traces' table.

-- Drop the old base view
DROP VIEW IF EXISTS otel.agent_events;

-- Agent lifecycles: spawn → activity
-- We find agent spawns in spans that have 'child_agent' in their Events array
CREATE OR REPLACE VIEW otel.agent_lifecycles AS
WITH 
spawns AS (
  SELECT 
    Timestamp AS spawned_at,
    replaceRegexpOne(if(SpanAttributes['agent_id'] != '', SpanAttributes['agent_id'], SpanAttributes['agent_name']), '^Some\("?([^")]+)"?\)$', '\1') AS parent_id,
    arrayFirst(x -> mapContains(x, 'child_agent'), Events.Attributes)['child_agent'] AS child_agent,
    arrayFirst(x -> mapContains(x, 'agent_type'), Events.Attributes)['agent_type'] AS agent_type,
    arrayFirst(x -> mapContains(x, 'spawn_type'), Events.Attributes)['spawn_type'] AS spawn_type,
    arrayFirst(x -> mapContains(x, 'branch'), Events.Attributes)['branch'] AS branch,
    arrayFirst(x -> mapContains(x, 'slug'), Events.Attributes)['slug'] AS slug
  FROM otel.otel_traces
  WHERE arrayExists(x -> mapContains(x, 'child_agent'), Events.Attributes)
),
activity AS (
  SELECT 
    replaceRegexpOne(if(SpanAttributes['agent_id'] != '', SpanAttributes['agent_id'], SpanAttributes['agent_name']), '^Some\("?([^")]+)"?\)$', '\1') AS agent_id,
    MAX(Timestamp) AS last_activity,
    countIf(SpanName = 'call_tool') AS tool_calls,
    countIf(SpanName = 'notify_parent_delivery') AS notifications
  FROM otel.otel_traces
  WHERE agent_id != ''
  GROUP BY agent_id
)
SELECT 
  s.child_agent AS agent_id,
  s.parent_id,
  s.agent_type,
  s.spawn_type,
  s.branch,
  s.slug,
  s.spawned_at,
  a.last_activity,
  a.tool_calls,
  a.notifications,
  if(a.last_activity > 0, dateDiff('ms', s.spawned_at, a.last_activity), 0) AS duration_ms,
  if(a.last_activity > 0, 'active', 'spawned') AS status
FROM spawns s
LEFT JOIN activity a ON a.agent_id = s.child_agent
WHERE s.child_agent != '';

-- Agent tree: parent-child hierarchy
CREATE OR REPLACE VIEW otel.agent_tree AS
SELECT
  replaceRegexpOne(if(SpanAttributes['agent_id'] != '', SpanAttributes['agent_id'], SpanAttributes['agent_name']), '^Some\("?([^")]+)"?\)$', '\1') AS parent_id,
  arrayFirst(x -> mapContains(x, 'child_agent'), Events.Attributes)['child_agent'] AS child_id,
  arrayFirst(x -> mapContains(x, 'agent_type'), Events.Attributes)['agent_type'] AS agent_type,
  arrayFirst(x -> mapContains(x, 'spawn_type'), Events.Attributes)['spawn_type'] AS spawn_type
FROM otel.otel_traces
WHERE arrayExists(x -> mapContains(x, 'child_agent'), Events.Attributes);

-- Tool usage: call counts and duration by tool and agent
CREATE OR REPLACE VIEW otel.tool_usage AS
SELECT
  SpanAttributes['tool'] AS tool_name,
  replaceRegexpOne(if(SpanAttributes['agent_id'] != '', SpanAttributes['agent_id'], SpanAttributes['agent_name']), '^Some\("?([^")]+)"?\)$', '\1') AS agent_id,
  COUNT(*) AS call_count,
  AVG(Duration / 1000000) AS avg_duration_ms
FROM otel.otel_traces
WHERE SpanName = 'call_tool'
GROUP BY tool_name, agent_id;

-- Tool summary: aggregated across agents
CREATE OR REPLACE VIEW otel.tool_summary AS
SELECT
  tool_name,
  SUM(call_count) AS total_calls,
  uniq(agent_id) AS agent_count,
  AVG(avg_duration_ms) AS avg_duration_ms
FROM otel.tool_usage
GROUP BY tool_name
ORDER BY total_calls DESC;

-- Delivery audit: message delivery chain
CREATE OR REPLACE VIEW otel.delivery_audit AS
SELECT
  Timestamp AS ts,
  replaceRegexpOne(if(SpanAttributes['agent_id'] != '', SpanAttributes['agent_id'], SpanAttributes['agent_name']), '^Some\("?([^")]+)"?\)$', '\1') AS agent_id,
  SpanName AS type,
  SpanAttributes['status'] AS status,
  SpanAttributes['parent_session_id'] AS parent_session_id
FROM otel.otel_traces
WHERE SpanName IN ('notify_parent_delivery', 'deliver_to_agent');

-- Swarm timeline: flat interleaved timeline
CREATE OR REPLACE VIEW otel.swarm_timeline AS
SELECT
  Timestamp AS ts,
  replaceRegexpOne(if(SpanAttributes['agent_id'] != '', SpanAttributes['agent_id'], SpanAttributes['agent_name']), '^Some\("?([^")]+)"?\)$', '\1') AS agent_id,
  SpanName AS type,
  CASE
    WHEN arrayExists(x -> mapContains(x, 'child_agent'), Events.Attributes) THEN 
      concat('Spawned ', arrayFirst(x -> mapContains(x, 'child_agent'), Events.Attributes)['child_agent'], 
             ' (', arrayFirst(x -> mapContains(x, 'agent_type'), Events.Attributes)['agent_type'], ')')
    WHEN SpanName = 'call_tool' THEN 
      concat('Tool: ', SpanAttributes['tool'])
    WHEN SpanName = 'notify_parent_delivery' THEN 
      concat('Notify parent: ', SpanAttributes['status'])
    WHEN SpanName = 'handle_hook_request' THEN 
      concat('Hook: ', SpanAttributes['hook'])
    WHEN SpanName = 'dispatch' THEN 
      concat('Effect: ', SpanAttributes['effect_type'])
    WHEN SpanName = 'deliver_to_agent' THEN 
      'Delivering message'
    ELSE SpanName
  END AS summary
FROM otel.otel_traces
ORDER BY ts;

-- Swarm summary: one-row overview
CREATE OR REPLACE VIEW otel.swarm_summary AS
SELECT
  (SELECT count(DISTINCT arrayFirst(x -> mapContains(x, 'child_agent'), Events.Attributes)['child_agent']) 
   FROM otel.otel_traces WHERE arrayExists(x -> mapContains(x, 'child_agent'), Events.Attributes) 
   AND arrayFirst(x -> mapContains(x, 'child_agent'), Events.Attributes)['child_agent'] != '') AS total_agents,
  (SELECT count(*) FROM otel.otel_traces WHERE SpanName = 'call_tool') AS total_tool_calls,
  (SELECT count(*) FROM otel.otel_traces WHERE SpanName = 'notify_parent_delivery') AS total_notifications,
  (SELECT MIN(Timestamp) FROM otel.otel_traces) AS first_span,
  (SELECT MAX(Timestamp) FROM otel.otel_traces) AS last_span;

-- Copilot reviews (if they exist as spans)
CREATE OR REPLACE VIEW otel.copilot_reviews AS
SELECT
  SpanAttributes['branch'] AS branch,
  SpanAttributes['status'] AS status,
  SpanAttributes['message'] AS message,
  Timestamp AS review_ts
FROM otel.otel_traces
WHERE SpanName = 'copilot_review';

-- PR pipeline (adapted to SpanName)
CREATE OR REPLACE VIEW otel.pr_pipeline AS
WITH filed AS (
  SELECT Timestamp AS ts, 
    replaceRegexpOne(if(SpanAttributes['agent_id'] != '', SpanAttributes['agent_id'], SpanAttributes['agent_name']), '^Some\("?([^")]+)"?\)$', '\1') AS agent_id,
    SpanAttributes['pr_number'] AS pr_number,
    SpanAttributes['head_branch'] AS head_branch
  FROM otel.otel_traces WHERE SpanName = 'file_pr'
),
merged AS (
  SELECT Timestamp AS ts, SpanAttributes['pr_number'] AS pr_number
  FROM otel.otel_traces WHERE SpanName = 'merge_pr'
)
SELECT
  toInt32OrNull(f.pr_number) AS pr_number,
  f.head_branch AS branch,
  f.agent_id,
  f.ts AS filed_at,
  m.ts AS merged_at,
  dateDiff('ms', f.ts, m.ts) AS duration_ms,
  CASE WHEN m.ts > 0 THEN 'merged'
       ELSE 'open' END AS status
FROM filed f
LEFT JOIN merged m ON m.pr_number = f.pr_number;
