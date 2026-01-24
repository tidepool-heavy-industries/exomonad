# OpenObserve Queries

Common SQL queries for analyzing traces in OpenObserve.

## Stuck Sessions

Find sessions that have looped 3 or more times.

```sql
SELECT
  trace_id,
  attributes->>'session.id' as session_id,
  attributes->>'jsonl.file' as transcript,
  attributes->>'routing.template' as template,
  attributes->>'routing.loop_count' as loops
FROM traces
WHERE
  span_name LIKE 'hook.stop%'
  AND CAST(attributes->>'routing.loop_count' AS INT) >= 3
ORDER BY timestamp DESC
LIMIT 20;
```

## Error Patterns

Group sessions by error pattern.

```sql
SELECT
  attributes->>'session.id' as session_id,
  attributes->>'error.pattern' as pattern,
  attributes->>'jsonl.file' as transcript,
  COUNT(*) as occurrences
FROM traces
WHERE attributes->>'error.pattern' IS NOT NULL
GROUP BY 1, 2, 3
ORDER BY occurrences DESC;
```

## Hook Duration

Average hook duration by template type.

```sql
SELECT
  attributes->>'routing.template' as template,
  AVG(duration) / 1000000 as avg_ms,
  COUNT(*) as count
FROM traces
WHERE span_name LIKE 'hook.%'
GROUP BY 1
ORDER BY avg_ms DESC;
```
