# ExoMonad Observability

This directory documents the observability infrastructure for ExoMonad, powered by OpenObserve.

## Architecture

ExoMonad uses a dual-storage approach for observability:
1.  **OpenTelemetry Traces (OpenObserve):** Used as a queryable index to find sessions of interest (e.g., "show me all sessions that errored").
2.  **JSONL Transcripts (OpenObserve & Filesystem):** Full session transcripts (JSONL) are automatically shipped to OpenObserve when a session ends. This allows for deep full-text search across all agent interactions. The authoritative source of truth still remains on the filesystem for local processing.

### Transcript Shipping

When a Claude Code session ends (`SessionEnd` hook) or a subagent finishes (`SubagentStop` hook), the `exomonad-control-server` reads the transcript JSONL file and POSTs it to the OpenObserve `claude_sessions` stream.

Each event is enriched with:
- `session_id`: The unique Claude session identifier.
- `agent_role`: The role of the agent (e.g., `tl`, `dev`).
- `cwd`: The working directory where the session ran.
- `hook_event`: The hook that triggered the shipping.

## Configuration

The transcript shipping is configured via the following environment variables:

| Variable | Description | Default |
|----------|-------------|---------|
| `OPENOBSERVE_URL` | OpenObserve base URL | `http://localhost:5080` |
| `OPENOBSERVE_ORG` | Organization name | `default` |
| `OPENOBSERVE_STREAM` | Destination stream name | `claude_sessions` |
| `OPENOBSERVE_EMAIL` | Ingestion user email | `admin@exomonad.local` |
| `OPENOBSERVE_PASSWORD` | Ingestion user password | `exomonad-dev` |

## Setup

The OpenObserve service is defined in `docker-compose.yml`.

1.  Start the service:
    ```bash
    docker compose up -d openobserve
    ```

2.  Verify health:
    ```bash
    ./scripts/verify-openobserve.sh
    ```

3.  Access the Web UI:
    - URL: http://localhost:5080
    - Email: `admin@exomonad.local`
    - Password: `exomonad-dev`

## Querying

### CLI Wrapper
Use the `exo-query` script to search traces and get paths to JSONL transcripts.

```bash
# Find sessions with a specific error pattern
./scripts/exo-query "attributes->>'error.pattern' = 'TypeMismatch'"
```

### SQL Queries
See [queries.md](./queries.md) for common SQL analysis patterns.
