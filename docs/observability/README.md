# ExoMonad Observability

This directory documents the observability infrastructure for ExoMonad, powered by OpenObserve.

## Architecture

ExoMonad uses a dual-storage approach for observability:
1.  **OpenTelemetry Traces (OpenObserve):** Used as a queryable index to find sessions of interest (e.g., "show me all sessions that errored").
2.  **JSONL Transcripts (Filesystem):** The authoritative source of truth for full session history, referenced by traces.

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
