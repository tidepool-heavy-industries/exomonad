# ssh-proxy

SSH Proxy for remote command execution in agent containers.

## Build and Run

```bash
# Build
cargo build -p ssh-proxy

# Run
export SSH_PROXY_KEY_PATH="/path/to/private/key"
export SSH_PROXY_PORT="7433"
cargo run -p ssh-proxy
```

## API

### POST /exec

Executes a command in a target container via SSH.

**Request Body:**
```json
{
  "container": "agent-id",
  "command": "cabal",
  "args": ["build"],
  "working_dir": "/worktrees/my-worktree",
  "env": {
    "PATH": "/usr/local/bin:/usr/bin"
  },
  "timeout_secs": 300
}
```

**Response (Success):**
```json
{
  "exit_code": 0,
  "stdout": "...",
  "stderr": "..."
}
```

**Response (Error):**
```json
{
  "error": "SSH connection failed: ...",
  "container": "agent-id"
}
```

## Architecture

- **Axum**: HTTP server for the API.
- **Connection Pool**: Maintains a `HashMap` of `Arc<Client>` connections to Docker containers.
- **async-ssh2-tokio**: SSH client library (uses `russh` backend).
- **Parallelism**: Connections are shared via `Arc`, allowing multiple commands to run in parallel across different containers, and potentially multiple channels on the same container connection.
- **Shell Escaping**: Uses `shell-escape` to prevent command injection.

## Environment Variables

- `SSH_PROXY_KEY_PATH`: Path to the SSH private key used to connect to agents. Default: `/etc/ssh-proxy/orchestrator_key`.
- `SSH_PROXY_PORT`: Port for the HTTP server. Default: `7433`.
- `RUST_LOG`: Logging level (e.g., `info`, `debug`).
