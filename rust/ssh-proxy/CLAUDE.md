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

## Manual Testing

**Check Health:**
```bash
curl http://localhost:7433/health
# Output: ok
```

**Execute Command:**
```bash
curl -X POST http://localhost:7433/exec \
  -H "Content-Type: application/json" \
  -d '{
    "container": "test-agent",
    "command": "echo",
    "args": ["hello world"],
    "working_dir": "/tmp"
  }'
```

## Troubleshooting

### Connection Refused (SSH)
- **Error**: `SSH connection failed: Connection refused`
- **Cause**: The agent container is not running `sshd`, or it is not listening on port 22.
- **Fix**: Verify `sshd` is running in the agent container (`service ssh status`).

### Host Unreachable
- **Error**: `SSH connection failed: No route to host`
- **Cause**: The container hostname (e.g., `agent-id.docker.internal`) cannot be resolved.
- **Fix**: Ensure the `ssh-proxy` and agent are on the same Docker network, or that internal DNS is working.

### Permission Denied (Public Key)
- **Error**: `SSH connection failed: Authentication failed`
- **Cause**: The key at `SSH_PROXY_KEY_PATH` does not match the public key in the agent's `authorized_keys`.
- **Fix**: Re-deploy the keys or check permissions on the key file (should be 600).
