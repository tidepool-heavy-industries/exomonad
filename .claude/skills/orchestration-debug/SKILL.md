---
name: orchestration-debug
description: Use when debugging Zellij, process-compose, or ExoMonad orchestration issues like port conflicts, stale sessions, services not starting, or trap handler failures.
---

# Orchestration Debugging

Debug the Hybrid ExoMonad stack: process-compose + Zellij + services.

## Quick Diagnostics

```bash
# Check if services are running
pgrep -f "control-server|tui-sidebar|process-compose"

# Check port usage
lsof -i :7432  # control-server main
lsof -i :7433  # tui-sidebar
lsof -i :7434  # control-server health
lsof -i :8080  # process-compose API

# Check process-compose status
curl -sf http://localhost:8080/processes | jq .

# Check control-server health
curl -sf http://localhost:7434
```

## Port Allocation

| Port | Service | Protocol | If Blocked |
|------|---------|----------|------------|
| 7432 | control-server | TCP NDJSON | MCP tools fail to connect |
| 7433 | tui-sidebar | TCP NDJSON | TUI won't render |
| 7434 | control-server | HTTP | Health probes fail, tui-sidebar won't start |
| 8080 | process-compose | HTTP | Stale session detection fails |

## Common Issues

### 1. Stale Session (Orphaned Processes)

**Symptoms**: `start-augmented.sh` hangs or services conflict

**Diagnosis**:
```bash
# Check for orphaned processes
pgrep -f "control-server|tui-sidebar|process-compose"

# Check if process-compose API responds
curl -sf http://localhost:8080/hostname
```

**Fix**:
```bash
# Graceful shutdown
~/.local/bin/process-compose down --ordered-shutdown

# Nuclear option if above fails
pkill -f "control-server|tui-sidebar|process-compose"
```

### 2. tui-sidebar Won't Start (Depends on control-server)

**Symptoms**: tui-sidebar stays pending, never reaches healthy

**Diagnosis**:
```bash
# Check if control-server health endpoint responds
curl -sf http://localhost:7434
echo $?  # 0 = healthy, non-zero = problem

# Check control-server logs
tail -f .exo/logs/control-server.log
```

**Cause**: tui-sidebar has `depends_on: control-server: condition: process_healthy`

**Fix**: Wait for control-server to become healthy, or check its logs for errors.

### 3. Trap Handler Didn't Fire (Services Running After Zellij Exit)

**Symptoms**: Exited Zellij but services still running

**Diagnosis**:
```bash
# Check if services survived
pgrep -f "control-server|tui-sidebar"
```

**Cause**: Zellij detached instead of quitting (missing `on_force_close "quit"`)

**Verify config**:
```bash
cat .zellij/config.kdl | grep on_force_close
# Should show: on_force_close "quit"
```

**Fix**:
```bash
# 1. Clean up orphans
~/.local/bin/process-compose down --ordered-shutdown

# 2. Ensure config is correct
echo 'on_force_close "quit"' >> .zellij/config.kdl
```

### 4. MCP Tools Show "Failed" on Startup

**Symptoms**: `/mcp` shows tools as failed immediately after startup

**Cause**: control-server not ready when Claude tried to connect

**Fix**: Wait for services to be healthy, then use `/mcp` → `Reconnect`

### 5. Health Probe Timeout

**Symptoms**: control-server never becomes healthy, 30 failures in logs

**Diagnosis**:
```bash
# Check if HLS is building (takes time on first run)
ps aux | grep haskell-language-server

# Check control-server stderr
cat .exo/logs/control-server.log | grep -i error
```

**Cause**: HLS initialization takes longer than probe timeout (30s × 30 attempts = 15 min max)

**Fix**: Increase `failure_threshold` in `process-compose.yaml` if HLS needs more time.

## Shutdown Flow Verification

Expected sequence when pressing `Ctrl+P → q` in Zellij:

1. Zellij receives quit command
2. `on_force_close "quit"` prevents detach
3. Zellij exits, triggering `exomonad-runner.sh` EXIT trap
4. Trap calls `process-compose down --ordered-shutdown`
5. Services stop in reverse dependency order (tui-sidebar → control-server)

**Verify**:
```bash
# After Zellij exit, nothing should be running
pgrep -f "control-server|tui-sidebar|process-compose"
# Should return nothing (exit code 1)
```

## Log Locations

| Log | Path |
|-----|------|
| process-compose | `.exo/logs/process-compose.log` |
| control-server | stdout in process-compose TUI |
| tui-sidebar | stdout in process-compose TUI |

## Config File Locations

| File | Purpose |
|------|---------|
| `process-compose.yaml` | Service definitions, health probes |
| `.zellij/exomonad.kdl` | 3-pane layout |
| `.zellij/config.kdl` | Zellij behavior (`on_force_close`) |
| `scripts/exomonad-runner.sh` | Trap handlers |
| `start-augmented.sh` | Entry point, validation |

## Emergency Recovery

If everything is broken:

```bash
# 1. Kill everything
pkill -9 -f "control-server|tui-sidebar|process-compose|zellij"

# 2. Clean runtime directories
rm -rf .exo/sockets/* .exo/logs/*

# 3. Verify ports are free
lsof -i :7432,:7433,:7434,:8080

# 4. Fresh start
./start-augmented.sh
```
