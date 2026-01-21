# Task: Fix: Use Unix Domain Sockets for subagent process-compose to eliminate port 8080 conflicts

**ID:** tidepool-l8p
**Status:** open
**Priority:** 1
**Branch:** bd-l8p/fix-use-unix-domain-sockets-for-subagent-processcompose-to-eliminate-port-8080-conflicts

## Description

Disable HTTP API in worktree process-compose instances and use Unix Domain Sockets exclusively, eliminating TCP port 8080 conflicts when multiple process-compose instances run in parallel worktrees

## Context
When spawning multiple agents, each worktree's process-compose tries to bind to TCP port 8080 for its HTTP API, causing "address already in use" errors. The Hub & Spoke architecture supports Unix Domain Sockets natively via PC_NO_SERVER, PC_USE_UDS, and PC_SOCKET_PATH environment variables.

**Solution:**
1. Set PC_NO_SERVER=true and PC_USE_UDS=true in subagent process-compose environment
2. Set PC_SOCKET_PATH=./.tidepool/sockets/process-compose.sock for predictable socket paths
3. Update start-augmented.sh stale session detection to use `process-compose info -u <socket>` instead of `curl http://localhost:8080`
4. Update any other health checks to verify socket existence with `test -S`

**Benefits:**
- Zero port conflicts
- Lower latency (UDS vs TCP loopback)
- Fully isolated per-worktree state
- Aligns with existing control-server/tui-sidebar socket architecture

## Scope Hint
Medium - configuration + health check update

## Dependencies

None

## Workflow

1. Implement changes
2. Commit: [tidepool-l8p] <description>
3. Push: git push -u origin bd-l8p/fix-use-unix-domain-sockets-for-subagent-processcompose-to-eliminate-port-8080-conflicts
4. File PR: gh pr create --title "[tidepool-l8p] Fix: Use Unix Domain Sockets for subagent process-compose to eliminate port 8080 conflicts"
