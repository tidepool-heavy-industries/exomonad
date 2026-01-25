# Zellij Effect Interpreter

Interprets the Zellij effect by calling the `zellij` CLI. Enables graphs to create tabs for parallel agent orchestration.

## When to Read This

Read this if you're:
- Working on spawn_agents or parallel agent orchestration
- Debugging Zellij tab creation issues
- Understanding cross-container Zellij access in Docker

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ Graph (e.g., SpawnAgentsGraph)                              │
│   newTab TabConfig { tcName, tcLayout, tcCwd, tcEnv }       │
└────────────────────────────┬────────────────────────────────┘
                             │ Zellij effect
                             ▼
┌─────────────────────────────────────────────────────────────┐
│ Zellij Interpreter (runZellijIO)                            │
│   • checkZellijEnv → ZELLIJ or ZELLIJ_SESSION_NAME env      │
│   • newTab → zellij action new-tab ...                      │
│   • goToTab → zellij action go-to-tab-name ...              │
└────────────────────────────┬────────────────────────────────┘
                             │ CLI subprocess
                             ▼
┌─────────────────────────────────────────────────────────────┐
│ Zellij Process (local or remote session)                    │
│   Socket: $XDG_RUNTIME_DIR/zellij/<version>/<session>       │
└─────────────────────────────────────────────────────────────┘
```

## Effect Operations

| Operation | Description | CLI Command |
|-----------|-------------|-------------|
| `CheckZellijEnv` | Check if Zellij access is available | Reads `ZELLIJ` or `ZELLIJ_SESSION_NAME` env |
| `NewTab config` | Create a new tab with layout | `zellij [--session NAME] action new-tab --layout ... --cwd ... --name ...` |
| `GoToTab tabId` | Switch focus to a tab | `zellij [--session NAME] action go-to-tab-name ...` |

## Cross-Container Access (Docker)

In Docker deployments, the `control-server` container needs to create Zellij tabs in the `orchestrator` container. This is achieved by:

1. **Shared Zellij socket volume**: Both containers mount `tidepool-zellij` at `/run/user/1000`
2. **Session targeting**: `ZELLIJ_SESSION_NAME=orchestrator` enables `--session` flag
3. **Zellij binary in control-server**: Installed via `cargo-binstall`

### Environment Variables

| Variable | Description | Example |
|----------|-------------|---------|
| `ZELLIJ` | Set when running inside a Zellij pane | Auto-set by Zellij |
| `ZELLIJ_SESSION_NAME` | Target session for cross-container access | `orchestrator` |
| `XDG_RUNTIME_DIR` | Zellij socket directory | `/run/user/1000` |

### Session Prefix Logic

The interpreter automatically adds `--session <name>` when:
- `ZELLIJ` is NOT set (not inside a pane)
- `ZELLIJ_SESSION_NAME` IS set (cross-container mode)

```haskell
getSessionPrefix :: IO [String]
getSessionPrefix = do
  zellijEnv <- lookupEnv "ZELLIJ"
  case zellijEnv of
    Just _ -> pure []  -- Inside Zellij pane, no prefix needed
    Nothing -> do
      sessionName <- lookupEnv "ZELLIJ_SESSION_NAME"
      pure $ case sessionName of
        Just name -> ["--session", name]
        Nothing -> []
```

### Docker Volume Setup

In `docker-compose.yml`:
```yaml
volumes:
  tidepool-zellij:
    name: tidepool-zellij

services:
  orchestrator:
    volumes:
      - tidepool-zellij:/run/user/1000
    environment:
      - XDG_RUNTIME_DIR=/run/user/1000

  control-server:
    volumes:
      - tidepool-zellij:/run/user/1000
    environment:
      - XDG_RUNTIME_DIR=/run/user/1000
      - ZELLIJ_SESSION_NAME=orchestrator
```

## Error Types

| Error | Cause | Resolution |
|-------|-------|------------|
| `ZellijLayoutNotFound` | Layout file doesn't exist | Check `.zellij/worktree.kdl` exists |
| `ZellijCommandFailed` | CLI returned non-zero exit | Check Zellij session is running |

## Usage Example

```haskell
import Tidepool.Zellij.Interpreter (runZellijIO)
import Tidepool.Effects.Zellij

main = runM $ runZellijIO $ do
  mSession <- checkZellijEnv
  case mSession of
    Nothing -> error "Not in Zellij and no session name configured"
    Just _ -> do
      let cfg = TabConfig "worker" ".zellij/worktree.kdl" "/cwd" [("X", "Y")]
      newTab cfg
```

## Requirements

- **Local mode**: Running inside a Zellij session (`ZELLIJ` env var set)
- **Cross-container mode**: `ZELLIJ_SESSION_NAME` set + shared `XDG_RUNTIME_DIR` + `zellij` binary installed

## Related Documentation

- **[control-server/CLAUDE.md](../../control-server/CLAUDE.md)** - MCP tools using Zellij
- **[ExoTools/CLAUDE.md](../../control-server/src/Tidepool/Control/ExoTools/CLAUDE.md)** - spawn_agents implementation
- **[Root CLAUDE.md](../../../CLAUDE.md)** - Docker compose setup
