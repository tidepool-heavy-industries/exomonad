# TUI Spawner

FIFO-based popup spawning for cross-container TUI interactions.

## Purpose

Bridges the gap between the Haskell control-server and the Zellij container for TUI popups. When an MCP tool needs user input, control-server shells out to `tui-spawner`, which handles:

1. Writing the popup definition to a shared volume
2. Creating a FIFO for the result
3. Spawning a floating Zellij pane with `tui-popup`
4. Blocking until the user responds
5. Returning the result to stdout (for Haskell to parse)

## Architecture

```
control-server (in tidepool-control-server container)
    │
    │ subprocess: tui-spawner --definition '{"title":"..."}'
    ▼
tui-spawner
    │
    ├─ 1. Generate UUID for this popup
    ├─ 2. Write definition to /sockets/popup-{uuid}-in.json
    ├─ 3. mkfifo /sockets/popup-{uuid}.fifo
    ├─ 4. docker exec -u 1000 tidepool-zellij zellij action new-pane \
    │      --floating --close-on-exit -- \
    │      tui-popup --input /sockets/popup-{uuid}-in.json \
    │                --output /sockets/popup-{uuid}.fifo
    │
    ├─ 5. open(fifo, O_RDONLY) ← BLOCKS until writer connects
    ├─ 6. read() ← BLOCKS until tui-popup writes + exits
    │
    ├─ 7. Cleanup: rm input file + fifo
    └─ 8. Print result JSON to stdout
    │
    ▼
control-server parses stdout as PopupResult
```

**Why FIFO?** Named pipes provide clean blocking semantics:
- No polling required - kernel handles synchronization
- Writer crash → reader gets EOF (empty read)
- Reader can timeout on open (handles crash-before-open)

**Socket Ownership:** The `docker exec -u 1000` flag ensures tui-popup runs as UID 1000 (user), not root. This prevents permission issues in /sockets volume:
- tui-spawner runs as UID 1000 in control-server
- Without `-u 1000`, docker exec defaults to root
- Root-created files are unwritable by UID 1000
- This declarative approach avoids ad-hoc `chown` fixes

## Usage

```bash
# Called by Haskell TUIInterpreter
tui-spawner --definition '{"title":"Confirm","elements":[...]}'

# With custom sockets directory
tui-spawner --sockets-dir /custom/path --definition '...'
```

## CLI Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `--definition <json>` | (stdin) | PopupDefinition JSON |
| `--sockets-dir <path>` | `/sockets` | Directory for input file and FIFO |
| `--timeout <secs>` | `300` | Timeout for FIFO open (handles crash-before-open) |

## Error Handling

| Scenario | Behavior |
|----------|----------|
| tui-popup crashes before connecting | FIFO open times out after `--timeout` |
| tui-popup crashes after connecting | Empty read from FIFO, returns error result |
| User closes pane (Esc) | Normal cancel result |
| docker exec fails | Exit with error, cleanup files |

## Environment

| Variable | Required | Description |
|----------|----------|-------------|
| (none) | | tui-spawner uses hardcoded paths and docker exec |

## Docker Setup

tui-spawner runs in the `control-server` container and needs:
- Docker CLI access (for `docker exec`)
- Shared `/sockets` volume with zellij container

```yaml
# docker-compose.yml
control-server:
  volumes:
    - tidepool-sockets:/sockets
    - /var/run/docker.sock:/var/run/docker.sock

zellij:
  volumes:
    - tidepool-sockets:/sockets
```

## Building

```bash
cargo build --release -p tui-spawner
```

Binary output: `target/release/tui-spawner`

## Docker Installation

The binary is installed in the `tidepool-control-server` container:

```dockerfile
# docker/control-server/Dockerfile
COPY --from=rust-builder /build/target/release/tui-spawner /usr/local/bin/tui-spawner
```

## Haskell Integration

The Haskell `TUIInterpreter` shells out to tui-spawner:

```haskell
-- TUIInterpreter.hs
spawnPopupFifo :: PopupDefinition -> IO PopupResult
spawnPopupFifo definition = do
  let json = encode definition
  (exitCode, stdout, stderr) <- readProcessWithExitCode
    "tui-spawner"
    ["--definition", T.unpack $ TE.decodeUtf8 $ LBS.toStrict json]
    ""
  case exitCode of
    ExitSuccess -> case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 $ T.pack stdout) of
      Right result -> pure result
      Left err -> pure $ errorResult err
    ExitFailure _ -> pure $ errorResult stderr
```

## Troubleshooting

### "docker exec failed"

tui-spawner can't reach the zellij container.

**Check:**
- Is `tidepool-zellij` running? (`docker ps`)
- Does control-server have `/var/run/docker.sock` mounted?

### "Timeout waiting for TUI"

FIFO open timed out - tui-popup never started or crashed immediately.

**Check:**
- Is `tui-popup` installed in zellij container?
- Check zellij container logs: `docker logs tidepool-zellij`

### "Empty result"

tui-popup crashed after opening the FIFO but before writing.

**Check:**
- tui-popup logs in the Zellij pane (if visible)
- Ensure `/dev/tty` is accessible in the pane

### Files left in /sockets

Cleanup failed (crash or kill -9).

**Fix:** Manually remove: `rm /sockets/popup-*.json /sockets/popup-*.fifo`

## Related Documentation

- **[rust/tui-popup/CLAUDE.md](../tui-popup/CLAUDE.md)** - The popup renderer
- **[haskell/control-server/CLAUDE.md](../../haskell/control-server/CLAUDE.md)** - TUIInterpreter integration
- **[rust/tui-sidebar/CLAUDE.md](../tui-sidebar/CLAUDE.md)** - Component library
