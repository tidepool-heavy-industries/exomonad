# ADR 002: Hangar Directory Schema

## Status
Accepted

## Context

The current development setup has pain points:

1. **Rebuild overhead**: Each worktree potentially rebuilds Haskell/Rust binaries
2. **Scattered state**: `.tidepool/` directories in multiple locations
3. **Hardcoded paths**: `spawn_agents`, `process-compose.yaml`, `.mcp.json` use relative paths
4. **Single-repo assumption**: Tooling assumes it's running inside tidepool, not on other projects

We want tidepool to be a **tool** (like XMonad) that can be used on any project, with:
- Pre-built binaries shared across all worktrees
- Centralized runtime state
- Clear separation between tool source and project source

## Decision

**Define a "Hangar" as a workspace envelope with a canonical directory schema.**

### Schema

```
<hangar-root>/
  Hangar.toml                    # Marker + config source-of-truth
  config/                        # User customization
  worktrees/                     # spawn_agents target
    bd-xxx-.../
  runtime/
    tidepool/                    # STABLE tool source (for building binaries)
    bin/                         # Built binaries
      exomonad
      mantle-agent
      tui-sidebar
    state/                       # Runtime (sockets, logs)
      sockets/
      logs/
  repo/                          # DEVELOPMENT checkout (active work)
    <project files>
```

### Detection

Walk up from current working directory looking for `Hangar.toml`. The directory containing it is `HANGAR_ROOT`.

```
cwd = /hangars/tidepool/repo/src/
walk_up() finds /hangars/tidepool/Hangar.toml
HANGAR_ROOT = /hangars/tidepool/
```

### Key Decisions

1. **Marker file**: `Hangar.toml` serves dual purpose (detection + configuration)
2. **No dot-prefix for state**: `runtime/state/` not `.tidepool/` (visibility)
3. **Two checkouts in self-dev**: `runtime/tidepool/` (stable tools) vs `repo/` (active dev)
4. **Binaries under runtime**: `runtime/bin/` keeps build artifacts together
5. **Worktrees at root**: `worktrees/` not nested under runtime (project-level concept)

### Hangar.toml Schema

```toml
[hangar]
name = "tidepool"
version = "0.1"

[runtime]
tool_repo = "runtime/tidepool"
bin_dir = "runtime/bin"
state_dir = "runtime/state"

[project]
repo = "repo"
worktrees = "worktrees"
```

## Consequences

### Positive

- **No rebuild per worktree**: All worktrees share `runtime/bin/` binaries
- **Clear separation**: Tool source vs project source are distinct
- **Portable**: Hangar can wrap any project, not just tidepool
- **Discoverable**: Walk-up detection works from any subdirectory
- **Configurable**: `Hangar.toml` can evolve with new settings

### Negative

- **Migration cost**: Existing setups need restructuring
- **Two checkouts**: Self-development case has apparent duplication
- **New abstraction**: Contributors must understand Hangar concept

### Neutral

- **Path updates**: `spawn_agents`, `process-compose.yaml`, `.mcp.json` need updates
- **Environment**: May still want `HANGAR_ROOT` env var for explicit override

## Implementation

1. **tidepool-kg6**: Update `spawn_agents` to use `<hangar>/worktrees/`
2. **tidepool-hp4**: Update `process-compose.yaml` to use `<hangar>/runtime/bin/`
3. **tidepool-ai5**: Update `.mcp.json` to use `<hangar>/runtime/bin/mantle-agent`

## Alternatives Considered

### Global ~/.local/bin installation
**Rejected**: Doesn't support multiple project-specific configurations

### Symlinks everywhere
**Rejected**: Fragile, hard to reason about, breaks on path changes

### Docker/Nix isolation
**Rejected**: Heavyweight for this use case, adds complexity

## References

- XMonad configuration model (~/.xmonad/)
- Cargo workspace layout
- Git worktree patterns
