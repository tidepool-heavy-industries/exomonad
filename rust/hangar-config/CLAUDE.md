# hangar-config

Rust library for discovering and parsing Hangar.toml configuration.

## Purpose

Provides walk-up directory discovery for Hangar environments, eliminating the need for `HANGAR_ROOT` environment variable.

## Usage

```rust
use hangar_config::Hangar;

// Discover Hangar from current directory
let hangar = Hangar::discover()?;

// Get paths
let bin_dir = hangar.bin_dir();
let control_server = hangar.bin_path("exomonad");
let env_file = hangar.env_file();

println!("Hangar root: {}", hangar.root.display());
println!("Binary: {}", control_server.display());
```

## Discovery Algorithm

1. Start from current working directory
2. Check for `Hangar.toml` in current directory
3. If not found, move to parent directory
4. Stop at filesystem root (`/`) or home directory (`~`)
5. Return error if no `Hangar.toml` found

## Configuration Schema

```toml
[hangar]
name = "tidepool"
version = "0.1"

[project]
repo = "repo"
worktrees = "worktrees"

[runtime]
tool_source = "runtime/tidepool"
bin = "runtime/bin"

[auth]
env_file = ".env"
```

## Error Handling

```rust
match Hangar::discover() {
    Ok(hangar) => {
        // Use hangar.bin_dir(), etc.
    }
    Err(HangarError::NotFound { start_dir }) => {
        eprintln!("No Hangar.toml found from {}", start_dir.display());
    }
    Err(e) => {
        eprintln!("Hangar error: {}", e);
    }
}
```

## Integration

Used by:
- `tui-sidebar` - Discovers binary paths without HANGAR_ROOT
- `mantle-agent` - (planned) MCP server binary discovery
- `process-compose.yaml` - (via bash discovery in command)
- `start-augmented.sh` - (via bash discovery)

## Future

- [ ] Cache discovered Hangar to avoid repeated walks
- [ ] Support `HANGAR_ROOT` env override (skip discovery)
- [ ] Validate Hangar.toml schema versions
- [ ] Binary version checking
