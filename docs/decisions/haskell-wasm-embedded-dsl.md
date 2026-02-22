# Haskell WASM as Embedded DSL

**Status:** Accepted

## Decision

All MCP tools, hooks, and decision logic are defined in Haskell, compiled to WASM32-WASI, and loaded by the Rust runtime via Extism. Rust is the I/O runtime: it executes effects that the Haskell DSL yields. Haskell never does I/O. Rust never defines tool schemas or contains tool logic.

This is the entire architectural premise.

## Why

### Type Safety at the Tool Boundary

MCP tool schemas, argument parsing, and dispatch logic are all type-checked at compile time. A tool with a malformed schema or mismatched handler types is a compile error, not a runtime surprise.

```haskell
data Tools mode = Tools
  { agents :: AgentTools mode
  , pr     :: FilePRTools mode
  } deriving Generic
```

The `mode` parameter lets the same record serve as both schema generator (`AsSchema`) and handler dispatcher (`AsHandler`). Invalid compositions are type errors.

### Hot Reload (xmonad Pattern)

The Rust server checks WASM file mtime on each tool call. Edit Haskell, run `exomonad recompile`, and the next tool call picks up new logic — zero downtime, no agent restart. This enables live iteration on tool behavior during an orchestration session.

### IO-Blind Agents

Haskell code yields typed effects (`Git`, `GitHub`, `Zellij`, `Messaging`). It never calls `readProcess`, never opens sockets, never touches the filesystem. All I/O happens in Rust host functions that the WASM plugin calls via FFI.

This makes tools:
- **Testable**: Swap the effect interpreter, run pure
- **Sandboxed**: WASM can't escape its capability set
- **Portable**: Same WASM runs in any host that implements the effect interface

### Unified WASM Module

One WASM module contains all roles (TL, dev). Role-based tool filtering happens in Rust at session init — the server resolves which tools are visible based on the role config baked into the WASM. Hot reload updates all roles atomically (single file swap).

## Architecture

```
Tool call arrives (HTTP)
  → Rust MCP server receives JSON-RPC
  → Dispatches to WASM plugin (Extism)
  → Haskell handler runs:
      - Parses arguments (typed, schema-validated)
      - Executes logic (pure computation + effect yields)
      - Yields effects via host function calls
  → Rust executes each effect:
      - Git operations (Command::new("git"))
      - GitHub API (reqwest)
      - Zellij (plugin pipe)
      - Filesystem, messaging, etc.
  → Result returned to caller
```

### Build Pipeline

1. Role configs in `.exo/roles/unified/` define tool composition per role
2. `AllRoles.hs` registers all roles; `Main.hs` provides FFI exports
3. `just wasm-all` builds via `nix develop .#wasm -c wasm32-wasi-cabal build`
4. Compiled WASM copied to `.exo/wasm/wasm-guest-unified.wasm`
5. `exomonad serve` loads WASM at runtime (hot reload via mtime check)

### Effect System

Uses `freer-simple` (reified continuations). Effects are algebraic — the WASM plugin yields an effect, Rust interprets it, and returns the result to continue the computation.

```haskell
-- Haskell yields this effect
data GitEffect r where
  GitStatus :: GitEffect Text
  GitDiff   :: [Text] -> GitEffect Text
  GitCommit :: Text -> [FilePath] -> GitEffect ()

-- Rust handles it via host function
fn handle_git_effect(effect: &GitEffectProto) -> Result<Vec<u8>> {
    match effect.variant {
        "GitStatus" => { /* Command::new("git").arg("status") */ }
        "GitDiff"   => { /* Command::new("git").arg("diff") */ }
        // ...
    }
}
```

## Consequences

- Every MCP tool is type-safe at compile time
- Hot reload enables live iteration without restarting agents
- WASM sandboxing bounds what tool code can do
- Requires Haskell toolchain (via Nix) to build roles — higher barrier to entry
- WASM compilation is slow (~30s for full rebuild)
- Two-language boundary (Haskell↔Rust) adds conceptual overhead
- All new tools go in `haskell/wasm-guest/src/ExoMonad/Guest/Tools/` — never in Rust

## Why Not Alternatives

**Rust-only tools**: Loses type-level tool composition, hot reload, and the schema-shaped cognition pattern. Tools become ad-hoc handler functions.

**Lua/Python scripting**: Good flexibility, lacks the type safety that catches schema mismatches at compile time.

**JSON/YAML config for tools**: Too limited for complex tool logic (conditional dispatch, effect composition, typed hooks).
