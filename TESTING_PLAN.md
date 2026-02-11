# Phase 1 Hardening Plan

Make hylo phase 1 robust and tested before dogfooding.

## Current Test Coverage

**What's tested (unit — 226 tests across 34 files):**
- `detect_base_branch` — `/`, `.`, explicit, fallback to `main`
- `slugify`, `escape_for_shell_command`, `parse_agent_dir_name`
- `AgentType` — command, emoji, suffix, display_name, deserialization
- `build_initial_prompt`, agent identity task-local scope, file PR URL parsing

**What's tested (integration):**
- WASM: Rust host ↔ Haskell WASM effect pipeline (mock handlers)
- MCP HTTP: JSON-RPC over HTTP → WASM → effects → response

**What's NOT tested:**
- Branch naming ↔ PR detection roundtrip (spawn creates `base.child`, file_pr resolves `base`)
- `write_agent_mcp_config` output format (Claude vs Gemini JSON)
- `cleanup_agent` path reconstruction (tmp dir + config dir + worktree)
- `parse_agent_tab` for subtree agents (non-`gh-*` naming)
- Tool schema correctness via MCP (spawn_worker params, no spawn_leaf)
- spawn_worker WASM effect roundtrip
- End-to-end spawn → work → PR → cleanup (requires Zellij)

## Test Plan

### Wave 1: Pure Logic Unit Tests (no I/O)

All run via `cargo test`. No Zellij, no server, no network.

#### 1.1 Branch naming ↔ PR detection roundtrip

Verify `spawn_gemini_teammate`'s branch format is correctly parsed by `detect_base_branch`.

```rust
// file_pr.rs tests
fn test_branch_naming_roundtrip_root() {
    let branch = format!("{}.{}", "main", "auth-service");
    assert_eq!(detect_base_branch(&branch, None), "main");
}

fn test_branch_naming_roundtrip_nested() {
    let branch = format!("{}.{}", "main.auth-service", "middleware");
    assert_eq!(detect_base_branch(&branch, None), "main.auth-service");
}

fn test_branch_naming_roundtrip_deep() {
    assert_eq!(detect_base_branch("main.feature.sub.leaf", None), "main.feature.sub");
}
```

**File:** `rust/exomonad-core/src/services/file_pr.rs`

#### 1.2 MCP config output validation

Extract JSON generation from `write_agent_mcp_config` into a pure `fn generate_mcp_config(name: &str, port: u16, agent_type: AgentType) -> String`. Test both variants.

```rust
fn test_claude_mcp_config_format() {
    let config = generate_mcp_config("test-claude", 7432, AgentType::Claude);
    let parsed: Value = serde_json::from_str(&config).unwrap();
    assert_eq!(parsed["mcpServers"]["exomonad"]["url"],
        "http://localhost:7432/agents/test-claude/mcp");
    assert!(parsed["mcpServers"]["exomonad"].get("httpUrl").is_none());
}

fn test_gemini_mcp_config_format() {
    let config = generate_mcp_config("test-gemini", 7432, AgentType::Gemini);
    let parsed: Value = serde_json::from_str(&config).unwrap();
    assert_eq!(parsed["mcpServers"]["exomonad"]["httpUrl"],
        "http://localhost:7432/agents/test-gemini/mcp");
    assert!(parsed["mcpServers"]["exomonad"].get("url").is_none());
}
```

**File:** `rust/exomonad-core/src/services/agent_control.rs`
**Refactor:** Extract pure fn from async `write_agent_mcp_config`

#### 1.3 `parse_agent_tab` for subtree agents

Currently only tested for `gh-*` naming. Subtree agents use slugified names.

```rust
fn test_parse_agent_tab_subtree_claude() {
    let info = parse_agent_tab("🤖 auth-service").unwrap();
    assert_eq!(info.issue_id, "auth-service");
    assert_eq!(info.agent_type, Some("claude".to_string()));
    assert_eq!(info.topology, Topology::SharedDir);
}

fn test_parse_agent_tab_subtree_gemini() {
    let info = parse_agent_tab("💎 data-migration").unwrap();
    assert_eq!(info.issue_id, "data-migration");
    assert_eq!(info.agent_type, Some("gemini".to_string()));
}

fn test_parse_agent_tab_not_agent() {
    assert!(parse_agent_tab("Server").is_none());
    assert!(parse_agent_tab("TL").is_none());
}
```

**File:** `rust/exomonad-core/src/services/agent_control.rs`

### Wave 2: MCP Integration Tests

Extend `rust/exomonad/tests/mcp_integration.rs`. Requires running server (`just test-mcp`).

#### 2.1 Tool schema validation
- `spawn_subtree` has params: task, branch_name, context (optional), agent_type (optional)
- `spawn_worker` has params: name, prompt — NO agent_type
- `file_pr` has params: title, body, base_branch (optional)
- `get_agent_messages` accepts timeout_secs
- `spawn_leaf` is NOT in tools/list

#### 2.2 Fix stale doc comment
Line 9 of mcp_integration.rs still references `spawn_leaf`. Update to `spawn_worker`.

### Wave 3: WASM

#### 3.1 Build verification
```bash
just wasm-all  # Must succeed cleanly
```

#### 3.2 spawn_worker effect roundtrip
Extend `rust/exomonad-core/tests/wasm_integration.rs` — mock agent handler accepts `spawn_worker` effect, returns valid SpawnWorkerResponse.

### Wave 4: Manual Smoke Test

After waves 1-3 pass:

1. `exomonad init --recreate`
2. Call `spawn_subtree` with simple task
3. Verify: worktree at `.exomonad/worktrees/{slug}-{type}`, branch `{current}.{slug}`, Zellij tab opens, agent has MCP tools
4. From subtree agent, call `file_pr` — verify PR targets parent branch
5. Call `cleanup_agent` — verify worktree + tab + config dirs removed
6. Call `spawn_worker` — verify pane opens in current dir, no worktree

## Execution

```
Wave 1 (unit)     — no deps, parallelizable
Wave 2 (MCP)      — needs server (just test-mcp handles lifecycle)
Wave 3 (WASM)     — needs nix develop .#wasm
Wave 4 (manual)   — after automated tests pass
```

## Files to Modify

| File | Changes |
|------|---------|
| `rust/exomonad-core/src/services/file_pr.rs` | Add roundtrip tests (1.1) |
| `rust/exomonad-core/src/services/agent_control.rs` | Extract `generate_mcp_config` pure fn, add tests (1.2, 1.3) |
| `rust/exomonad/tests/mcp_integration.rs` | Schema validation tests (2.1), fix stale spawn_leaf ref (2.2) |
| `rust/exomonad-core/tests/wasm_integration.rs` | spawn_worker mock handler (3.2) |

## Success Criteria

- `cargo test --workspace` green with new tests
- `just test-mcp` green with schema checks
- `just wasm-all` builds clean
- Manual smoke test passes
- Zero `spawn_leaf` references in test code
