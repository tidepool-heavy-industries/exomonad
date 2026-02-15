# Error Handling Audit Report

**Date**: 2026-02-15
**Scope**: `rust/` directory (crates: `exomonad`, `exomonad-core`, `exomonad-plugin`, `exomonad-proto`)

## Executive Summary

The codebase generally adheres to idiomatic Rust error handling patterns, extensively using `anyhow::Result` for error propagation in application logic. The vast majority (>90%) of `unwrap()` and `expect()` calls are located within test modules, build scripts, or `proto-test` utilities, which is acceptable.

However, a few specific locations in production code rely on `unwrap()` or `expect()` in ways that present minor to moderate risks of runtime panics, particularly regarding configuration validation and concurrency assumptions.

## Detailed Findings

### 1. Critical / High Risk
*None identified.* No obvious paths were found where external user input triggers a direct panic in the main control flow.

### 2. Medium Risk

#### `exomonad-core/src/mcp/server.rs` - Session Retrieval
*   **Location**: `handle_list_tools` (approx line 194) and `handle_call_tool`.
*   **Code**: `let session = sessions.get(session_id).unwrap();`
*   **Context**: The `handle` method first acquires a write lock to update `session.last_active`, ensuring the session exists. It then releases the lock and calls `handle_list_tools`, which acquires a read lock and `unwrap`s the session.
*   **Risk**: Brittle synchronization. While currently safe because the background sweeper respects `last_active`, any future change that introduces another eviction mechanism (e.g., an admin command to kill sessions) could cause a race condition leading to a panic here.
*   **Recommendation**: Change `unwrap()` to `ok_or_else(|| ...)?` or `expect("Session disappeared between check and use")` to be explicit about the invariant.

#### `exomonad-core/src/services/external/ollama.rs` - URL Parsing
*   **Location**: `OllamaService::call` (approx line 24).
*   **Code**: `let host = url.scheme().to_string() + "://" + url.host_str().unwrap();`
*   **Context**: This assumes the configured Ollama URL always has a host.
*   **Risk**: If a user configures `ollama_url` as `file:///path/to/socket` or similar scheme without a host, the service will panic during the call.
*   **Recommendation**: Validate the URL during service construction or handle the `None` case gracefully.

### 3. Low Risk / Acceptable Patterns

#### `exomonad/src/main.rs` - Signal Handling
*   **Location**: `main` and `serve` command initialization.
*   **Code**: `.expect("failed to install SIGTERM handler")`
*   **Context**: Occurs during startup.
*   **Verdict**: Acceptable "Fail Fast" behavior. If the process cannot register signal handlers, it should probably not start.

#### `exomonad-core/src/services/git.rs` - Parsing
*   **Location**: `has_unpushed_commits`.
*   **Code**: `.parse::<u32>().unwrap_or(0)`
*   **Verdict**: Safe. It explicitly handles the error case by defaulting to 0.

#### `exomonad-core/src/protocol/service.rs` & `hook.rs`
*   **Location**: Various `serde_json` serialization/deserialization calls.
*   **Verdict**: All found instances were within `mod tests`. Production deserialization in `main.rs` uses `context(...)`.

### 4. Codebase Statistics

*   **`unwrap()`**: ~417 occurrences (vast majority in `tests/`, `mod tests`, `build.rs`).
*   **`expect()`**: ~39 occurrences (mostly tests, some startup assertions).
*   **`panic!()`**: ~74 occurrences (mostly tests, some "unreachable" branches in exhaustive matches).
*   **`todo!()`**: 1 occurrence (in documentation/comment).

## Recommendations

1.  **Refactor `McpServer` locking**: Consider holding the lock for the duration of the operation or returning the `McpSession` clone (if cheap) / `Arc` from the validation step to avoid looking it up again with `unwrap`.
2.  **Harden `OllamaService`**: Add a check in `new()` to ensure `url.host_str()` is `Some`, or handle it in `call()`.
3.  **Continuous Monitoring**: Ensure new PRs do not introduce `unwrap()` on `serde_json` calls for external input.

