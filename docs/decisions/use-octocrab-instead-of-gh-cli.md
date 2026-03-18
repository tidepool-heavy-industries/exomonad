# Use octocrab instead of gh CLI for GitHub API operations

**Status:** Implemented (commit ca10eee4)

## Context

ExoMonad previously relied on shelling out to the `gh` CLI for all GitHub operations, including pull request creation (`file_pr`), merging (`merge_pr`), and status polling (`github_poller`). This approach had several drawbacks:

1.  **Fragility**: Parsing unstructured text or JSON output from a CLI tool is error-prone and sensitive to version changes.
2.  **Subprocess Overhead**: Frequent subprocess spawning adds latency and resource consumption, especially in the background poller.
3.  **Error Handling**: CLI exit codes and stderr messages are less expressive than typed Rust `Result` types.
4.  **I/O Model**: Shelling out is inherently synchronous or requires complex `spawn_blocking` wrappers, making it difficult to integrate with the async `tokio` runtime used by the ExoMonad server.

## Decision

Migrate all GitHub API operations within the ExoMonad server code from the `gh` CLI to [octocrab](https://github.com/XAMPPRocky/octocrab), a high-level, async GitHub API client for Rust.

The migration covers:
*   **PR Management**: `file_pr` and `merge_pr` now use octocrab for all API calls.
*   **Status Polling**: The `github_poller` background service uses octocrab to check PR review status and CI results.
*   **Authentication**: The server now requires a `GITHUB_TOKEN` environment variable for authentication, moving away from the implicit `gh auth` state.

## Consequences

*   **Type Safety**: All API requests and responses are now type-checked at compile time, reducing runtime parsing errors.
*   **Proper Error Handling**: GitHub API errors are captured as structured `octocrab::Error` types, allowing for more granular error reporting in effect responses.
*   **Async I/O**: Operations are fully integrated with the `tokio` async runtime, eliminating the need for `spawn_blocking` when interacting with GitHub.
*   **No Subprocess Overhead**: API calls are made directly via `hyper` (used by octocrab), reducing resource usage.
*   **Hybrid Tooling**: The `gh` CLI remains the primary tool for human operators and Claude Code's native GitHub tools. ExoMonad's server code uses octocrab exclusively, but it still interacts with the same git repository and GitHub state.
*   **Zero Logic in Rust**: In accordance with the project's core mandates, the *logic* of when to call these operations remains in Haskell WASM. Rust merely provides the I/O implementation for the GitHub effect namespace.
