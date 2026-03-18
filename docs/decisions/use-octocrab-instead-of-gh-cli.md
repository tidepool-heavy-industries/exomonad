# Use octocrab instead of gh CLI for GitHub API operations

**Status:** Implemented (commit ca10eee4)

## Context

ExoMonad previously relied on shelling out to the `gh` CLI for most GitHub operations, including pull request creation (`file_pr`), merging (`merge_pr`), and status polling (`github_poller`). This approach had several drawbacks:

1.  **Fragility**: Parsing unstructured text or JSON output from a CLI tool is error-prone and sensitive to version changes.
2.  **Subprocess Overhead**: Frequent subprocess spawning adds latency and resource consumption, especially in the background poller.
3.  **Error Handling**: CLI exit codes and stderr messages are less expressive than typed Rust `Result` types.
4.  **I/O Model**: Shelling out is inherently synchronous or requires complex `spawn_blocking` wrappers, making it difficult to integrate with the async `tokio` runtime used by the ExoMonad server.

## Decision

Migrate core GitHub API operations within the ExoMonad server code from the `gh` CLI to [octocrab](https://github.com/XAMPPRocky/octocrab), a high-level, async GitHub API client for Rust.

The migration covers:
*   **PR Management**: `file_pr` and `merge_pr` now use octocrab for all API calls.
*   **Status Polling**: The `github_poller` background service uses octocrab to check PR review status and CI results.
*   **Authentication**: These operations now require a `GITHUB_TOKEN` environment variable for authentication, moving away from the implicit `gh auth` state.

Note: Some specialized operations, such as the `copilot_review` service (which handles synchronous waiting for Copilot comments), still use the `gh` CLI as an interim measure until they can be fully ported to octocrab.

## Consequences

*   **Type Safety**: API requests and responses for migrated paths are now type-checked at compile time, reducing runtime parsing errors.
*   **Proper Error Handling**: GitHub API errors in migrated paths are captured as structured `octocrab::Error` types, allowing for more granular error reporting.
*   **Async I/O**: Migrated operations are fully integrated with the `tokio` async runtime, eliminating `spawn_blocking` for those paths.
*   **Reduced Subprocess Overhead**: API calls for PR management and polling are made directly via `hyper` (used by octocrab), reducing resource usage.
*   **Hybrid Tooling**: The `gh` CLI remains a prerequisite for the ExoMonad server (validated at startup) and is still used by the `copilot_review` service. It also remains the primary tool for human operators and Claude Code's native GitHub tools.
*   **Zero Logic in Rust**: In accordance with the project's core mandates, the *logic* of when to call these operations remains in Haskell WASM. Rust merely provides the I/O implementation for the GitHub effect namespace.
