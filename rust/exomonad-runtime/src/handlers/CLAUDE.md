# Effect Handlers

Namespace-based effect handlers for the extensible effects system. Each handler owns a namespace and routes effects to the underlying service.

## Architecture

```
yield_effect "git.get_branch" {...}
    │
    │ EffectRegistry::dispatch
    ▼
GitHandler (namespace = "git")
    │
    │ match "get_branch"
    ▼
GitService::get_branch(dir)
```

## Handlers

| Handler | Namespace | Effects |
|---------|-----------|---------|
| `GitHandler` | `git` | get_branch, get_status, get_commits, has_unpushed_commits, get_remote_url, get_repo_info, get_worktree |
| `GitHubHandler` | `github` | list_issues, get_issue, list_pull_requests, get_pull_request, get_pull_request_for_branch, create_pull_request |
| `LogHandler` | `log` | info, error, debug, warn, emit_event |

## Usage

Handlers are registered with the `RuntimeBuilder`:

```rust
use exomonad_runtime::{RuntimeBuilder, Services};

let services = Arc::new(Services::new().validate()?);
let runtime = RuntimeBuilder::new()
    .with_builtin_handlers(&services)
    .with_wasm_path("plugin.wasm")
    .build()
    .await?;
```

## Adding a New Handler

1. Create a new file in `handlers/` (e.g., `handlers/myns.rs`)
2. Implement the `EffectHandler` trait:
   ```rust
   #[async_trait]
   impl EffectHandler for MyHandler {
       fn namespace(&self) -> &str { "myns" }
       async fn handle(&self, effect_type: &str, payload: Value) -> EffectResult<Value> {
           // Route by suffix
       }
   }
   ```
3. Add to `handlers/mod.rs`
4. Register in `RuntimeBuilder::with_builtin_handlers` (or as custom handler)

## Request/Response Types

Handlers define their own request/response types matching the proto definitions:

```rust
#[derive(Debug, Deserialize)]
struct GetBranchRequest {
    working_dir: String,
}

#[derive(Debug, Serialize)]
struct GetBranchResponse {
    branch: String,
    detached: bool,
}
```

These types are serialized/deserialized via serde_json for the WASM boundary.
