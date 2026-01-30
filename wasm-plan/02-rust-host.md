# Rust Host Architecture

The thick runtime that handles all IO. Loads Haskell WASM, dispatches effects.

## Package Structure

```
rust/exomonad-runtime/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── main.rs              # HTTP server entry point
│   ├── plugin.rs            # Extism plugin lifecycle
│   ├── host_functions.rs    # Host function registration
│   ├── dispatch.rs          # Effect → service routing
│   │
│   ├── generated/           # quicktype output
│   │   └── effects.rs
│   │
│   ├── services/            # IO implementations
│   │   ├── mod.rs
│   │   ├── git.rs           # docker exec + git
│   │   ├── github.rs        # octocrab
│   │   ├── docker.rs        # docker-ctl integration
│   │   ├── lsp.rs           # tower-lsp client
│   │   ├── telemetry.rs     # tracing + OTLP
│   │   └── tui.rs           # tui-spawner
│   │
│   └── server/              # HTTP/MCP layer
│       ├── mod.rs
│       ├── routes.rs        # axum routes
│       ├── mcp.rs           # MCP protocol handler
│       └── hooks.rs         # Hook protocol handler
```

## Dependencies

```toml
[dependencies]
# Extism
extism = "1.0"

# Async runtime
tokio = { version = "1", features = ["full"] }

# HTTP server
axum = "0.7"
tower = "0.4"

# Serialization (matches quicktype output)
serde = { version = "1", features = ["derive"] }
serde_json = "1"

# Services
octocrab = "0.32"          # GitHub API
bollard = "0.16"           # Docker API (alternative to shelling out)
tower-lsp = "0.20"         # LSP client

# Observability
tracing = "0.1"
tracing-subscriber = "0.3"
opentelemetry = "0.21"
```

## Plugin Lifecycle

```rust
// plugin.rs
use extism::{Plugin, Manifest, Wasm};
use std::sync::Arc;
use tokio::sync::RwLock;

pub struct PluginManager {
    plugin: Arc<RwLock<Plugin>>,
    wasm_path: PathBuf,
}

impl PluginManager {
    pub async fn new(wasm_path: PathBuf, services: Arc<Services>) -> Result<Self> {
        let plugin = Self::load_plugin(&wasm_path, services).await?;
        Ok(Self {
            plugin: Arc::new(RwLock::new(plugin)),
            wasm_path,
        })
    }

    async fn load_plugin(path: &Path, services: Arc<Services>) -> Result<Plugin> {
        let wasm = Wasm::file(path);
        let manifest = Manifest::new([wasm]);

        Plugin::new(&manifest, host_functions(services), true)
    }

    /// Hot reload: load new WASM without restarting runtime
    pub async fn reload(&self, services: Arc<Services>) -> Result<()> {
        let new_plugin = Self::load_plugin(&self.wasm_path, services).await?;
        let mut guard = self.plugin.write().await;
        *guard = new_plugin;
        Ok(())
    }

    /// Call an exported function
    pub async fn call<I: Serialize, O: DeserializeOwned>(
        &self,
        function: &str,
        input: &I,
    ) -> Result<O> {
        let input_json = serde_json::to_vec(input)?;
        let guard = self.plugin.read().await;

        // spawn_blocking because plugin.call is sync
        let output = tokio::task::spawn_blocking(move || {
            guard.call::<_, Vec<u8>>(function, input_json)
        }).await??;

        Ok(serde_json::from_slice(&output)?)
    }
}
```

## Host Functions

Host functions are the Rust implementations that Haskell calls via Extism imports.

```rust
// host_functions.rs
use extism::{Function, Val, ValType, UserData, CurrentPlugin};
use crate::services::Services;

pub fn host_functions(services: Arc<Services>) -> Vec<Function> {
    vec![
        git_get_branch(services.clone()),
        git_get_worktree(services.clone()),
        github_list_issues(services.clone()),
        github_create_pr(services.clone()),
        docker_exec(services.clone()),
        log_info(services.clone()),
        emit_event(services.clone()),
        tui_popup(services.clone()),
    ]
}

fn git_get_branch(services: Arc<Services>) -> Function {
    Function::new(
        "git_get_branch",
        [ValType::I64],  // input pointer
        [ValType::I64],  // output pointer
        Some(UserData::new(services)),
        |plugin, inputs, outputs, user_data| {
            let services = user_data.get::<Arc<Services>>()?;

            // Read input JSON from WASM memory
            let offset = inputs[0].unwrap_i64() as u64;
            let input_bytes = plugin.memory_read(offset)?;
            let payload: GitGetBranchPayload = serde_json::from_slice(&input_bytes)?;

            // Execute async operation (bridge to Tokio)
            let handle = tokio::runtime::Handle::current();
            let result = handle.block_on(async {
                services.git.get_branch(&payload.working_dir).await
            });

            // Write result to WASM memory
            let result_json = serde_json::to_vec(&result.into_effect_result())?;
            let result_offset = plugin.memory_alloc(result_json.len() as u64)?;
            plugin.memory_write(result_offset, &result_json)?;

            outputs[0] = Val::I64(result_offset as i64);
            Ok(())
        },
    )
}
```

## Services Layer

Each service is a struct with async methods for IO operations.

```rust
// services/git.rs
use crate::services::docker::DockerService;

pub struct GitService {
    docker: Arc<DockerService>,
}

impl GitService {
    pub fn new(docker: Arc<DockerService>) -> Self {
        Self { docker }
    }

    pub async fn get_branch(&self, working_dir: &str) -> Result<String, ServiceError> {
        let output = self.docker.exec(
            &["git", "branch", "--show-current"],
            Some(working_dir),
        ).await?;

        Ok(output.stdout.trim().to_string())
    }

    pub async fn get_worktree(&self, working_dir: &str) -> Result<WorktreeInfo, ServiceError> {
        let rev_parse = self.docker.exec(
            &["git", "rev-parse", "--show-toplevel", "--abbrev-ref", "HEAD", "HEAD"],
            Some(working_dir),
        ).await?;

        let lines: Vec<&str> = rev_parse.stdout.lines().collect();
        Ok(WorktreeInfo {
            path: lines.get(0).unwrap_or(&"").to_string(),
            branch: lines.get(1).unwrap_or(&"").to_string(),
            commit: lines.get(2).unwrap_or(&"").to_string(),
        })
    }
}
```

```rust
// services/github.rs
use octocrab::Octocrab;

pub struct GitHubService {
    client: Octocrab,
}

impl GitHubService {
    pub fn new(token: String) -> Result<Self> {
        let client = Octocrab::builder()
            .personal_token(token)
            .build()?;
        Ok(Self { client })
    }

    pub async fn list_issues(&self, repo: &Repo, filter: Option<&IssueFilter>) -> Result<Vec<Issue>> {
        let mut query = self.client.issues(&repo.owner, &repo.name).list();

        if let Some(f) = filter {
            if let Some(state) = &f.state {
                query = query.state(state.into());
            }
            if let Some(labels) = &f.labels {
                query = query.labels(labels);
            }
        }

        let page = query.send().await?;
        Ok(page.items.into_iter().map(Into::into).collect())
    }

    pub async fn create_pr(&self, repo: &Repo, spec: CreatePRSpec) -> Result<PullRequest> {
        let pr = self.client.pulls(&repo.owner, &repo.name)
            .create(&spec.title, &spec.head, &spec.base)
            .body(&spec.body)
            .send()
            .await?;
        Ok(pr.into())
    }
}
```

## HTTP Server

Entry points for MCP and hooks.

```rust
// server/routes.rs
use axum::{Router, routing::post, Json, Extension};

pub fn routes() -> Router {
    Router::new()
        .route("/role/:role/mcp/tools", get(list_tools))
        .route("/role/:role/mcp/call", post(call_tool))
        .route("/hook/:event", post(handle_hook))
        .route("/health", get(health))
}

async fn call_tool(
    Path(role): Path<String>,
    Query(params): Query<McpParams>,
    Extension(plugin): Extension<Arc<PluginManager>>,
    Json(request): Json<McpToolCallRequest>,
) -> Result<Json<McpResponse>, AppError> {
    let input = McpCallInput {
        role,
        container_id: params.container,
        tool_name: request.tool_name,
        arguments: request.arguments,
    };

    let result: McpResponse = plugin.call("handle_mcp_call", &input).await?;
    Ok(Json(result))
}

async fn handle_hook(
    Path(event): Path<String>,
    Extension(plugin): Extension<Arc<PluginManager>>,
    Json(input): Json<HookInput>,
) -> Result<Json<HookOutput>, AppError> {
    let function = match event.as_str() {
        "pre-tool-use" => "handle_pre_tool_use",
        "post-tool-use" => "handle_post_tool_use",
        "session-start" => "handle_session_start",
        "session-end" => "handle_session_end",
        _ => return Err(AppError::UnknownHook(event)),
    };

    let result = plugin.call(function, &input).await?;
    Ok(Json(result))
}
```

## Startup

```rust
// main.rs
#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt::init();

    // Load config
    let config = Config::from_env()?;

    // Initialize services
    let docker = Arc::new(DockerService::new(config.docker_socket)?);
    let git = Arc::new(GitService::new(docker.clone()));
    let github = Arc::new(GitHubService::new(config.github_token)?);
    let lsp = Arc::new(LspService::new(config.lsp_config)?);

    let services = Arc::new(Services { docker, git, github, lsp });

    // Load WASM plugin
    let plugin = Arc::new(PluginManager::new(
        config.wasm_path,
        services.clone(),
    ).await?);

    // Build router
    let app = routes()
        .layer(Extension(plugin.clone()))
        .layer(Extension(services));

    // Start server
    let addr = SocketAddr::from(([0, 0, 0, 0], config.port));
    tracing::info!("Starting server on {}", addr);
    axum::serve(TcpListener::bind(addr).await?, app).await?;

    Ok(())
}
```

## Hot Reload Endpoint

```rust
// server/routes.rs
async fn reload_plugin(
    Extension(plugin): Extension<Arc<PluginManager>>,
    Extension(services): Extension<Arc<Services>>,
) -> Result<Json<ReloadResponse>, AppError> {
    plugin.reload(services).await?;
    Ok(Json(ReloadResponse { success: true }))
}
```

## Migration from Current Architecture

Current: Haskell control-server does everything.
Target: Rust runtime + Haskell WASM.

Migration steps:
1. Create `exomonad-runtime` crate
2. Implement services (start with Git, simplest)
3. Implement plugin loader + one host function
4. Validate roundtrip with spike
5. Add remaining services incrementally
6. Eventually deprecate Haskell control-server

## Open Questions

1. **Docker API**: Shell out to `docker-ctl` or use bollard crate directly?
2. **LSP**: Keep HLS session management or move to Rust?
3. **TUI**: Keep tui-spawner FIFO approach or rethink?
