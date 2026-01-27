# Task: Service Integration

Wire Haskell interpreters to Rust external services via Unix socket.

## Goal

```
Haskell Interpreter          Unix Socket         Rust Service Server
     │                            │                      │
     │ ServiceRequest (JSON)      │                      │
     │──────────────────────────▶│──────────────────────▶│
     │                            │                      │ routes to:
     │                            │                      │ - AnthropicService
     │                            │                      │ - GitHubService
     │                            │                      │ - OllamaService
     │                            │                      │ - OtelService
     │ ServiceResponse (JSON)     │                      │
     │◀──────────────────────────│◀──────────────────────│
```

## Scope

1. Create Rust service socket server (listens, routes, responds)
2. Wire control-server to use SocketConfig for interpreters
3. Verify protocol alignment (Haskell ↔ Rust JSON)
4. E2E testing

## Phase 1: Rust Service Socket Server

Create `rust/exomonad-services/src/server.rs`:

```rust
use tokio::net::UnixListener;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use crate::{AnthropicService, GitHubService, OllamaService, OtelService};
use exomonad_shared::protocol::{ServiceRequest, ServiceResponse};

pub struct ServiceServer {
    anthropic: AnthropicService,
    github: GitHubService,
    ollama: OllamaService,
    otel: OtelService,
}

impl ServiceServer {
    pub async fn run(self, socket_path: &str) -> Result<(), Error> {
        let listener = UnixListener::bind(socket_path)?;

        loop {
            let (stream, _) = listener.accept().await?;
            let server = self.clone();
            tokio::spawn(async move {
                server.handle_connection(stream).await
            });
        }
    }

    async fn handle_connection(&self, stream: UnixStream) {
        let (reader, mut writer) = stream.into_split();
        let mut lines = BufReader::new(reader).lines();

        while let Some(line) = lines.next_line().await? {
            let request: ServiceRequest = serde_json::from_str(&line)?;
            let response = self.route(request).await;
            let json = serde_json::to_string(&response)? + "\n";
            writer.write_all(json.as_bytes()).await?;
        }
    }

    async fn route(&self, req: ServiceRequest) -> ServiceResponse {
        match req {
            ServiceRequest::AnthropicChat { .. } => {
                self.anthropic.call(req).await.into()
            }
            ServiceRequest::GitHubGetIssue { .. } |
            ServiceRequest::GitHubListIssues { .. } |
            ServiceRequest::GitHubCreateIssue { .. } |
            ServiceRequest::GitHubCreatePR { .. } |
            ServiceRequest::GitHubGetPR { .. } => {
                self.github.call(req).await.into()
            }
            ServiceRequest::OllamaGenerate { .. } => {
                self.ollama.call(req).await.into()
            }
            ServiceRequest::OtelSpan { .. } |
            ServiceRequest::OtelMetric { .. } => {
                self.otel.call(req).await.into()
            }
        }
    }
}
```

### Binary

Add to `rust/exomonad-services/src/bin/service-server.rs`:

```rust
#[tokio::main]
async fn main() -> Result<()> {
    let socket_path = std::env::var("EXOMONAD_SERVICE_SOCKET")
        .unwrap_or_else(|_| ".exomonad/sockets/services.sock".to_string());

    let server = ServiceServer::new(
        AnthropicService::from_env()?,
        GitHubService::from_env()?,
        OllamaService::from_env()?,
        OtelService::from_env()?,
    );

    server.run(&socket_path).await
}
```

## Phase 2: Control Server Wiring

Update `haskell/control-server/src/ExoMonad/Control/Server.hs`:

```haskell
-- Currently:
loadLLMConfig :: IO (Maybe LLMConfig)
loadLLMConfig = do
  apiKey <- lookupEnv "ANTHROPIC_API_KEY"
  pure $ fmap (\k -> LLMHttpConfig { lcAnthropicSecrets = Just ... }) apiKey

-- Change to:
loadLLMConfig :: IO LLMConfig
loadLLMConfig = do
  socketPath <- lookupEnv "EXOMONAD_SERVICE_SOCKET"
  case socketPath of
    Just path -> pure $ LLMSocketConfig path
    Nothing -> error "EXOMONAD_SERVICE_SOCKET environment variable required for LLM configuration"
```

Similarly for GitHub, Gemini, Observability interpreters.

## Phase 3: Protocol Alignment

Verify JSON serialization matches between:

| Haskell | Rust |
|---------|------|
| `haskell/effects/socket-client/src/.../SocketClient.hs` | `rust/exomonad-shared/src/protocol.rs` |

### Known Differences to Fix

1. **Field naming**: Haskell uses `maxTokens`, Rust uses `max_tokens`
   - Fix: Add `#[serde(rename = "maxTokens")]` to Rust or snake_case to Haskell

2. **GitHub response**: Haskell has `GitHubIssueResponse`, Rust has `GitHubIssue`
   - Fix: Align variant names

3. **Missing variants**: Some Rust variants not in Haskell (CreateIssue, CreatePR, GetPR)
   - Fix: Add to Haskell if needed, or accept subset

### Test Script

```bash
# Start service server
EXOMONAD_SERVICE_SOCKET=.exomonad/sockets/services.sock cargo run -p exomonad-services --bin service-server &

# Test from Haskell
cabal run test-service-integration
```

## Phase 4: Docker Compose Integration

Update `docker-compose.yml` to run service-server alongside control-server:

```yaml
services:
  service-server:
    build:
      context: .
      target: service-server
    environment:
      - ANTHROPIC_API_KEY
      - GITHUB_TOKEN
      - OLLAMA_HOST=http://ollama:11434
      - OTLP_ENDPOINT
    volumes:
      - exomonad-sockets:/app/.exomonad/sockets
```

Or integrate into control-server container (simpler).

## File Checklist

### New Files
- [ ] `rust/exomonad-services/src/server.rs` - Socket server
- [ ] `rust/exomonad-services/src/bin/service-server.rs` - Binary entry

### Modified Files
- [ ] `rust/exomonad-services/Cargo.toml` - Add tokio, bin target
- [ ] `rust/exomonad-shared/src/protocol.rs` - Field renames for alignment
- [ ] `haskell/effects/socket-client/src/.../SocketClient.hs` - Align types
- [ ] `haskell/control-server/src/ExoMonad/Control/Server.hs` - SocketConfig wiring

## Acceptance Criteria

- [ ] `cargo build -p exomonad-services` succeeds (with server)
- [ ] `cabal build all` succeeds
- [ ] Service server starts and listens on Unix socket
- [ ] Haskell interpreter can send ServiceRequest, receive ServiceResponse
- [ ] E2E: control-server → socket-client → service-server → Anthropic API
- [ ] Tests pass

## Out of Scope

- Removing HTTP code paths (cleanup PR later)
- Removing HTTP dependencies (cleanup PR later)
- Production hardening (connection pooling, metrics)

## Dependencies

This PR depends on:
- ✅ PR #367: Haskell socket-client + dual-mode interpreters
- ✅ PR #368: Rust exomonad-services crate
