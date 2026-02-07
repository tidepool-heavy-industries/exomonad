# exomonad-services

Standalone clients for external services used by ExoMonad.

## Overview

Provides `ExternalService` trait and implementations for:
- **Anthropic**: Claude API (chat completions)
- **GitHub**: Issue/PR management via octocrab
- **Ollama**: Local LLM generation
- **OTLP**: OpenTelemetry traces/metrics export

## ExternalService Trait

```rust
#[async_trait]
pub trait ExternalService {
    type Request;
    type Response;

    async fn call(&self, req: Self::Request) -> Result<Self::Response, ServiceError>;
}
```

**Design:** Uniform interface for all external services. Request/Response types defined in `exomonad-shared::protocol`.

## Services

| Service | Client Library | Key Methods |
|---------|----------------|-------------|
| AnthropicService | reqwest (manual) | call(ServiceRequest::AnthropicChat) |
| GitHubService | octocrab 0.38 | call(ServiceRequest::GitHubGetIssue/ListIssues/CreatePR/ListPRs) |
| OllamaService | ollama-rs 0.2 | call(ServiceRequest::OllamaGenerate) |
| OtelService | reqwest (manual) | call(ServiceRequest::OtelExport) |

### AnthropicService

**Purpose:** Claude API chat completions

**Methods:**
- `call(ServiceRequest::AnthropicChat)` - Send messages to Claude with tools

**Authentication:** Requires API key passed to constructor

**Implementation:** Direct HTTP calls via reqwest (no official Rust SDK)

### GitHubService

**Purpose:** Issue and PR management

**Methods:**
- `call(ServiceRequest::GitHubGetIssue)` - Get single issue with comments
- `call(ServiceRequest::GitHubListIssues)` - List repository issues
- `call(ServiceRequest::GitHubCreatePR)` - Create a pull request
- `call(ServiceRequest::GitHubListPRs)` - List repository pull requests
- `call(ServiceRequest::GitHubGetPR)` - Get single PR details
- `call(ServiceRequest::GitHubListPRFiles)` - List files changed in a PR

**Authentication:** Requires GitHub token passed to constructor

**Implementation:** Uses octocrab 0.38 (official GitHub REST API client)

### OllamaService

**Purpose:** Local LLM inference via Ollama

**Methods:**
- `call(ServiceRequest::OllamaGenerate)` - Generate text with local model

**Configuration:** Connects to Ollama server (default: http://localhost:11434)

**Implementation:** Uses ollama-rs 0.2 library

### OtelService

**Purpose:** OpenTelemetry export (traces and metrics)

**Methods:**
- `call(ServiceRequest::OtelExport)` - Export traces/metrics to OTLP endpoint

**Configuration:** Requires OTLP endpoint URL

**Implementation:** Direct HTTP calls via reqwest

## ServiceError Types

| Variant | Description |
|---------|-------------|
| Http(reqwest::Error) | Low-level HTTP client error |
| Api { code, message } | API-level error (non-2xx response) |
| RateLimited { retry_after_ms } | Rate limit exceeded |
| Timeout(ms) | Operation timeout |

**Error handling pattern:**
```rust
match service.call(req).await {
    Ok(response) => { /* handle success */ },
    Err(ServiceError::RateLimited { retry_after_ms }) => { /* retry logic */ },
    Err(ServiceError::Api { code, message }) => { /* API error */ },
    Err(e) => { /* other errors */ },
}
```

## Usage Example

```rust
use exomonad_services::{GitHubService, ExternalService};
use exomonad_shared::protocol::ServiceRequest;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let github = GitHubService::new("token".to_string());
    let req = ServiceRequest::GitHubGetIssue {
        owner: "owner".into(),
        repo: "repo".into(),
        number: 1,
        include_comments: false,
    };
    let res = github.call(req).await?;
    println!("Issue: {:?}", res);
    Ok(())
}
```

## ServiceServer

Optional HTTP server for remote service calls (not used in current sidecar architecture).

**Purpose:** Allows external clients to call services via HTTP instead of linking the library.

**Usage:**
```bash
# Start server on port 8080
service-server --port 8080

# Client sends POST /service with ServiceRequest JSON
curl -X POST http://localhost:8080/service \
  -H "Content-Type: application/json" \
  -d '{"type":"GitHubGetIssue","owner":"org","repo":"repo","number":123,"include_comments":false}'
```

**Note:** The sidecar uses these services directly via library linking, not via HTTP.

## Building

```bash
cargo build -p exomonad-services
cargo test -p exomonad-services
```

## Testing

```bash
cargo test -p exomonad-services
# Tests verify:
# - ServiceError serialization
# - Service trait implementations
# - Request/response type conversions
```

**Note:** Most tests are integration tests requiring API keys/running services:
- GitHub tests require GITHUB_TOKEN env var
- Ollama tests require Ollama server running
- Anthropic tests require ANTHROPIC_API_KEY env var

## Design Notes

- **Best-in-class clients**: Uses octocrab for GitHub (not raw HTTP), ollama-rs for Ollama
- **ServiceServer**: Optional HTTP server for remote service calls (not used in current sidecar architecture)
- **Shared types**: Uses exomonad-shared protocol types for wire format
- **Async-first**: All service calls are async via tokio runtime
- **Error granularity**: ServiceError provides structured error information (rate limits, API errors, timeouts)

## Related Documentation

- [exomonad-contrib](../exomonad-contrib/) - Uses these services via dependency injection
- [exomonad-shared](../exomonad-shared/CLAUDE.md) - Protocol types (ServiceRequest, ServiceResponse)
- [Root CLAUDE.md](../../CLAUDE.md) - Project overview
