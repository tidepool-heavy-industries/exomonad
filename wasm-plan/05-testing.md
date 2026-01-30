# Testing Strategy

Contract tests, property-based fuzzing, integration tests.

## Testing Pyramid

```
                    ┌─────────────────┐
                    │  E2E (manual)   │  ← Smoke tests in real environment
                    ├─────────────────┤
                    │   Integration   │  ← WASM ↔ Rust roundtrip
                    ├─────────────────┤
                    │    Contract     │  ← JSON serialization parity
                    ├─────────────────┤
                    │      Unit       │  ← Individual components
                    └─────────────────┘
```

## Level 1: Unit Tests

### Haskell (wasm-guest)

Test handlers in native mode with mock interpreter:

```haskell
-- test/Handler/MCPSpec.hs
module Handler.MCPSpec where

import Test.Hspec
import Control.Monad.Freer
import Handler.MCP
import ExoMonad.Guest.Effects

-- Mock interpreter for testing
data MockEnv = MockEnv
  { mockBranch :: Text
  , mockWorktree :: WorktreeInfo
  , mockIssues :: [Issue]
  }

runMock :: MockEnv -> Eff '[HostEffect, IO] a -> IO a
runMock env = runM . interpretM go
  where
    go (GitGetBranch _) = pure $ Right env.mockBranch
    go (GitGetWorktree _) = pure $ Right env.mockWorktree
    go (GitHubListIssues _ _) = pure $ Right env.mockIssues
    go (GitHubCreatePR _ spec) = pure $ Right $ PullRequest
      { url = "https://github.com/test/test/pull/1"
      , number = 1
      , title = spec.title
      }
    go (Log _ _) = pure ()

spec :: Spec
spec = describe "TL.handleMcpTool" $ do
  describe "file_pr" $ do
    it "creates PR from gh- branch" $ do
      let env = MockEnv
            { mockBranch = "gh-123/test-feature"
            , mockWorktree = WorktreeInfo "." "gh-123/test-feature" "abc123"
            , mockIssues = []
            }
      result <- runMock env $ TL.handleMcpTool "file_pr" emptyObject
      case result of
        McpSuccess val -> val ^? key "pr_url" `shouldBe` Just "https://github.com/test/test/pull/1"
        McpError err -> expectationFailure $ "Expected success, got: " <> unpack err

    it "rejects non-gh branches" $ do
      let env = MockEnv { mockBranch = "main", ... }
      result <- runMock env $ TL.handleMcpTool "file_pr" emptyObject
      case result of
        McpError _ -> pure ()
        McpSuccess _ -> expectationFailure "Expected error for non-gh branch"
```

### Rust (exomonad-runtime)

Test services in isolation:

```rust
// tests/services/git_test.rs
use exomonad_runtime::services::GitService;
use mockall::predicate::*;

#[tokio::test]
async fn test_get_branch() {
    let mut mock_docker = MockDockerService::new();
    mock_docker
        .expect_exec()
        .with(eq(&["git", "branch", "--show-current"]), any())
        .returning(|_, _| Ok(ExecOutput {
            stdout: "gh-123/test\n".into(),
            stderr: "".into(),
            exit_code: 0,
        }));

    let git = GitService::new(Arc::new(mock_docker));
    let branch = git.get_branch(".").await.unwrap();
    assert_eq!(branch, "gh-123/test");
}
```

## Level 2: Contract Tests

Verify JSON serialization matches between Rust and Haskell.

### Property-Based Serialization Tests (Rust)

```rust
// tests/contract_tests.rs
use proptest::prelude::*;
use exomonad_runtime::generated::effects::*;

// Generate arbitrary Effect values
fn arb_effect() -> impl Strategy<Value = Effect> {
    prop_oneof![
        any::<String>().prop_map(|dir| Effect::GitGetBranch {
            payload: GitGetBranchPayload { working_dir: dir }
        }),
        (any::<String>(), any::<String>()).prop_map(|(owner, name)| Effect::GitHubListIssues {
            payload: GitHubListIssuesPayload {
                repo: Repo { owner, name },
                filter: None,
            }
        }),
        (any::<String>(), any::<String>()).prop_map(|(level, msg)| Effect::Log {
            payload: LogPayload {
                level: LogLevel::Info,
                message: msg,
                fields: None,
            }
        }),
    ]
}

proptest! {
    #[test]
    fn effect_roundtrip(effect in arb_effect()) {
        // Serialize to JSON
        let json = serde_json::to_string(&effect).unwrap();

        // Verify structure matches our schema contract
        let value: serde_json::Value = serde_json::from_str(&json).unwrap();

        // Must have "kind" and "payload" (adjacently tagged)
        assert!(value.get("kind").is_some(), "Missing 'kind' field");
        assert!(value.get("payload").is_some(), "Missing 'payload' field");

        // Roundtrip
        let parsed: Effect = serde_json::from_str(&json).unwrap();
        assert_eq!(effect, parsed);
    }
}
```

### Golden Tests

Store known-good JSON and verify both sides parse it:

```
tests/golden/
├── effects/
│   ├── git_get_branch.json
│   ├── github_list_issues.json
│   └── log_info.json
└── results/
    ├── success.json
    └── error.json
```

```json
// tests/golden/effects/git_get_branch.json
{
  "kind": "GitGetBranch",
  "payload": {
    "workingDir": "/path/to/repo"
  }
}
```

```rust
// Rust golden test
#[test]
fn parse_git_get_branch_golden() {
    let json = include_str!("golden/effects/git_get_branch.json");
    let effect: Effect = serde_json::from_str(json).unwrap();
    match effect {
        Effect::GitGetBranch { payload } => {
            assert_eq!(payload.working_dir, "/path/to/repo");
        }
        _ => panic!("Wrong variant"),
    }
}
```

```haskell
-- Haskell golden test
spec :: Spec
spec = describe "Golden JSON parsing" $ do
  it "parses GitGetBranch" $ do
    json <- readFile "tests/golden/effects/git_get_branch.json"
    case eitherDecode json of
      Left err -> expectationFailure err
      Right (Effect (GitGetBranch payload)) ->
        payload.workingDir `shouldBe` "/path/to/repo"
```

## Level 3: Integration Tests

Test full WASM ↔ Rust roundtrip.

```rust
// tests/integration_test.rs
use exomonad_runtime::PluginManager;
use std::sync::Arc;

#[tokio::test]
async fn test_mcp_call_roundtrip() {
    // Load real WASM
    let services = Arc::new(MockServices::new());
    let plugin = PluginManager::new(
        PathBuf::from("plugin.wasm"),
        services.clone(),
    ).await.unwrap();

    // Make MCP call
    let input = McpCallInput {
        role: "tl".into(),
        container_id: Some("test-container".into()),
        tool_name: "file_pr".into(),
        arguments: json!({"title": "Test PR"}),
    };

    let result: McpResponse = plugin.call("handle_mcp_call", &input).await.unwrap();

    // Verify response
    match result {
        McpSuccess(val) => {
            assert!(val.get("pr_url").is_some());
        }
        McpError(err) => panic!("Expected success, got error: {}", err),
    }
}

#[tokio::test]
async fn test_host_function_called() {
    let services = Arc::new(MockServices::new());
    services.git.expect_get_branch()
        .times(1)
        .returning(|_| Ok("gh-123/test".into()));

    let plugin = PluginManager::new(
        PathBuf::from("plugin.wasm"),
        services.clone(),
    ).await.unwrap();

    let input = McpCallInput {
        role: "tl".into(),
        tool_name: "file_pr".into(),
        ..Default::default()
    };

    let _ = plugin.call::<_, McpResponse>("handle_mcp_call", &input).await;

    // Verify mock was called
    services.git.checkpoint();
}
```

## Level 4: E2E Smoke Tests

Manual verification in real environment:

```bash
# Start runtime
just dev-runtime

# Test MCP call
curl -X POST http://localhost:7432/role/tl/mcp/call \
  -H "Content-Type: application/json" \
  -d '{"tool_name": "file_pr", "arguments": {}}'

# Test hook
curl -X POST http://localhost:7432/hook/pre-tool-use \
  -H "Content-Type: application/json" \
  -d '{"hook_event_name": "PreToolUse", "tool_name": "Write", ...}'
```

## Error Handling Tests

Verify errors propagate correctly across boundary:

```rust
#[tokio::test]
async fn test_host_error_propagates() {
    let services = Arc::new(MockServices::new());
    services.github.expect_create_pr()
        .returning(|_, _| Err(ServiceError::ApiError("Rate limited".into())));

    let plugin = PluginManager::new(...).await.unwrap();

    let input = McpCallInput {
        tool_name: "file_pr".into(),
        ..Default::default()
    };

    let result: McpResponse = plugin.call("handle_mcp_call", &input).await.unwrap();

    match result {
        McpError(msg) => assert!(msg.contains("Rate limited")),
        _ => panic!("Expected error"),
    }
}
```

## CI Test Matrix

```yaml
test:
  runs-on: ubuntu-latest
  strategy:
    matrix:
      test-type: [unit-haskell, unit-rust, contract, integration]
  steps:
    - name: Run tests
      run: |
        case "${{ matrix.test-type }}" in
          unit-haskell) just test-wasm-guest ;;
          unit-rust) just test-rust-runtime ;;
          contract) just test-contract ;;
          integration) just test-integration ;;
        esac
```

## Test Data Fixtures

```
tests/fixtures/
├── mcp_requests/
│   ├── file_pr_minimal.json
│   ├── file_pr_full.json
│   └── spawn_agent.json
├── hook_inputs/
│   ├── pre_tool_use_write.json
│   └── session_start.json
└── expected_responses/
    ├── file_pr_success.json
    └── file_pr_error.json
```
