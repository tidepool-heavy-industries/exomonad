use proptest::prelude::*;
use exomonad_runtime::common::{ErrorCode, ErrorContext, HostError, HostResult};
use exomonad_runtime::services::agent_control::*;
use exomonad_shared::{GithubOwner, GithubRepo, IssueNumber};
use serde::{Deserialize, Serialize};

// ============================================================================
// Strategies for Shared Types
// ============================================================================

prop_compose! {
    fn arb_issue_number()(n in 1u64..1000000) -> IssueNumber {
        IssueNumber::try_from(n).unwrap()
    }
}

prop_compose! {
    fn arb_github_owner()(s in "[a-zA-Z0-9_-]{1,39}") -> GithubOwner {
        GithubOwner::try_from(s).unwrap()
    }
}

prop_compose! {
    fn arb_github_repo()(s in "[a-zA-Z0-9_-]{1,100}") -> GithubRepo {
        GithubRepo::try_from(s).unwrap()
    }
}

fn arb_agent_type() -> BoxedStrategy<AgentType> {
    prop_oneof![
        Just(AgentType::Claude),
        Just(AgentType::Gemini),
    ].boxed()
}

fn arb_error_code() -> BoxedStrategy<ErrorCode> {
    prop_oneof![
        Just(ErrorCode::NotFound),
        Just(ErrorCode::NotAuthenticated),
        Just(ErrorCode::GitError),
        Just(ErrorCode::IoError),
        Just(ErrorCode::NetworkError),
        Just(ErrorCode::InvalidInput),
        Just(ErrorCode::InternalError),
        Just(ErrorCode::Timeout),
        Just(ErrorCode::AlreadyExists),
    ].boxed()
}

prop_compose! {
    fn arb_error_context()(
        command in proptest::option::of(".*"),
        exit_code in proptest::option::of(any::<i32>()),
        stderr in proptest::option::of(".*"),
        stdout in proptest::option::of(".*"),
        file_path in proptest::option::of(".*"),
        working_dir in proptest::option::of(".*"),
    ) -> ErrorContext {
        ErrorContext {
            command,
            exit_code,
            stderr,
            stdout,
            file_path,
            working_dir,
        }
    }
}

// ============================================================================
// Strategies for Input Types (Haskell -> Rust)
// ============================================================================

prop_compose! {
    fn arb_spawn_agent_input()(
        issue_id in (1u64..1000000).prop_map(|n| n.to_string()),
        owner in arb_github_owner(),
        repo in arb_github_repo(),
        worktree_dir in proptest::option::of("[a-z0-9/_-]{1,50}"),
        agent_type in arb_agent_type(),
    ) -> SpawnAgentInput {
        SpawnAgentInput {
            issue_id,
            owner,
            repo,
            worktree_dir,
            agent_type,
        }
    }
}

prop_compose! {
    fn arb_spawn_agents_input()(
        issue_ids in proptest::collection::vec("[0-9]{1,10}", 1..10),
        owner in arb_github_owner(),
        repo in arb_github_repo(),
        worktree_dir in proptest::option::of("[a-z0-9/_-]{1,50}"),
        agent_type in arb_agent_type(),
    ) -> SpawnAgentsInput {
        SpawnAgentsInput {
            issue_ids,
            owner,
            repo,
            worktree_dir,
            agent_type,
        }
    }
}

prop_compose! {
    fn arb_cleanup_agent_input()(
        issue_id in "[0-9]{1,10}",
        force in any::<bool>(),
    ) -> CleanupAgentInput {
        CleanupAgentInput {
            issue_id,
            force,
        }
    }
}

prop_compose! {
    fn arb_cleanup_agents_input()(
        issue_ids in proptest::collection::vec("[0-9]{1,10}", 1..10),
        force in any::<bool>(),
    ) -> CleanupAgentsInput {
        CleanupAgentsInput {
            issue_ids,
            force,
        }
    }
}

// ============================================================================
// Strategies for Output Types (Rust -> Haskell)
// ============================================================================

prop_compose! {
    fn arb_host_error()(
        message in ".*",
        code in arb_error_code(),
        context in proptest::option::of(arb_error_context()),
        suggestion in proptest::option::of(".*"),
    ) -> HostError {
        HostError {
            message,
            code,
            context,
            suggestion,
        }
    }
}

prop_compose! {
    fn arb_spawn_result()(
        worktree_path in "/tmp/worktrees/[a-z0-9-]+",
        branch_name in "gh-[0-9]+-[a-z0-9-]+",
        tab_name in "gh-[0-9]+-[a-z]+",
        issue_title in ".*",
        agent_type in "claude|gemini",
    ) -> SpawnResult {
        SpawnResult {
            worktree_path,
            branch_name,
            tab_name,
            issue_title,
            agent_type,
        }
    }
}

prop_compose! {
    fn arb_agent_pr_info()(
        number in 1u64..10000,
        title in "[A-Za-z ]+",
        url in "https://github.com/[a-z]+/[a-z]+/pull/[0-9]+",
        state in prop_oneof!["open", "closed", "merged"],
    ) -> AgentPrInfo {
        AgentPrInfo {
            number,
            title,
            url,
            state,
        }
    }
}

prop_compose! {
    fn arb_agent_info()(
        issue_id in "[0-9]+",
        worktree_path in "/tmp/worktrees/[a-z0-9-]+",
        branch_name in "gh-[0-9]+-[a-z0-9-]+",
        has_changes in any::<bool>(),
        slug in proptest::option::of("[a-z0-9-]+"),
        agent_type in proptest::option::of(prop_oneof!["claude", "gemini"]),
        pr in proptest::option::of(arb_agent_pr_info()),
    ) -> AgentInfo {
        AgentInfo {
            issue_id,
            worktree_path,
            branch_name,
            has_changes,
            slug,
            agent_type,
            pr,
        }
    }
}

prop_compose! {
    fn arb_batch_spawn_result()(
        spawned in proptest::collection::vec(arb_spawn_result(), 0..5),
        failed in proptest::collection::vec(("[0-9]+", ".*"), 0..5),
    ) -> BatchSpawnResult {
        BatchSpawnResult {
            spawned,
            failed,
        }
    }
}

prop_compose! {
    fn arb_batch_cleanup_result()(
        cleaned in proptest::collection::vec("[0-9]+", 0..5),
        failed in proptest::collection::vec(("[0-9]+", ".*"), 0..5),
    ) -> BatchCleanupResult {
        BatchCleanupResult {
            cleaned,
            failed,
        }
    }
}

fn arb_host_result<T: std::fmt::Debug + Clone + Serialize + for<'de> Deserialize<'de> + 'static>(
    strategy: impl Strategy<Value = T> + 'static,
) -> BoxedStrategy<HostResult<T>> {
    prop_oneof![
        strategy.prop_map(HostResult::Success),
        arb_host_error().prop_map(HostResult::Error),
    ].boxed()
}

// ============================================================================
// Tests
// ============================================================================

fn roundtrip<T: std::fmt::Debug + PartialEq + Serialize + for<'de> Deserialize<'de>>(val: T) {
    let serialized = serde_json::to_string(&val).unwrap();
    let deserialized: T = serde_json::from_str(&serialized).unwrap();
    assert_eq!(val, deserialized);
}

proptest! {
    #[test]
    fn test_spawn_agent_input_roundtrip(val in arb_spawn_agent_input()) {
        roundtrip(val);
    }

    #[test]
    fn test_spawn_agents_input_roundtrip(val in arb_spawn_agents_input()) {
        roundtrip(val);
    }

    #[test]
    fn test_cleanup_agent_input_roundtrip(val in arb_cleanup_agent_input()) {
        roundtrip(val);
    }

    #[test]
    fn test_cleanup_agents_input_roundtrip(val in arb_cleanup_agents_input()) {
        roundtrip(val);
    }

    #[test]
    fn test_host_result_spawn_result_roundtrip(val in arb_host_result(arb_spawn_result())) {
        roundtrip(val);
    }

    #[test]
    fn test_host_result_batch_spawn_result_roundtrip(val in arb_host_result(arb_batch_spawn_result())) {
        roundtrip(val);
    }

    #[test]
    fn test_host_result_batch_cleanup_result_roundtrip(val in arb_host_result(arb_batch_cleanup_result())) {
        roundtrip(val);
    }

    #[test]
    fn test_host_result_agent_list_roundtrip(val in arb_host_result(proptest::collection::vec(arb_agent_info(), 0..10))) {
        roundtrip(val);
    }
}

// ============================================================================
// Cross-Boundary Contract Tests
// ============================================================================
// These tests verify that Rust can parse JSON that Haskell produces.
// The JSON strings here match exactly what the Haskell code generates.

mod cross_boundary_tests {
    use serde_json::Value;
    use exomonad_shared::protocol::{InternalStopHookOutput, StopDecision};

    /// Test that emit_event can parse what Haskell Runtime.hs sends.
    /// Haskell sends: {"type": "agent:stopped", "agent_id": "...", "timestamp": "..."}
    /// Rust now accepts flat objects and extracts "type" as event_type.
    #[test]
    fn test_emit_event_parses_haskell_format() {
        // This is exactly what Haskell Runtime.hs line 110-114 produces:
        let haskell_json = r#"{"type": "agent:stopped", "agent_id": "gh-123-gemini", "timestamp": "2026-02-03T00:00:00Z"}"#;

        // Should parse as a generic Value (which is what emit_event now expects)
        let result: Result<Value, _> = serde_json::from_str(haskell_json);
        assert!(result.is_ok(), "Failed to parse Haskell emit_event payload: {:?}", result.err());

        // Verify we can extract the event type
        let payload = result.unwrap();
        let event_type = payload.get("type").and_then(|v| v.as_str());
        assert_eq!(event_type, Some("agent:stopped"));
    }

    /// Test that InternalStopHookOutput can parse what Haskell StopHookOutput sends.
    /// Haskell now sends: {"decision": "block", "reason": "..."}
    /// This is the internal domain format; Rust translates to Claude/Gemini at the edge.
    ///
    /// This test verifies the WASM/Rust boundary for stop hooks.
    #[test]
    fn test_hook_output_parses_stop_hook_format() {
        // Haskell StopHookOutput now produces internal domain format:
        let haskell_block_json = r#"{"decision": "block", "reason": "You have uncommitted changes."}"#;

        // Parse as InternalStopHookOutput (the domain type from WASM)
        let result: InternalStopHookOutput = serde_json::from_str(haskell_block_json).unwrap();

        // The block decision MUST be preserved
        assert_eq!(
            result.decision,
            StopDecision::Block,
            "Block decision was lost!"
        );
        assert_eq!(
            result.reason.as_deref(),
            Some("You have uncommitted changes."),
            "Reason was not preserved"
        );
    }

    /// Test that InternalStopHookOutput correctly parses allow decision.
    #[test]
    fn test_hook_output_parses_allow_format() {
        // Haskell StopHookOutput now produces internal domain format:
        let haskell_allow_json = r#"{"decision": "allow"}"#;

        let result: InternalStopHookOutput = serde_json::from_str(haskell_allow_json).unwrap();

        // Allow should mean decision = Allow
        assert_eq!(
            result.decision,
            StopDecision::Allow,
            "Allow decision was lost!"
        );
    }
}
