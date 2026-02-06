use exomonad_runtime::common::{ErrorCode, ErrorContext, HostError, HostResult};
use exomonad_runtime::services::agent_control::*;
use exomonad_runtime::services::copilot_review::*;
use exomonad_runtime::services::file_pr::*;
use exomonad_runtime::services::filesystem::*;
use exomonad_runtime::services::git::*;
use exomonad_runtime::services::github::*;
use exomonad_runtime::services::log::*;
use exomonad_runtime::services::popup::*;
use exomonad_shared::protocol::HookInput;
use exomonad_shared::{GithubOwner, GithubRepo, IssueNumber};
use proptest::prelude::*;
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
    prop_oneof![Just(AgentType::Claude), Just(AgentType::Gemini),].boxed()
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
    ]
    .boxed()
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

// --- Agent Control ---

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

// --- Git ---

prop_compose! {
    fn arb_git_host_input()(
        working_dir in "[a-z0-9/._-]{1,50}",
        container_id in "[a-z0-9-]{1,20}"
    ) -> GitHostInput {
        GitHostInput {
            working_dir,
            container_id,
        }
    }
}

prop_compose! {
    fn arb_git_log_input()(
        working_dir in "[a-z0-9/._-]{1,50}",
        container_id in "[a-z0-9-]{1,20}",
        limit in 1u32..100
    ) -> GitLogInput {
        GitLogInput {
            working_dir,
            container_id,
            limit,
        }
    }
}

// --- GitHub ---

prop_compose! {
    fn arb_github_repo_struct()(
        owner in arb_github_owner(),
        name in arb_github_repo(),
    ) -> Repo {
        Repo { owner, name }
    }
}

prop_compose! {
    fn arb_issue_filter()(
        state in proptest::option::of(prop_oneof!["open", "closed", "all"]),
        labels in proptest::option::of(proptest::collection::vec("[a-z0-9-]{1,20}", 0..5)),
    ) -> IssueFilter {
        IssueFilter { state: state.map(|s| s.to_string()), labels }
    }
}

prop_compose! {
    fn arb_create_pr_spec()(
        title in "[A-Za-z0-9 ]{1,100}",
        body in ".*",
        head in "[a-z0-9/-]{1,50}",
        base in "[a-z0-9/-]{1,50}",
    ) -> CreatePRSpec {
        CreatePRSpec { title, body, head, base }
    }
}

prop_compose! {
    fn arb_pr_filter()(
        state in proptest::option::of(prop_oneof!["open", "closed", "all"]),
        limit in proptest::option::of(1u32..100),
    ) -> PRFilter {
        PRFilter { state: state.map(|s| s.to_string()), limit }
    }
}

prop_compose! {
    fn arb_github_list_issues_input()(
        repo in arb_github_repo_struct(),
        filter in proptest::option::of(arb_issue_filter()),
    ) -> GithubListIssuesInput {
        GithubListIssuesInput { repo, filter }
    }
}

prop_compose! {
    fn arb_github_get_issue_input()(
        repo in arb_github_repo_struct(),
        number in 1u64..1000000,
    ) -> GithubGetIssueInput {
        GithubGetIssueInput { repo, number }
    }
}

prop_compose! {
    fn arb_github_create_pr_input()(
        repo in arb_github_repo_struct(),
        spec in arb_create_pr_spec(),
    ) -> GithubCreatePRInput {
        GithubCreatePRInput { repo, spec }
    }
}

prop_compose! {
    fn arb_github_list_prs_input()(
        repo in arb_github_repo_struct(),
        filter in proptest::option::of(arb_pr_filter()),
    ) -> GithubListPRsInput {
        GithubListPRsInput { repo, filter }
    }
}

prop_compose! {
    fn arb_github_get_pr_for_branch_input()(
        repo in arb_github_repo_struct(),
        head in "[a-z0-9/-]{1,50}",
    ) -> GithubGetPRForBranchInput {
        GithubGetPRForBranchInput { repo, head }
    }
}

prop_compose! {
    fn arb_github_get_pr_review_comments_input()(
        repo in arb_github_repo_struct(),
        pr_number in 1u64..1000000,
    ) -> GithubGetPRReviewCommentsInput {
        GithubGetPRReviewCommentsInput { repo, pr_number }
    }
}

// --- Filesystem ---

prop_compose! {
    fn arb_read_file_input()(
        path in "[a-z0-9/._-]{1,50}",
        max_bytes in 0..1024*1024usize,
    ) -> ReadFileInput {
        ReadFileInput { path, max_bytes }
    }
}

prop_compose! {
    fn arb_write_file_input()(
        path in "[a-z0-9/._-]{1,50}",
        content in ".*",
        create_parents in any::<bool>(),
    ) -> WriteFileInput {
        WriteFileInput { path, content, create_parents }
    }
}

// --- File PR ---

prop_compose! {
    fn arb_file_pr_input()(
        title in "[A-Za-z0-9 ]{1,100}",
        body in ".*",
    ) -> FilePRInput {
        FilePRInput { title, body }
    }
}

// --- Copilot Review ---

prop_compose! {
    fn arb_wait_for_copilot_review_input()(
        pr_number in 1u64..1000000,
        timeout_secs in 1u64..3600,
        poll_interval_secs in 1u64..600,
    ) -> WaitForCopilotReviewInput {
        WaitForCopilotReviewInput { pr_number, timeout_secs, poll_interval_secs }
    }
}

// --- Log ---

prop_compose! {
    fn arb_log_payload()(
        message in ".*",
        fields in proptest::option::of(proptest::collection::hash_map(".*", ".*", 0..5)),
    ) -> LogPayload {
        LogPayload { message, fields }
    }
}

// --- Popup ---

prop_compose! {
    fn arb_popup_input()(
        title in "[A-Za-z0-9 ]{1,50}",
        // Simplified components for roundtrip testing
        components in proptest::collection::vec(
            prop_oneof![
                Just(serde_json::json!({"type": "text", "id": "t1", "content": "label"})),
                Just(serde_json::json!({"type": "input", "id": "i1", "label": "name", "placeholder": "..."})),
                Just(serde_json::json!({"type": "checkbox", "id": "c1", "label": "check", "default": true})),
            ],
            1..5
        )
    ) -> PopupInput {
        PopupInput { title, components }
    }
}

// --- Hook ---

prop_compose! {
    fn arb_hook_input()(
        session_id in "[a-z0-9]{8,12}",
        transcript_path in "[a-z0-9/._-]{1,50}",
        cwd in "[a-z0-9/._-]{1,50}",
        permission_mode in prop_oneof!["default", "plan", "acceptEdits", "dontAsk", "bypassPermissions"],
        hook_event_name in prop_oneof!["PreToolUse", "PostToolUse", "SessionStart", "SessionEnd", "Stop", "AfterAgent"],
        runtime in proptest::option::of(prop_oneof![Just(exomonad_shared::protocol::Runtime::Claude), Just(exomonad_shared::protocol::Runtime::Gemini)]),
        tool_name in proptest::option::of("[A-Z][a-zA-Z0-9]+"),
        tool_use_id in proptest::option::of("toolu_[a-z0-9]+"),
    ) -> HookInput {
        HookInput {
            session_id: session_id.as_str().into(),
            transcript_path,
            cwd,
            permission_mode: permission_mode.to_string(),
            hook_event_name: hook_event_name.to_string(),
            runtime,
            tool_name: tool_name.map(|n| n.as_str().into()),
            tool_input: None,
            tool_use_id,
            tool_response: None,
            prompt: None,
            message: None,
            notification_type: None,
            stop_hook_active: None,
            trigger: None,
            custom_instructions: None,
            source: None,
            reason: None,
            prompt_response: None,
            timestamp: None,
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

prop_compose! {
    fn arb_read_file_output()(
        content in ".*",
        bytes_read in 0..1024*1024usize,
        truncated in any::<bool>(),
    ) -> ReadFileOutput {
        ReadFileOutput { content, bytes_read, truncated }
    }
}

prop_compose! {
    fn arb_write_file_output()(
        bytes_written in 0..1024*1024usize,
        path in "/tmp/[a-z0-9/._-]{1,50}",
    ) -> WriteFileOutput {
        WriteFileOutput { bytes_written, path }
    }
}

prop_compose! {
    fn arb_worktree_info()(
        path in "/tmp/[a-z0-9/._-]{1,50}",
        branch in "gh-[0-9]+-[a-z0-9-]+",
    ) -> WorktreeInfo {
        WorktreeInfo { path, branch }
    }
}

prop_compose! {
    fn arb_commit()(
        hash in "[a-f0-9]{40}",
        message in ".*",
        author in "[A-Za-z ]+ <[a-z@.]+>",
        date in "202[0-9]-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z",
    ) -> Commit {
        Commit { hash, message, author, date }
    }
}

prop_compose! {
    fn arb_repo_info()(
        branch in "gh-[0-9]+-[a-z0-9-]+",
        owner in proptest::option::of(arb_github_owner().prop_map(|o| o.to_string())),
        name in proptest::option::of(arb_github_repo().prop_map(|r| r.to_string())),
    ) -> RepoInfo {
        RepoInfo { branch, owner, name }
    }
}

prop_compose! {
    fn arb_github_issue()(
        number in 1u64..1000000,
        title in ".*",
        body in ".*",
        state in prop_oneof!["open", "closed"],
        url in "https://github.com/.*",
        author in "[a-z0-9-]+",
        labels in proptest::collection::vec("[a-z0-9-]+", 0..5),
    ) -> Issue {
        Issue { number, title, body, state: state.to_string(), url, author, labels }
    }
}

prop_compose! {
    fn arb_github_pr()(
        number in 1u64..1000000,
        title in ".*",
        body in ".*",
        state in prop_oneof!["open", "closed", "merged"],
        url in "https://github.com/.*",
        author in "[a-z0-9-]+",
        head_ref in "[a-z0-9-]+",
        base_ref in "[a-z0-9-]+",
        created_at in ".*",
        merged_at in proptest::option::of(".*"),
    ) -> PullRequest {
        PullRequest { number, title, body, state: state.to_string(), url, author, head_ref, base_ref, created_at, merged_at }
    }
}

prop_compose! {
    fn arb_github_review_comment()(
        id in 1u64..1000000,
        body in ".*",
        path in ".*",
        line in proptest::option::of(1u32..10000),
        author in "[a-z0-9-]+",
        created_at in ".*",
    ) -> ReviewComment {
        ReviewComment { id, body, path, line, author, created_at }
    }
}

prop_compose! {
    fn arb_file_pr_output()(
        pr_url in "https://github.com/.*",
        pr_number in 1u64..1000000,
        head_branch in ".*",
        base_branch in ".*",
        created in any::<bool>(),
    ) -> FilePROutput {
        FilePROutput { pr_url, pr_number, head_branch, base_branch, created }
    }
}

prop_compose! {
    fn arb_copilot_comment()(
        path in ".*",
        line in proptest::option::of(1u64..10000),
        body in ".*",
        diff_hunk in proptest::option::of(".*"),
    ) -> CopilotComment {
        CopilotComment { path, line, body, diff_hunk }
    }
}

prop_compose! {
    fn arb_copilot_review_output()(
        status in prop_oneof!["reviewed", "timeout"],
        comments in proptest::collection::vec(arb_copilot_comment(), 0..5),
    ) -> CopilotReviewOutput {
        CopilotReviewOutput { status: status.to_string(), comments }
    }
}

prop_compose! {
    fn arb_popup_output()(
        button in prop_oneof!["submit", "cancel"],
        values in Just(serde_json::json!({"field1": "val1"})),
    ) -> PopupOutput {
        PopupOutput { button: button.to_string(), values }
    }
}

fn arb_host_result<T: std::fmt::Debug + Clone + Serialize + for<'de> Deserialize<'de> + 'static>(
    strategy: impl Strategy<Value = T> + 'static,
) -> BoxedStrategy<HostResult<T>> {
    prop_oneof![
        strategy.prop_map(HostResult::Success),
        arb_host_error().prop_map(HostResult::Error),
    ]
    .boxed()
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
    // --- Agent Control ---
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

    // --- Git ---
    #[test]
    fn test_git_host_input_roundtrip(val in arb_git_host_input()) {
        roundtrip(val);
    }

    #[test]
    fn test_git_log_input_roundtrip(val in arb_git_log_input()) {
        roundtrip(val);
    }

    // --- GitHub ---
    #[test]
    fn test_github_list_issues_input_roundtrip(val in arb_github_list_issues_input()) {
        roundtrip(val);
    }

    #[test]
    fn test_github_get_issue_input_roundtrip(val in arb_github_get_issue_input()) {
        roundtrip(val);
    }

    #[test]
    fn test_github_create_pr_input_roundtrip(val in arb_github_create_pr_input()) {
        roundtrip(val);
    }

    #[test]
    fn test_github_list_prs_input_roundtrip(val in arb_github_list_prs_input()) {
        roundtrip(val);
    }

    #[test]
    fn test_github_get_pr_for_branch_input_roundtrip(val in arb_github_get_pr_for_branch_input()) {
        roundtrip(val);
    }

    #[test]
    fn test_github_get_pr_review_comments_input_roundtrip(val in arb_github_get_pr_review_comments_input()) {
        roundtrip(val);
    }

    // --- Filesystem ---
    #[test]
    fn test_read_file_input_roundtrip(val in arb_read_file_input()) {
        roundtrip(val);
    }

    #[test]
    fn test_write_file_input_roundtrip(val in arb_write_file_input()) {
        roundtrip(val);
    }

    // --- File PR ---
    #[test]
    fn test_file_pr_input_roundtrip(val in arb_file_pr_input()) {
        roundtrip(val);
    }

    // --- Copilot Review ---
    #[test]
    fn test_wait_for_copilot_review_input_roundtrip(val in arb_wait_for_copilot_review_input()) {
        roundtrip(val);
    }

    // --- Log ---
    #[test]
    fn test_log_payload_roundtrip(val in arb_log_payload()) {
        roundtrip(val);
    }

    // --- Popup ---
    #[test]
    fn test_popup_input_roundtrip(val in arb_popup_input()) {
        roundtrip(val);
    }

    // --- Hook ---
    #[test]
    fn test_hook_input_roundtrip(val in arb_hook_input()) {
        roundtrip(val);
    }

    // --- Output Results ---
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

    #[test]
    fn test_host_result_read_file_roundtrip(val in arb_host_result(arb_read_file_output())) {
        roundtrip(val);
    }

    #[test]
    fn test_host_result_write_file_roundtrip(val in arb_host_result(arb_write_file_output())) {
        roundtrip(val);
    }

    #[test]
    fn test_host_result_worktree_info_roundtrip(val in arb_host_result(arb_worktree_info())) {
        roundtrip(val);
    }

    #[test]
    fn test_host_result_commit_list_roundtrip(val in arb_host_result(proptest::collection::vec(arb_commit(), 0..10))) {
        roundtrip(val);
    }

    #[test]
    fn test_host_result_repo_info_roundtrip(val in arb_host_result(arb_repo_info())) {
        roundtrip(val);
    }

    #[test]
    fn test_host_result_github_issue_roundtrip(val in arb_host_result(arb_github_issue())) {
        roundtrip(val);
    }

    #[test]
    fn test_host_result_github_issue_list_roundtrip(val in arb_host_result(proptest::collection::vec(arb_github_issue(), 0..10))) {
        roundtrip(val);
    }

    #[test]
    fn test_host_result_github_pr_roundtrip(val in arb_host_result(arb_github_pr())) {
        roundtrip(val);
    }

    #[test]
    fn test_host_result_github_pr_list_roundtrip(val in arb_host_result(proptest::collection::vec(arb_github_pr(), 0..10))) {
        roundtrip(val);
    }

    #[test]
    fn test_host_result_github_review_comment_list_roundtrip(val in arb_host_result(proptest::collection::vec(arb_github_review_comment(), 0..10))) {
        roundtrip(val);
    }

    #[test]
    fn test_host_result_file_pr_roundtrip(val in arb_host_result(arb_file_pr_output())) {
        roundtrip(val);
    }

    #[test]
    fn test_host_result_copilot_review_roundtrip(val in arb_host_result(arb_copilot_review_output())) {
        roundtrip(val);
    }

    #[test]
    fn test_host_result_popup_roundtrip(val in arb_host_result(arb_popup_output())) {
        roundtrip(val);
    }
}

// ============================================================================
// Edge Case & Error Propagation Tests
// ============================================================================

mod edge_case_tests {
    use super::*;

    /// Test with very long strings to ensure we don't hit memory limits or buffer overflows
    /// during serialization/deserialization.
    #[test]
    fn test_extreme_string_roundtrip() {
        let long_string = "a".repeat(1024 * 1024); // 1MB string
        let input = WriteFileInput {
            path: "long.txt".to_string(),
            content: long_string.clone(),
            create_parents: true,
        };
        let serialized = serde_json::to_string(&input).unwrap();
        let deserialized: WriteFileInput = serde_json::from_str(&serialized).unwrap();
        assert_eq!(input.content, deserialized.content);
    }

    /// Test with various unicode characters including emoji and non-latin scripts.
    #[test]
    fn test_unicode_roundtrip() {
        let unicode_content = "Hello 🌍! こんいちは! Привет! 🦀";
        let input = WriteFileInput {
            path: "unicode.txt".to_string(),
            content: unicode_content.to_string(),
            create_parents: true,
        };
        let serialized = serde_json::to_string(&input).unwrap();
        let deserialized: WriteFileInput = serde_json::from_str(&serialized).unwrap();
        assert_eq!(input.content, deserialized.content);
    }

    /// Test that missing optional fields are handled correctly (set to None).
    #[test]
    fn test_missing_optional_fields() {
        let json = r#"{"path": "test.txt"}"#; // max_bytes is missing
        let input: ReadFileInput = serde_json::from_str(json).unwrap();
        assert_eq!(input.max_bytes, 0); // Default for usize
    }

    /// Test that extra fields in JSON are ignored (forward compatibility).
    #[test]
    fn test_extra_fields_ignored() {
        let json = r#"{"path": "test.txt", "extra": "field"}"#;
        let result: Result<ReadFileInput, _> = serde_json::from_str(json);
        assert!(result.is_ok());
    }

    /// Test error propagation for malformed JSON.
    #[test]
    fn test_malformed_json_error() {
        let json = r#"{"path": "test.txt", "truncated": "not-a-bool"}"#;
        let result: Result<ReadFileOutput, _> = serde_json::from_str(json);
        assert!(result.is_err());
    }

    /// Test error propagation for missing required fields.
    #[test]
    fn test_missing_required_field_error() {
        let json = r#"{"owner": "anthropics"}"#; // repo is missing
        let result: Result<Repo, _> = serde_json::from_str(json);
        assert!(result.is_err());
    }
}

// ============================================================================
// Testing Strategy Documentation
// ============================================================================

/// # FFI Boundary Property Testing Strategy
///
/// The tests in this file verify the integrity of the WASM/Rust FFI boundary.
/// Because ExoMonad relies on passing complex JSON structures between Haskell (WASM)
/// and Rust (Host), any mismatch in serialization logic can lead to runtime failures.
///
/// ## Core Components:
///
/// 1. **Roundtrip Tests**: We use `proptest` to generate arbitrary instances of
///    all FFI input and output structs. We then verify that `serde_json` can
///    serialize and deserialize them without data loss.
///
/// 2. **Cross-Boundary Contract Tests**: These tests use hardcoded JSON strings
///    that match exactly what the Haskell code produces (e.g., in `Runtime.hs`).
///    This ensures that Rust's types are truly compatible with Haskell's producers.
///
/// 3. **Edge Case Tests**: We explicitly test extreme conditions:
///    - Very large payloads (1MB+ strings)
///    - Complex Unicode/Emoji content
///    - Empty collections
///    - Null vs missing optional fields
///
/// 4. **Error Propagation**: We verify that malformed JSON or type mismatches
///    are caught at the boundary and don't cause panics.
///
/// ## How to run:
/// ```bash
/// cargo test --test proptest_ffi
/// ```
///
/// ## How to interpret failures:
/// If a roundtrip test fails, it usually means a struct is missing a required
/// `#[serde(default)]` or has an incompatible mapping between Haskell and Rust.
/// If a cross-boundary test fails, it means the Haskell and Rust protocol
/// definitions have diverged and must be synchronized.
// These tests verify that Rust can parse JSON that Haskell produces.
// The JSON strings here match exactly what the Haskell code generates.

mod cross_boundary_tests {
    use exomonad_shared::protocol::{InternalStopHookOutput, StopDecision};
    use serde_json::Value;

    /// Test that emit_event can parse what Haskell Runtime.hs sends.
    /// Haskell sends: {"type": "agent:stopped", "agent_id": "...", "timestamp": "..."}
    /// Rust now accepts flat objects and extracts "type" as event_type.
    #[test]
    fn test_emit_event_parses_haskell_format() {
        // This is exactly what Haskell Runtime.hs line 110-114 produces:
        let haskell_json = r#"{"type": "agent:stopped", "agent_id": "gh-123-gemini", "timestamp": "2026-02-03T00:00:00Z"}"#;

        // Should parse as a generic Value (which is what emit_event now expects)
        let result: Result<Value, _> = serde_json::from_str(haskell_json);
        assert!(
            result.is_ok(),
            "Failed to parse Haskell emit_event payload: {:?}",
            result.err()
        );

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
        let haskell_block_json =
            r#"{"decision": "block", "reason": "You have uncommitted changes."}"#;

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
