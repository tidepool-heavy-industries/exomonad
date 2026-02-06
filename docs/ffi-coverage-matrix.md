# FFI Coverage Matrix

| Host Function | Argument Type (Haskell) | Return Type (Haskell) | Status | Test Location |
|---|---|---|---|---|
| **Git** | | | | |
| `git_get_branch` | `GitHostInput` | `String` | 🟢 | `ffi_property_tests.rs` |
| `git_get_worktree` | `GitHostInput` | `WorktreeInfo` | 🟢 | `ffi_property_tests.rs` |
| `git_get_dirty_files` | `GitHostInput` | `[String]` | 🟢 | `ffi_property_tests.rs` |
| `git_get_recent_commits` | `GitLogInput` | `[Commit]` | 🟢 | `ffi_property_tests.rs` |
| `git_has_unpushed_commits` | `GitHostInput` | `Bool` | 🟢 | `ffi_property_tests.rs` |
| `git_get_remote_url` | `GitHostInput` | `String` | 🟢 | `ffi_property_tests.rs` |
| `git_get_repo_info` | `GitHostInput` | `RepoInfo` | 🟢 | `ffi_property_tests.rs` |
| **GitHub** | | | | |
| `github_list_issues` | `GithubListIssuesInput` | `[Issue]` | 🔴 | |
| `github_get_issue` | `GithubGetIssueInput` | `Issue` | 🔴 | |
| `github_create_pr` | `GithubCreatePRInput` | `PR` | 🔴 | |
| `github_list_prs` | `GithubListPRsInput` | `[PR]` | 🔴 | |
| `github_get_pr_for_branch` | `GithubGetPRForBranchInput` | `Maybe PR` | 🔴 | |
| `github_get_pr_review_comments` | `GithubGetPRReviewCommentsInput` | `[ReviewComment]` | 🔴 | |
| **Log** | | | | |
| `log_info` | `LogPayload` | `()` | 🔴 | |
| `log_error` | `LogPayload` | `()` | 🔴 | |
| `emit_event` | `Value` | `()` | 🔴 | |
| **Agent Control** | | | | |
| `agent_spawn` | `SpawnAgentInput` | `SpawnResult` | 🟢 | `ffi_property_tests.rs` |
| `agent_spawn_batch` | `SpawnAgentsInput` | `BatchSpawnResult` | 🟡 | `proptest_ffi.rs` (types only) |
| `agent_cleanup` | `CleanupAgentInput` | `()` | 🟡 | `proptest_ffi.rs` (types only) |
| `agent_cleanup_batch` | `CleanupAgentsInput` | `BatchCleanupResult` | 🟡 | `proptest_ffi.rs` (types only) |
| `agent_list` | `()` | `[AgentInfo]` | 🟢 | `proptest_ffi.rs`, `agent_control.rs` (unit tests) |
| **Filesystem** | | | | |
| `fs_read_file` | `ReadFileInput` | `ReadFileOutput` | 🟢 | `ffi_property_tests.rs` |
| `fs_write_file` | `WriteFileInput` | `WriteFileOutput` | 🟢 | `ffi_property_tests.rs` |
| **Other** | | | | |
| `file_pr` | `FilePRInput` | `FilePROutput` | 🟢 | `file_pr.rs` (octocrab) |
| `wait_for_copilot_review` | `WaitForCopilotReviewInput` | `ReviewResult` | 🔴 | |

## Legend
- 🔴 Untested
- 🟡 Partially Tested (Types only, or incomplete scenarios)
- 🟢 Fully Tested (End-to-End Property Tests)
