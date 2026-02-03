# FFI Coverage Matrix

| Host Function | Argument Type (Haskell) | Return Type (Haskell) | Status | Test Location |
|---|---|---|---|---|
| **Git** | | | | |
| `git_get_branch` | `GitHostInput` | `String` | 🟢 | `ffi_property_tests.rs` |
| `git_get_worktree` | `GitHostInput` | `WorktreeInfo` | 🔴 | |
| `git_get_dirty_files` | `GitHostInput` | `[String]` | 🔴 | |
| `git_get_recent_commits` | `GitLogInput` | `[Commit]` | 🔴 | |
| `git_has_unpushed_commits` | `GitHostInput` | `Bool` | 🔴 | |
| `git_get_remote_url` | `GitHostInput` | `String` | 🔴 | |
| `git_get_repo_info` | `GitHostInput` | `RepoInfo` | 🔴 | |
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
| `agent_spawn` | `SpawnAgentInput` | `SpawnResult` | 🟡 | `proptest_ffi.rs` (types only) |
| `agent_spawn_batch` | `SpawnAgentsInput` | `BatchSpawnResult` | 🟡 | `proptest_ffi.rs` (types only) |
| `agent_cleanup` | `CleanupAgentInput` | `()` | 🟡 | `proptest_ffi.rs` (types only) |
| `agent_cleanup_batch` | `CleanupAgentsInput` | `BatchCleanupResult` | 🟡 | `proptest_ffi.rs` (types only) |
| `agent_list` | `ListAgentsInput` | `[AgentInfo]` | 🟡 | `proptest_ffi.rs` (types only) |
| **Filesystem** | | | | |
| `fs_read_file` | `ReadFileInput` | `ReadFileOutput` | 🔴 | |
| `fs_write_file` | `WriteFileInput` | `WriteFileOutput` | 🔴 | |
| **Other** | | | | |
| `file_pr` | `FilePRInput` | `PR` | 🔴 | |
| `wait_for_copilot_review` | `WaitForCopilotReviewInput` | `ReviewResult` | 🔴 | |

## Legend
- 🔴 Untested
- 🟡 Partially Tested (Types only, or incomplete scenarios)
- 🟢 Fully Tested (End-to-End Property Tests)