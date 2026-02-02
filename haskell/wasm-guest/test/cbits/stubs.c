#include <stdint.h>
#include <stdlib.h>

uint64_t agent_spawn(uint64_t offset) { return 0; }
uint64_t agent_spawn_batch(uint64_t offset) { return 0; }
uint64_t agent_cleanup(uint64_t offset) { return 0; }
uint64_t agent_cleanup_batch(uint64_t offset) { return 0; }
uint64_t agent_list(uint64_t offset) { return 0; }

// Git
uint64_t git_get_branch(uint64_t offset) { return 0; }
uint64_t git_get_worktree(uint64_t offset) { return 0; }
uint64_t git_get_dirty_files(uint64_t offset) { return 0; }
uint64_t git_get_recent_commits(uint64_t offset) { return 0; }
uint64_t git_has_unpushed_commits(uint64_t offset) { return 0; }
uint64_t git_get_remote_url(uint64_t offset) { return 0; }
uint64_t git_get_repo_info(uint64_t offset) { return 0; }

// GitHub
uint64_t github_list_issues(uint64_t offset) { return 0; }
uint64_t github_get_issue(uint64_t offset) { return 0; }
uint64_t github_create_pr(uint64_t offset) { return 0; }
uint64_t github_list_prs(uint64_t offset) { return 0; }
uint64_t github_get_pr_for_branch(uint64_t offset) { return 0; }
uint64_t github_get_pr_review_comments(uint64_t offset) { return 0; }

// Log
void log_info(uint64_t offset) {}
void log_error(uint64_t offset) {}
void emit_event(uint64_t offset) {}

// Filesystem
uint64_t fs_read_file(uint64_t offset) { return 0; }
uint64_t fs_write_file(uint64_t offset) { return 0; }

// File PR
uint64_t file_pr(uint64_t offset) { return 0; }

// Copilot
uint64_t wait_for_copilot_review(uint64_t offset) { return 0; }

