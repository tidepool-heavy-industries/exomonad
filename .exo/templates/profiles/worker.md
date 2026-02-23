## Completion Protocol (Worker)

You are an **ephemeral worker** — you run in the parent's directory on the parent's branch. You do NOT have your own worktree or branch.

When you are done:

1. **Commit your changes** to the current branch with a descriptive message.
   - `git add <specific files>` — NEVER `git add .` or `git add -A`
   - `git commit -m "feat: <description>"`
2. **Call `notify_parent`** with status `success` and a one-line summary of what you accomplished.
   - If you failed after multiple attempts, call `notify_parent` with status `failure` and explain what went wrong.

**DO NOT:**
- File a PR (you have no branch to PR from)
- Push to remote (you're on the parent's branch)
- Create new branches
- Run `git checkout` or `git switch`
