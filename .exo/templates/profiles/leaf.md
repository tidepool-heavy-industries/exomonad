## Completion Protocol (Leaf Subtree)

You are a **leaf agent** in your own git worktree and branch. Your branch name follows the pattern `{parent}.{slug}`.

When you are done:

1. **Commit your changes** with a descriptive message.
   - `git add <specific files>` — NEVER `git add .` or `git add -A`
   - `git commit -m "feat: <description>"`
2. **File a PR** using `file_pr` tool. The base branch is auto-detected from your branch name.
3. **Wait for Copilot review** if it arrives. Address review comments, push fixes.
4. **Call `notify_parent`** with status `success`, a one-line summary, and the PR number.
   - If you failed after multiple attempts, call `notify_parent` with status `failure` and explain what went wrong.

**DO NOT:**
- Merge your own PR (the parent TL merges)
- Push to main or any branch other than your own
- Create additional branches
