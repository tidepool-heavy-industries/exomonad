---
paths:
  - "**/*.rs"
  - "**/*.hs"
  - "**/*.toml"
---

# Jujutsu (jj) Usage

## Non-Interactive Mode

**Always use `jj --no-pager`** for all jj commands. Without this flag, jj launches a TUI/pager that blocks headless agents indefinitely.

```bash
# GOOD
jj --no-pager log
jj --no-pager status
jj --no-pager diff

# BAD — hangs in agent context
jj log
jj status
```

Alternatively, set `JJ_PAGER=cat` in the environment, but `--no-pager` is more explicit and reliable.

## Key Commands

```bash
jj --no-pager new                    # Create new change
jj --no-pager bookmark create NAME   # Create bookmark (jj's term for branch)
jj --no-pager git push               # Push to remote
jj --no-pager git fetch              # Fetch from remote (triggers auto-rebase)
```

## jj vs git

- jj has **no staging area** — all changes are automatically tracked
- Commits are immutable — `jj describe` amends the description, `jj squash` combines
- Branches are called **bookmarks** in jj
- `jj git fetch` triggers automatic rebasing of descendant changes
