# Git Interpreter — Git Effect Handler (Pure)

Interprets the `Git` effect by injecting pure handlers for repository queries and operations. This is primarily used for testing or when delegating to host environments that provide their own git implementation.

## When to Read This

Read this if you're:
- Injecting mock git behavior for integration tests
- Understanding how the `Git` effect is handled in pure contexts
- Extracting worktree names from filesystem paths

## Architecture

The `git-interpreter` provides a pure handler that allows the caller to specify the logic for each git operation. The previous subprocess-based implementation has been replaced by remote execution via the control socket in production environments.

## Usage

```haskell
import ExoMonad.Git.Interpreter (runGit)
import ExoMonad.Effects.Git (Git, getWorktreeInfo)

main :: IO ()
main = runM $ runGit
  (pure Nothing)       -- GetWorktreeInfo
  (pure [])            -- GetDirtyFiles
  (\_ -> pure [])      -- GetRecentCommits
  (pure "main")        -- GetCurrentBranch
  (\_ -> pure 0)       -- GetCommitsAhead
  (\_ _ -> pure ())    -- FetchRemote
  $ do
    info <- getWorktreeInfo
    -- Handle info
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `ExoMonad.Git.Interpreter` | Pure effect interpreter and path utility functions |

## Related Documentation

- [dsl/core/CLAUDE.md](../../dsl/core/CLAUDE.md) - Git effect type definition
- [effects/CLAUDE.md](../CLAUDE.md) - Effect interpreter listing
