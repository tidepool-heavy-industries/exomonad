# Effector: Stateless IO Executor

Stateless IO executor that runs in agent containers and returns structured JSON. This is the foundation for the Stop Hook workflow engine.

## Design Principles

1. **Structured JSON output, always** - No plain text, always parseable.
2. **Exit code 0 = effector ran successfully** - Even if build failed (failure is data).
3. **Exit code non-zero = effector itself broke** - Couldn't find cabal, parse error, etc.
4. **Stateless** - No state between invocations.
5. **Env vars for config** - CWD override, GitHub token, etc.

## Commands

### Cabal
- `effector cabal build [--cwd <path>]`: Run cabal build and return JSON.
- `effector cabal test [--cwd <path>] [--package <name>]`: Run cabal test and return JSON.

### Git
- `effector git status [--cwd <path>]`: Get git status as JSON.
- `effector git diff [--cwd <path>] [--staged]`: Get git diff as JSON.

### GitHub (gh)
- `effector gh pr-status [--branch <name>]`: Get PR status as JSON.
- `effector gh pr-create --title <title> --body <body> [--base <branch>]`: Create PR and return JSON.

## Integration

Used by the Haskell control-server via SSH or HTTP effects to execute commands in isolated agent environments.
