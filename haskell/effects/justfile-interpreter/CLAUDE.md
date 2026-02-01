# Justfile Interpreter

Implements `Justfile` effect: Runs recipes via `just` CLI.

## Overview

The Justfile interpreter executes just recipes as subprocesses. This enables agents to leverage project-specific build/test/deploy scripts defined in `justfile`.

## Effect Operations

| Operation | Description | Implementation |
|-----------|-------------|----------------|
| `RunRecipe name args` | Execute a just recipe | `just <name> <args...>` subprocess |

## Build and Test

```bash
cabal build exomonad-justfile-interpreter
cabal test exomonad-justfile-interpreter
```

## Dependencies

- `just` CLI installed and in PATH

## Usage Example

```haskell
import ExoMonad.Justfile.Interpreter (runJustfileIO)
import ExoMonad.Effects.Justfile

main = runM $ runJustfileIO $ do
  runRecipe "build" []
  runRecipe "test" ["--verbose"]
```

## Related Documentation

- [Root CLAUDE.md](../../../CLAUDE.md) - Project overview
- [effects/CLAUDE.md](../CLAUDE.md) - Effect interpreter pattern
