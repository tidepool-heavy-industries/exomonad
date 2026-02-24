# Env Interpreter — Environment Variable Access

Interprets the `Env` effect for reading environment variables using standard Haskell `System.Environment` operations.

## When to Read This

Read this if you're:
- Debugging environment variable lookup logic
- Adding support for system-wide environment queries to agents

## Architecture

The `env-interpreter` is a direct I/O implementation that lifts `System.Environment` functions (like `lookupEnv` and `getEnvironment`) into the Polysemy effect stack.

## Usage

```haskell
import ExoMonad.Env.Interpreter (runEnvIO)
import ExoMonad.Effects.Env

main :: IO ()
main = runM . runEnvIO $ do
  mToken <- getEnv "GITHUB_TOKEN"
  envPairs <- getEnvironment
  pure (mToken, envPairs)
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `ExoMonad.Env.Interpreter` | Implementation using `System.Environment` |

## Related Documentation

- [dsl/core/CLAUDE.md](../../dsl/core/CLAUDE.md) - Env effect type
- [effects/CLAUDE.md](../CLAUDE.md) - Effect interpreter listing
