# FileSystem Interpreter — Direct File I/O

Interprets the `FileSystem` effect using standard Haskell `System.Directory` and `System.IO` operations.

## When to Read This

Read this if you're:
- Debugging file system operations (creation, writing, copying)
- Understanding how file paths are handled in the interpreter
- Adding new file manipulation capabilities to the agent environment

## Architecture

This interpreter uses the `Polysemy.Embed` effect to perform direct side-effecting I/O operations on the local filesystem. It ensures that parent directories exist before writing or copying files.

## Usage

```haskell
import ExoMonad.FileSystem.Interpreter (runFileSystemIO)
import ExoMonad.Effects.FileSystem

main :: IO ()
main = runM . runFileSystemIO $ do
  createDirectory someDirPath
  writeTextFile someFilePath "Hello ExoMonad"
  exists <- fileExists someFilePath
  pure exists
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `ExoMonad.FileSystem.Interpreter` | I/O implementation of file system operations |

## Related Documentation

- [dsl/core/CLAUDE.md](../../dsl/core/CLAUDE.md) - FileSystem effect type
- [effects/CLAUDE.md](../CLAUDE.md) - Effect interpreter listing
