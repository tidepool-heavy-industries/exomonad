# worktree-interpreter - Git Worktree Effect Interpreter

Interprets `Worktree` effects by shelling out to `git worktree` commands.

## Effect Types

Effect type defined in `exomonad-core/src/ExoMonad/Effects/Worktree.hs`:

```haskell
-- Type-safe worktree path (prevents mixing with regular FilePaths)
newtype WorktreePath = WorktreePath FilePath

-- Specification for creating a worktree
data WorktreeSpec = WorktreeSpec
  { wsBaseName :: Text          -- Base name for the worktree
  , wsFromBranch :: Maybe Text  -- Branch to base on (Nothing = HEAD)
  }

-- Structured errors (no crashes, explicit handling required)
data WorktreeError
  = WorktreeGitError { wgeCommand :: Text, wgeExitCode :: Int, wgeStderr :: Text }
  | WorktreeFileCopyError { wfceSrcPath, wfceDestPath :: FilePath, wfceReason :: Text }

-- GADT operations (all return Either for explicit error handling)
data Worktree r where
  CreateWorktree  :: WorktreeSpec -> Worktree (Either WorktreeError WorktreePath)
  DeleteWorktree  :: WorktreePath -> Worktree (Either WorktreeError ())
  ListWorktrees   :: Worktree (Either WorktreeError [(WorktreePath, Text)])
  MergeWorktree   :: WorktreePath -> Text -> Worktree (Either WorktreeError MergeResult)
  CherryPickFiles :: WorktreePath -> [FilePath] -> FilePath -> Worktree (Either WorktreeError ())
```

## Usage Patterns

### Basic create/delete
```haskell
result <- createWorktree (WorktreeSpec "feature" Nothing)
case result of
  Left err -> handleError err
  Right path -> useWorktree path

-- Cleanup
_ <- deleteWorktree path
```

### Bracket pattern (automatic cleanup)
```haskell
result <- withWorktree (WorktreeSpec "temp" Nothing) $ \path -> do
  -- path is automatically cleaned up after this block
  doWorkInWorktree path
```

### Parallel agents with cleanup
```haskell
-- Create worktrees
wt1 <- createWorktree spec1 >>= either (error . show) pure
wt2 <- createWorktree spec2 >>= either (\e -> deleteWorktree wt1 >> error (show e)) pure

-- Run with cleanup on failure
result <- try $ runParallelWork wt1 wt2
case result of
  Left (exc :: SomeException) -> do
    deleteWorktree wt1
    deleteWorktree wt2
    throwIO exc
  Right r -> pure r
```

## Interpreter

`runWorktreeIO` interprets `Worktree` effects:

```haskell
import ExoMonad.Worktree.Interpreter (runWorktreeIO, defaultWorktreeConfig)

result <- runM
  . runWorktreeIO (defaultWorktreeConfig "/path/to/repo")
  $ createWorktree (WorktreeSpec "test" Nothing)
```

### Configuration

```haskell
data WorktreeConfig = WorktreeConfig
  { wcRepoRoot :: FilePath      -- Git repository root
  , wcWorktreeDir :: FilePath   -- Where to create worktrees (default: .worktrees/)
  }

defaultWorktreeConfig :: FilePath -> WorktreeConfig
```

## Tests

- **ParseSpec**: 10 pure tests for `parseWorktreeList`
- **InterpreterSpec**: 9 integration tests using temp git repos

Run tests:
```bash
cabal test exomonad-worktree-interpreter-test
```

## Design Notes

1. **Type-safe paths**: `WorktreePath` newtype prevents accidental FilePath mixing
2. **Explicit errors**: All operations return `Either WorktreeError a`, no crashes
3. **Resource safety**: `withWorktree` bracket for automatic cleanup
4. **Parallel-friendly**: Worktrees enable isolated parallel agent execution
