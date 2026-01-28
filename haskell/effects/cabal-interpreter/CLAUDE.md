# Cabal Interpreter - Container-Integrated Build & Test

## Single Interpreter

`runCabalRemote` in `control-server/Effects/Cabal.hs` handles all cases:

| container_id | docker-ctl command |
|--------------|-------------------|
| `Just "exomonad-agent-123"` | `docker-ctl exec exomonad-agent-123 -- cabal ...` |
| `Nothing` | `docker-ctl exec --local -- cabal ...` |

## Interpreters

| Interpreter | Location | Use Case |
|-------------|----------|----------|
| `runCabalRemote` | `control-server/Effects/Cabal.hs` | **Unified** - Runs cabal via `docker-ctl` (either in container or local) |
| (Removed) | `runCabalStub` | Removed in favor of local execution fallback |
| (Removed) | `runCabalIO` | Removed in favor of unified `docker-ctl` path |

## How It Works

1. **Hook arrives** with `container_id` (derived from `EXOMONAD_ISSUE_ID` env var)
2. **control-server routes** to `runCabalRemote`:
   - **Present**: Use `runCabalRemote (Just container)` → `docker-ctl exec {container} cabal ...`
   - **Absent**: Use `runCabalRemote Nothing` → `docker-ctl exec --local -- cabal ...`
3. **Results** are parsed into structured `CabalResult` types for LLM consumption

## Effect Operations

| Operation | Remote Command | Returns |
|-----------|----------------|---------|
| `cabalBuild path` | `docker-ctl exec ... -- cabal build all -v0` | `CabalResult` |
| `cabalTest path` | `docker-ctl exec ... -- cabal test --test-show-details=always -v0` | `CabalResult` |
| `cabalClean path` | `docker-ctl exec ... -- cabal clean` | `CabalResult` |

## Result Types

```haskell
data CabalResult
  = CabalSuccess
  | CabalBuildFailure
      { cbfExitCode :: Int
      , cbfStderr :: Text
      , cbfStdout :: Text
      , cbfParsedErrors :: [RawCompileError]  -- Parsed GHC errors
      }
  | CabalTestFailure
      { ctfParsedFailures :: [TestFailure]  -- Parsed test failures
      , ctfRawOutput :: Text
      }
  | CabalTestSuccess
      { ctsOutput :: Text
      }
```

## Usage in Stop Hook

The Stop hook uses cabal to verify build status before allowing agent completion. The logic is now unified:

```haskell
-- In Hook.hs runStopHookLogic:
(result, finalState) <- runM
  $ runSshExec dockerCtlPath
  -- Unified path: container is optional (Maybe Text)
  $ runCabalRemote mContainerId
  $ traceCabal tracer
  $ ...
```

## Test Output Parsing

The interpreter parses GHC errors from stderr:

```haskell
-- Parses: "src/Foo.hs:42:5: error: Not in scope: 'bar'"
parseGhcErrors :: Text -> [RawCompileError]
```

Additional parsers in `Interpreter.hs` handle QuickCheck and HSpec output:

```haskell
parseQuickCheckOutput :: Text -> [TestFailure]
parseHSpecOutput :: Text -> [TestFailure]
parseTestOutput :: Text -> [TestFailure]  -- Auto-detect framework
```