# Cabal Interpreter - Container-Only Build & Test Integration

**IMPORTANT:** Cabal operations are ONLY available inside agent containers. This interpreter is designed exclusively for the Docker-based agent execution environment.

## Container-Only Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│ control-server (Haskell)                                                     │
│   receives hook with container_id from Rust exomonad                        │
└─────────────────────────────────────┬───────────────────────────────────────┘
                                      │
                    ┌─────────────────┴─────────────────┐
                    │                                   │
            container_id present              container_id absent
                    │                                   │
                    ▼                                   ▼
┌───────────────────────────────┐     ┌───────────────────────────────┐
│ runCabalRemote container      │     │ runCabalStub                  │
│   └─► docker-ctl exec         │     │   └─► CabalBuildFailure       │
│       └─► cabal build/test    │     │       "only available in      │
│                               │     │        agent containers"      │
└───────────────────────────────┘     └───────────────────────────────┘
```

## Interpreters

| Interpreter | Location | Use Case |
|-------------|----------|----------|
| `runCabalRemote` | `control-server/Effects/Cabal.hs` | **Production** - Runs cabal inside agent containers via `docker-ctl exec` |
| `runCabalStub` | `cabal-interpreter/Interpreter.hs` | **Fallback** - Fails explicitly when no container available |
| `runCabalIO` | `cabal-interpreter/Interpreter.hs` | **Legacy/Testing** - Direct local subprocess (NOT for production) |

## How It Works

1. **Hook arrives** with `container_id` (derived from `EXOMONAD_ISSUE_ID` env var)
2. **control-server routes** based on presence of `container_id`:
   - **Present**: Use `runCabalRemote container` → `docker-ctl exec {container} cabal ...`
   - **Absent**: Use `runCabalStub` → Immediate failure with clear error message
3. **Results** are parsed into structured `CabalResult` types for LLM consumption

## Required Environment

The Cabal effect requires running inside the ExoMonad Docker environment:

| Requirement | How It's Set |
|-------------|--------------|
| `EXOMONAD_ISSUE_ID` | Set by `docker-ctl spawn` when creating agent containers |
| `container_id` | Derived by Rust hook handler: `exomonad-agent-{EXOMONAD_ISSUE_ID}` |
| `docker-ctl` binary | Available in control-server container at `/usr/local/bin/docker-ctl` |

**If these aren't present, cabal operations will fail explicitly.**

## Effect Operations

| Operation | Remote Command | Returns |
|-----------|----------------|---------|
| `cabalBuild path` | `docker-ctl exec {container} -- cabal build all -v0` | `CabalResult` |
| `cabalTest path` | `docker-ctl exec {container} -- cabal test --test-show-details=always -v0` | `CabalResult` |
| `cabalClean path` | `docker-ctl exec {container} -- cabal clean` | `CabalResult` |

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

data RawCompileError = RawCompileError
  { rceFile :: FilePath
  , rceLine :: Int
  , rceMessage :: Text
  }

data TestFailure = TestFailure
  { tfPropertyName :: Text
  , tfMessage :: Text
  , tfCounterexample :: Maybe Text
  , tfSeed :: Maybe Int
  , tfLocation :: Maybe Text
  }
```

## Usage in Stop Hook

The Stop hook uses cabal to verify build status before allowing agent completion:

```haskell
-- In Hook.hs runStopHookLogic:
(result, finalState) <- case mContainerId of
  Just container ->
    runM
    $ runSshExec dockerCtlPath
    $ runCabalRemote container    -- ← Remote execution via docker-ctl
    $ traceCabal tracer
    $ runGraph stopHookHandlers agentState
  Nothing ->
    runM
    $ runCabalStub                -- ← Explicit failure, no container
    $ traceCabal tracer
    $ runGraph stopHookHandlers agentState
```

## Error Handling

When `runCabalStub` is used (no container), all operations return:

```haskell
CabalBuildFailure
  { cbfExitCode = 1
  , cbfStderr = "Cabal operations only available inside agent containers (no container_id provided)"
  , cbfStdout = ""
  , cbfParsedErrors = []
  }
```

This ensures configuration issues are caught immediately rather than silently failing.

## Test Output Parsing

The remote interpreter parses GHC errors from stderr:

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

## Related Documentation

- [control-server/CLAUDE.md](../../control-server/CLAUDE.md) - Hook handling and container integration
- [effects/CLAUDE.md](../CLAUDE.md) - Effect interpreter pattern
- [docker-ctl/CLAUDE.md](../../../rust/docker-ctl/CLAUDE.md) - Container exec implementation
