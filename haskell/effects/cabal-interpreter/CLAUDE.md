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
      , cbfStderr :: Text      -- Raw stderr output
      , cbfStdout :: Text      -- Raw stdout output
      }
  | CabalTestFailure Text      -- Raw test output (unparsed)
  | CabalTestSuccess Text      -- Raw test output (unparsed)
```

**Philosophy:** Claude can read error messages. We pass raw output to templates without parsing. This is simpler and more robust than brittle regex parsing that was removed in PR #381.

## Usage in Stop Hook

The Stop hook uses cabal to verify build status before allowing agent completion. Templates receive raw output via `{{ raw_output | truncate(N) }}`.

```haskell
-- In Hook.hs runStopHookLogic:
(result, finalState) <- runM
  $ runSshExec dockerCtlPath
  -- Unified path: container is optional (Maybe Text)
  $ runCabalRemote mContainerId
  $ traceCabal tracer
  $ ...
```