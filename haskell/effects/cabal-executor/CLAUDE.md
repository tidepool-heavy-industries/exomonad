# Cabal Executor - Build & Test Integration

Interprets the `Cabal` effect by running cabal commands and parsing test output into structured failure information.

## When to Read This

Read this if you're:
- Building agents that run Haskell tests
- Understanding how test failures are structured for LLM consumption
- Working with TDD workflows that need build/test feedback
- Debugging cabal subprocess issues

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ Agent Effects                                                        │
│   cabalBuild projectPath / cabalTest projectPath                    │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ Cabal effect
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ Cabal Executor (subprocess)                                          │
│   cabal build all / cabal test all --test-show-details=always       │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ stdout/stderr
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ Test Output Parsers                                                  │
│   parseQuickCheckOutput / parseHSpecOutput / parseTestOutput        │
└─────────────────────────────────────────────────────────────────────┘
```

## Usage

```haskell
import Tidepool.Cabal.Executor (runCabalIO, defaultCabalConfig)
import Tidepool.Effects.Cabal (cabalBuild, cabalTest, CabalResult(..))

main :: IO ()
main = do
  result <- runM $ runCabalIO defaultCabalConfig $ do
    buildResult <- cabalBuild "/path/to/project"
    case buildResult of
      CabalSuccess -> cabalTest "/path/to/project"
      failure -> pure failure
  print result
```

## Effect Operations

| Operation | Command | Returns |
|-----------|---------|---------|
| `cabalBuild path` | `cabal build all` | `CabalResult` |
| `cabalTest path` | `cabal test all --test-show-details=always` | `CabalResult` |
| `cabalClean path` | `cabal clean` | `CabalResult` |

## Result Types

```haskell
data CabalResult
  = CabalSuccess
  | CabalBuildError { cbeStdout :: Text, cbeStderr :: Text }
  | CabalTestFailure
      { ctfExitCode :: Int
      , ctfStdout :: Text
      , ctfStderr :: Text
      , ctfFailures :: [TestFailure]  -- Parsed failures for LLM
      }

data TestFailure = TestFailure
  { tfTestName :: Text      -- e.g., "Spec.MyModule.should parse input"
  , tfMessage :: Text       -- Failure message
  , tfExpected :: Maybe Text
  , tfActual :: Maybe Text
  }
```

## Test Output Parsing

Parsers extract structured failures from test framework output:

```haskell
-- QuickCheck: "Failed! Falsified (after 42 tests): ..."
parseQuickCheckOutput :: Text -> [TestFailure]

-- HSpec: "  1) MyModule should work"
parseHSpecOutput :: Text -> [TestFailure]

-- Auto-detect framework
parseTestOutput :: Text -> [TestFailure]
```

## Configuration

```haskell
data CabalConfig = CabalConfig
  { ccVerbosity :: Int         -- 0 = quiet, 1 = normal, 2 = verbose
  , ccShowTestDetails :: Bool  -- --test-show-details=always
  , ccTimeout :: Maybe Int     -- Subprocess timeout (seconds)
  }

defaultCabalConfig :: CabalConfig
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `Executor.hs` | Effect interpreter, subprocess management |
| `test/ParserSpec.hs` | Parser unit tests |

## LLM Integration

The structured `TestFailure` type is designed for LLM consumption:
- Test names provide context ("which test failed")
- Expected/actual values enable targeted fixes
- Messages explain why the test failed

This enables TDD loops where the LLM can:
1. See failing test details
2. Generate targeted fixes
3. Verify with another test run

## Related Documentation

- [effects/CLAUDE.md](../CLAUDE.md) - Effect interpreter pattern
- [types-first-dev/CLAUDE.md](../../../types-first-dev/CLAUDE.md) - TDD workflow using Cabal
