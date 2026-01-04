# GraphCLI Work Stream

## Status: Complete

GraphCLI typeclass enables wrapping any tidepool graph as a CLI tool. Entry type determines CLI arguments (derived from Haddock via TH), Exit type determines output format.

## Implementation

### Files Modified
- `tidepool-core/src/Tidepool/Graph/CLI.hs` - Complete rewrite

### Key Components

1. **`deriveCLIParser`** - TH macro that generates optparse-applicative parsers from record types
   - Reads Haddock comments for help text
   - Converts camelCase field names to kebab-case flags
   - Supports: Text, String, FilePath, Int, Integer, Double, Bool, Maybe, []
   - Sum types become subcommands
   - Rejects nested records with clear error message

2. **`runGraphCLIWith`** - Wires parser + executor + output formatting
   ```haskell
   runGraphCLIWith
     :: (Show output, ToJSON output)
     => Text -> Parser input -> (input -> IO output) -> IO ()
   ```

3. **`formatOutput`** - JSON or text output via `--format` flag
   ```haskell
   formatOutput :: (Show a, ToJSON a) => OutputFormat -> a -> Text
   ```

4. **Type families** - `GraphEntryType` / `GraphExitType` extract Entry/Exit types from graph records

5. **Validation constraints** - `ValidEntry` / `ValidExit` produce helpful type errors for missing Entry/Exit

### Requirements (TH Staging)
Same as `deriveJSONSchema`:
1. Input type in separate, already-compiled module
2. Module has `{-# LANGUAGE FieldSelectors #-}`
3. All fields have `-- ^` Haddock documentation

### Example Usage

```haskell
-- Types.hs (separate module)
{-# LANGUAGE FieldSelectors #-}

data ProcessInput = ProcessInput
  { inputFile :: FilePath
    -- ^ Path to input file
  , verbose :: Bool
    -- ^ Enable verbose output
  }

-- Main.hs
main :: IO ()
main = runGraphCLIWith
  "Process files"
  $(deriveCLIParser ''ProcessInput)
  processGraph
```

## Tests

Test suite in `tidepool-core/test/`:
- `CLITestTypes.hs` - Test types with Haddock (flat record + sum type)
- `CLISpec.hs` - Tests for deriveCLIParser, formatOutput, outputFormatParser
- `CLIGraphTypes.hs` - E2E test types (CounterInput/CounterOutput)
- `CLIGraphSpec.hs` - E2E tests: logic-only graph wired via CLI

All 120 tests pass including:
- 15 CLI derivation tests
- 11 CLI Graph E2E tests (CLI → parse → graph execute → formatted output)

## Research

- `docs/cli-research.md` - optparse-generic limitations documented
- Conclusion: TH derivation with manual optparse-applicative is the right approach
