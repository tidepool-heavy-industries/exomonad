# GraphCLI Parser Derivation

Use when: Deriving CLI parsers from Haddock-documented types, adding `--format` output flags, or wiring graphs to CLI entry points.

## Core Pattern

GraphCLI enables any tidepool graph to be wrapped as a CLI tool:
- Entry type → CLI arguments (derived from Haddock via TH)
- Exit type → formatted output (JSON or text via `--format` flag)

## Key Components

### deriveCLIParser

TH macro that generates optparse-applicative parsers from record types:

```haskell
-- In Types.hs (separate module for TH staging)
{-# LANGUAGE FieldSelectors #-}

data ProcessInput = ProcessInput
  { inputFile :: FilePath
    -- ^ Path to input file to process
  , outputDir :: Maybe FilePath
    -- ^ Optional output directory
  , verbose :: Bool
    -- ^ Enable verbose logging
  }

-- In Main.hs
main = runGraphCLIWith
  "Process files with my graph"
  $(deriveCLIParser ''ProcessInput)
  runMyGraph
```

### Field Type Mapping

| Haskell Type | CLI Parser | Flag Format |
|--------------|------------|-------------|
| `Text`/`String` | `strOption` | `--field-name TEXT` |
| `FilePath` | `strOption` | `--field-name PATH` |
| `Int`/`Integer` | `option auto` | `--field-name INT` |
| `Double` | `option auto` | `--field-name NUM` |
| `Bool` | `switch` | `--field-name` (flag) |
| `Maybe a` | `optional` | `--field-name` (optional) |
| `[a]` | `many` | `--field-name` (repeatable) |

### Sum Types as Subcommands

```haskell
data Command
  = Process { file :: FilePath }
    -- ^ Process a file
  | Validate { file :: FilePath, strict :: Bool }
    -- ^ Validate a file

-- Generates:
-- mycli process --file FILE
-- mycli validate --file FILE [--strict]
```

### runGraphCLIWith

Wires parser + executor + output formatting (low-level):

```haskell
runGraphCLIWith
  :: (Show output, ToJSON output)
  => Text                   -- ^ Description for --help
  -> Parser input           -- ^ Input parser (use deriveCLIParser)
  -> (input -> IO output)   -- ^ Graph executor
  -> IO ()
```

### runGraphCLIPure

Wires a logic-only graph (no effects) directly to CLI:

```haskell
runGraphCLIPure
  :: forall graph ...
  => Text                              -- ^ Description for --help
  -> Parser (GraphEntryType graph)     -- ^ From deriveCLIParser
  -> graph (AsHandler '[])             -- ^ Handlers (empty effect stack)
  -> IO ()
```

Key constraints:
- Graph must have `Entry` and `Exit` fields
- Handlers must use empty effect stack `'[]`
- Exit type must have `Show` and `ToJSON`

This is the recommended way to wire pure graphs to CLI - no manual `runGraph` call needed.

### formatOutput

Formats output based on `--format` flag:

```haskell
-- --format json  → JSON output (via ToJSON)
-- --format text  → Text output (via Show, default)
formatOutput :: (Show a, ToJSON a) => OutputFormat -> a -> Text
```

## Requirements (TH Staging)

Same as `deriveJSONSchema`:

1. **Separate module**: Input type must be in a separate, already-compiled module
2. **FieldSelectors**: Module must have `{-# LANGUAGE FieldSelectors #-}`
3. **Haddock docs**: All fields must have `-- ^` comments (these become help text)

## Common Errors

### "Missing Haddock documentation for field"

Add `-- ^` comment after the field:

```haskell
-- Wrong
inputFile :: FilePath

-- Correct
inputFile :: FilePath
  -- ^ Path to input file
```

### "deriveCLIParser must be record or sum type"

Only flat records and sum types with record constructors are supported. Nested records are rejected.

### Parser not finding flags

Field names are converted: `inputFile` → `--input-file`

The conversion inserts hyphens at lowercase→uppercase transitions:
- `inputFile` → `input-file`
- `processURL` → `process-url` (consecutive uppercase stays together)

## Type Families for Entry/Exit

```haskell
-- Extract Entry type from graph
type GraphEntryType :: (Type -> Type) -> Type
GraphEntryType MyGraph = Input  -- if entry :: mode :- Entry Input

-- Extract Exit type from graph
type GraphExitType :: (Type -> Type) -> Type
GraphExitType MyGraph = Output  -- if exit :: mode :- Exit Output

-- Validation constraints (produce helpful errors if missing)
type ValidEntry :: Type -> Constraint
type ValidExit :: Type -> Constraint
```

## Example: Full E2E

```haskell
-- Types.hs
data CounterInput = CounterInput
  { startValue :: Int
    -- ^ Starting value
  , increment :: Int
    -- ^ Amount to add each time
  , times :: Int
    -- ^ Number of iterations
  }

-- Graph.hs
data CounterGraph mode = CounterGraph
  { cgEntry :: mode :- Entry CounterInput
  , cgCompute :: mode :- LogicNode :@ Needs '[CounterInput]
               :@ UsesEffects '[Goto Exit CounterOutput]
  , cgExit :: mode :- Exit CounterOutput
  }

counterHandlers :: CounterGraph (AsHandler '[])
counterHandlers = CounterGraph
  { cgEntry   = Proxy
  , cgCompute = \input -> pure $ gotoExit CounterOutput
      { finalValue = input.startValue + (input.increment * input.times)
      , operationsPerformed = input.times
      }
  , cgExit    = Proxy
  }

-- Main.hs (using runGraphCLIPure - recommended)
main = runGraphCLIPure
  "Counter graph CLI"
  $(deriveCLIParser ''CounterInput)
  counterHandlers
```

Usage:
```bash
./counter --start-value 10 --increment 5 --times 3 --format json
{"finalValue":25,"operationsPerformed":3}
```
