# LSP Client Integration Skill

Use this skill when working with the `tidepool-lsp-executor` package, debugging LSP protocol issues, or extending LSP functionality.

## Package Location

`tidepool-native-gui/lsp-executor/`

## Key Files

- `src/Tidepool/LSP/Executor.hs` - Main LSP effect interpreter using `lsp-client`
- `test/SmokeTest.hs` - Smoke test against real HLS
- `tidepool-lsp-executor.cabal` - Package definition

## Library: lsp-client

Uses `lsp-client` from Hackage (not hand-rolled protocol):

```haskell
import Language.LSP.Client (runSessionWithHandles)
import Language.LSP.Client.Session (Session, initialize, request)
import qualified Language.LSP.Protocol.Types as L
import qualified Language.LSP.Protocol.Message as L
```

## Key Pattern: lsp-types 2.3

Result types use `X |? Null` instead of `Maybe X`:

```haskell
-- Correct pattern for nullable results
fromHover :: L.Hover L.|? L.Null -> Maybe HoverInfo
fromHover (L.InR L.Null) = Nothing
fromHover (L.InL h) = Just HoverInfo { ... }

-- Definition is particularly complex:
-- Definition |? ([DefinitionLink] |? Null)
fromDefinition :: L.Definition L.|? ([L.DefinitionLink] L.|? L.Null) -> [Location]
fromDefinition defOrLinksOrNull = case defOrLinksOrNull of
  L.InL (L.Definition locOrLocs) -> case locOrLocs of
    L.InL loc -> [fromLocation loc]
    L.InR locs -> map fromLocation locs
  L.InR linksOrNull -> case linksOrNull of
    L.InL links -> map fromDefinitionLink links
    L.InR L.Null -> []
```

## Key Pattern: Newtypes

Some lsp-types are newtypes wrapping `|?`:

```haskell
-- MarkedString is: newtype MarkedString = MarkedString (Text |? MarkedStringWithLanguage)
markedStringToText :: L.MarkedString -> Text
markedStringToText (L.MarkedString inner) = case inner of
  L.InL t -> t
  L.InR mswl -> mswl._value

-- Definition is: newtype Definition = Definition (Location |? [Location])
-- Must unwrap before pattern matching
```

## Session Handles Order

`runSessionWithHandles` takes `(serverOutput, serverInput)` = `(read, write)`:

```haskell
-- CORRECT: stdout (read from server) first, stdin (write to server) second
runSessionWithHandles session.lspStdout session.lspStdin $ do
  _ <- initialize Nothing
  action
```

## Running the Smoke Test

```bash
# Requires a test project at /tmp/lsp-test-project/
mkdir -p /tmp/lsp-test-project
cat > /tmp/lsp-test-project/Test.hs << 'EOF'
module Test where
add :: Int -> Int -> Int
add x y = x + y
theAnswer :: Int
theAnswer = 42
main :: IO ()
main = print (add theAnswer 1)
EOF

cat > /tmp/lsp-test-project/lsp-test.cabal << 'EOF'
cabal-version: 2.4
name: lsp-test
version: 0.1.0.0
build-type: Simple
executable lsp-test
    main-is: Test.hs
    build-depends: base
    default-language: Haskell2010
EOF

# Run test (from tidepool repo root)
cabal run lsp-smoke-test
```

## Common Issues

1. **Handle order wrong**: Symptom is immediate EOF or parse errors
2. **Missing initialize**: Must call `initialize Nothing` before requests
3. **Pattern match on Definition**: Must unwrap newtype first
4. **Null handling**: Use `L.InR L.Null` not `Nothing`
