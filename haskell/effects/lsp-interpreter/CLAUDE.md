# LSP Interpreter - Language Server Protocol Integration

Interprets the `LSP` effect by communicating with language servers via lsp-client.

## When to Read This

Read this if you're:
- Adding code intelligence features to an agent
- Understanding how agents query type information
- Debugging HLS integration issues
- Working with urchin's impact analysis

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│ Agent Effects                                                        │
│   hover doc pos / references doc pos / definition doc pos           │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ LSP effect
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ LSP Interpreter                                                         │
│   runLSP :: LSPSession → Eff (LSP ': effs) a → Eff effs a           │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ lsp-client
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ haskell-language-server-wrapper                                      │
│   stdio pipes: JSON-RPC messages                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## Usage

```haskell
import Tidepool.Effects.LSP
import Tidepool.LSP.Interpreter (withLSPSession, runLSP)

main :: IO ()
main = withLSPSession "/path/to/project" $ \session -> do
  result <- runM $ runLSP session $ do
    -- Get type info at position
    info <- hover (DocUri "src/Main.hs") (Pos 10 5)

    -- Find all references
    refs <- references (DocUri "src/Main.hs") (Pos 10 5)

    -- Go to definition
    defs <- definition (DocUri "src/Main.hs") (Pos 10 5)

    pure (info, refs, defs)
  print result
```

## Effect Operations

| Operation | LSP Method | Returns |
|-----------|-----------|---------|
| `hover doc pos` | textDocument/hover | `Maybe HoverInfo` |
| `references doc pos` | textDocument/references | `[Location]` |
| `definition doc pos` | textDocument/definition | `[Location]` |
| `diagnostics doc` | (notifications) | `[Diagnostic]` |

## Session Management

Sessions are long-lived to amortize HLS startup cost:

```haskell
-- Session lifecycle
withLSPSession
  :: FilePath              -- Project root
  -> (LSPSession -> IO a)  -- Action with session
  -> IO a

-- Session is automatically cleaned up on exit
-- HLS process is terminated via RAII bracket
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `Interpreter.hs` | Effect interpreter, session management |
| `test/SmokeTest.hs` | Basic connectivity test |

## Requirements

- `haskell-language-server-wrapper` on PATH
- Project must have valid `hie.yaml` or cabal/stack config
- Initial HLS load may take several seconds

## Error Handling

LSP errors are returned as `Left` values, not thrown:

```haskell
result <- hover doc pos
case result of
  Just info -> useHoverInfo info
  Nothing   -> -- Position has no hover info
```

## Related Documentation

- [effects/CLAUDE.md](../CLAUDE.md) - Effect interpreter pattern
- [effects/ghci-executor/CLAUDE.md](../ghci-executor/CLAUDE.md) - Alternative for type queries
- [tools/CLAUDE.md](../../tools/CLAUDE.md) - GHCi oracle (persistent GHCi)
