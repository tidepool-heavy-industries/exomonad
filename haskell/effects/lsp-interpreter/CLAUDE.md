# LSP Interpreter - Language Server Protocol Integration

Interprets the `LSP` effect by communicating with language servers via lsp-test.

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
│ LSP Interpreter                                                      │
│   runLSP :: LSPSession → Eff (LSP ': effs) a → Eff effs a           │
└──────────────────────────────────────┬──────────────────────────────┘
                                       │ lsp-test
                                       ▼
┌─────────────────────────────────────────────────────────────────────┐
│ haskell-language-server-wrapper                                      │
│   stdio pipes: JSON-RPC messages                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## Usage

```haskell
import ExoMonad.Effects.LSP
import ExoMonad.LSP.Interpreter (withLSPSession, runLSP)

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
| `workspaceSymbol query` | workspace/symbol | `[SymbolInformation]` |
| `completion doc pos` | textDocument/completion | `[CompletionItem]` |
| `codeActions doc pos` | textDocument/codeAction | `[CodeAction]` |
| `rename doc pos newName` | textDocument/rename | `WorkspaceEdit` |
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

### Concurrent Request Handling

The interpreter uses a **worker thread pattern** for concurrent LSP requests:

```
┌─────────────────────────────────────────────────────────────────────┐
│ withLSPSession                                                       │
│   • Spawns HLS process (stdio pipes)                                │
│   • Creates request channel (Chan LSPRequest)                        │
│   • Launches worker thread (async)                                   │
│   • Returns session handle                                           │
└──────────────────────────────────────┬───────────────────────────────┘
                                       │
         ┌─────────────────────────────┼────────────────────────┐
         ▼                             ▼                        ▼
    runLSP call 1                runLSP call 2           runLSP call 3
    (creates MVar)               (creates MVar)          (creates MVar)
         │                             │                        │
         └─────────────────────────────┼────────────────────────┘
                                       │ Chan
                                       ▼
                          ┌──────────────────────────┐
                          │ Worker Thread            │
                          │  • Reads from Chan       │
                          │  • Executes via Session  │
                          │  • Writes to MVar        │
                          └──────────────────────────┘
                                       │
                                       ▼
                            haskell-language-server
```

**Why this pattern?**
- **Thread safety**: Multiple ExoMonad agents can query LSP concurrently
- **Session reuse**: One HLS process serves many queries (semantic-scout)
- **Blocking I/O**: LSP operations block, but don't block caller threads

### Session Reuse (semantic-scout)

semantic-scout performs ~20+ LSP calls per query. Session reuse is critical:

```haskell
-- BAD: Start/stop HLS for each query (5s startup × 20 = 100s overhead)
main = do
  query1 <- withLSPSession "." $ \s -> exploreEff query1
  query2 <- withLSPSession "." $ \s -> exploreEff query2

-- GOOD: One session for all queries (5s startup once)
main = withLSPSession "." $ \session -> do
  query1 <- exploreEff session query1
  query2 <- exploreEff session query2
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
- [effects/ghci-interpreter/CLAUDE.md](../ghci-interpreter/CLAUDE.md) - Alternative for type queries
- [tools/CLAUDE.md](../../tools/CLAUDE.md) - GHCi oracle (persistent GHCi)
