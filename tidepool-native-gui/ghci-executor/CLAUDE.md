# ghci-executor - GHCi Effect Thin Client

Interprets the `GHCi` effect by connecting to a `ghci-oracle` server via TCP socket. Minimal dependencies (no process, no ghc-*) to keep it lightweight.

## Usage

```haskell
import Tidepool.Effect.GHCi (queryType, GHCiError)
import Tidepool.GHCi.Executor (runGHCiIO, withGHCiConnection)

-- Connect and run queries
main :: IO ()
main = do
  result <- withGHCiConnection "127.0.0.1" 9999 $ \conn ->
    runM $ runGHCiIO conn $ do
      typeInfo <- queryType "fmap"
      pure typeInfo
  print result
```

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│ Your Application                                                │
│   runGHCiIO conn $ queryType "fmap"                            │
└─────────────────────────────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────────────────────────────┐
│ Executor.hs                                                     │
│   interpret GHCi effect → sendRequest/receiveResponse          │
└─────────────────────────────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────────────────────────────┐
│ Protocol.hs                                                     │
│   Length-prefixed JSON over TCP socket                         │
└─────────────────────────────────────────────────────────────────┘
         │
    TCP socket
         │
         ▼
┌─────────────────────────────────────────────────────────────────┐
│ ghci-oracle server (tools/ghci-oracle/)                        │
└─────────────────────────────────────────────────────────────────┘
```

## API

### Connection Management

```haskell
-- Bracket-style (recommended)
withGHCiConnection :: String -> Int -> (GHCiConnection -> IO a) -> IO (Either GHCiError a)

-- Manual management
connectToOracle :: GHCiClientConfig -> IO (Either GHCiError GHCiConnection)
disconnectFromOracle :: GHCiConnection -> IO ()
```

### Effect Interpreter

```haskell
runGHCiIO :: LastMember IO effs => GHCiConnection -> Eff (GHCi ': effs) a -> Eff effs a
```

## Wire Protocol

Uses length-prefixed JSON (4-byte big-endian length + JSON payload). Types defined in `tidepool-core/src/Tidepool/Effect/GHCi.hs`.

## Tests

```bash
cabal test tidepool-ghci-executor
```

Tests cover:
- Length encoding/decoding (38 tests)
- JSON serialization round-trips for all request/response types
- Cross-compatibility with oracle server format

## Related

- [tidepool-core/src/Tidepool/Effect/GHCi.hs](../../tidepool-core/src/Tidepool/Effect/GHCi.hs) - Effect type definition
- [tools/ghci-oracle/](../../tools/ghci-oracle/) - Server this client connects to
