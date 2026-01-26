# ghci-oracle - Persistent GHCi Session Server

Standalone server that maintains a persistent GHCi session, exposing it via socket interface for type queries, expression evaluation, and compile checks.

## Why Standalone?

GHCi dependencies (process management, GHC internals) are heavy. Keeping this as a separate binary:
- Avoids polluting main exomonad build with heavy deps
- Enables independent versioning and deployment
- Isolates GHCi subprocess crashes from the main application

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│ Client (exomonad-ghci-interpreter)                                 │
│   runGHCiIO conn $ queryType "fmap"                            │
└─────────────────────────────────────────────────────────────────┘
                              │
                    TCP socket (length-prefixed JSON)
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ ghci-oracle server                                              │
│   ├─ Server.hs    - Socket accept loop, request dispatch       │
│   ├─ Session.hs   - GHCi subprocess lifecycle, MVar mutex      │
│   ├─ Protocol.hs  - Length-prefixed JSON framing               │
│   └─ Types.hs     - Wire protocol types (must match client)    │
└─────────────────────────────────────────────────────────────────┘
                              │
                    stdin/stdout pipes
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ GHCi subprocess (cabal repl)                                    │
│   Custom prompt: <<<GHCI_READY>>>                              │
└─────────────────────────────────────────────────────────────────┘
```

## Usage

```bash
# Start server (separate terminal)
cd tools/ghci-oracle
cabal run ghci-oracle -- --port 9999 --project /path/to/project

# Options
ghci-oracle --help
  --port PORT           Port to listen on (default: 9999)
  --project DIR         Project root directory (default: .)
  --module MODULE       Module to load at startup (repeatable)
  --ghci-command CMD    Command to start GHCi (default: "cabal repl")
  --query-timeout MS    Query timeout in milliseconds (default: 10000)
  --startup-timeout MS  Startup timeout in milliseconds (default: 60000)
  --no-restart          Disable auto-restart on crash
  --max-restarts N      Maximum restart attempts (default: 3)
  --verbose             Verbose logging
```

## Wire Protocol

Length-prefixed JSON over TCP:
- 4 bytes: message length (big-endian)
- N bytes: JSON payload

Request types:
- `ReqQueryType Text` - `:type expression`
- `ReqQueryInfo Text` - `:info name`
- `ReqQueryKind Text` - `:kind type`
- `ReqEvaluate Text` - evaluate expression
- `ReqCheckCompiles Text` - type-check without execution
- `ReqLoadModule Text` - `:load Module`
- `ReqReloadModules` - `:reload`
- `ReqPing` - health check

Response types:
- `RespSuccess Text` - successful query result
- `RespBool Bool` - boolean result (CheckCompiles)
- `RespUnit` - success with no result (Load, Reload)
- `RespError GHCiError` - error with details
- `RespPong` - ping response

## Session Management

- **MVar mutex**: Serializes queries (one at a time)
- **TVar state**: Tracks session lifecycle (NotStarted, Starting, Running, Crashed, Stopped)
- **Auto-restart**: On crash or timeout, session restarts automatically (up to `maxRestarts`)
- **Prompt detection**: Custom `<<<GHCI_READY>>>` marker for reliable output boundary detection

## Error Handling

| Error | Cause | Recovery |
|-------|-------|----------|
| `GHCiTimeout` | Query exceeded timeout | Session marked crashed, auto-restarts |
| `GHCiParseError` | GHCi reported type/parse error | Session remains valid |
| `GHCiLoadError` | Module failed to load | Session remains valid |
| `GHCiSessionCrashed` | GHCi process died | Auto-restart if enabled |

## Security Considerations

**⚠️ Local development only.** The oracle server:
- Binds to `127.0.0.1` (localhost only) by default
- Has **no authentication** - any local process can connect
- Can **execute arbitrary Haskell code** via `Evaluate` requests
- Runs with the permissions of the invoking user

Do not expose this server to untrusted networks. The lack of authentication is intentional for local development convenience, but means any local process can execute code in the GHCi session.

## Building

```bash
cd tools/ghci-oracle
cabal build
```

Note: This package has its own `cabal.project` and is NOT part of the main exomonad build. This is intentional to avoid dependency pollution.

## Related

- [exomonad-core/src/ExoMonad/Effect/GHCi.hs](../../exomonad-core/src/ExoMonad/Effect/GHCi.hs) - Effect type and wire protocol types
- [exomonad-native-gui/ghci-interpreter/](../../exomonad-native-gui/ghci-interpreter/) - Thin client that connects to this server
