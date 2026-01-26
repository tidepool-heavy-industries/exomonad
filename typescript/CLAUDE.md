# TypeScript Packages

Frontend and bot implementations consuming ExoMonad backends.

## When to Read Which CLAUDE.md

| I want to... | Read this |
|--------------|-----------|
| Work on the Solid.js debug UI | `native-gui/CLAUDE.md` |
| Work on the Telegram bot | `telegram-bot/CLAUDE.md` |
| Understand wire protocol | `../haskell/protocol/CLAUDE.md` |

## Documentation Tree

```
typescript/CLAUDE.md  ← YOU ARE HERE (router)
├── native-gui/CLAUDE.md   ← Solid.js frontend for native server
└── telegram-bot/CLAUDE.md ← Telegram bot for CF Workers
```

## Structure

| Package | Purpose | Backend |
|---------|---------|---------|
| `native-gui/` | Solid.js debug/diagnostic frontend | Native WebSocket server |
| `telegram-bot/` | Telegram bot integration | Cloudflare Workers |

## native-gui

Solid.js frontend connecting to the native Haskell server:

```bash
cd typescript/native-gui
pnpm install
pnpm dev  # Starts at localhost:3000
```

Connects to `ws://localhost:8080` (native server).

## telegram-bot

TypeScript Telegram bot handler for Cloudflare Workers:

```bash
cd typescript/telegram-bot
pnpm install
pnpm dev  # Local CF worker
```

## Related Documentation

- [haskell/native-server/CLAUDE.md](../haskell/native-server/CLAUDE.md) - Backend for native-gui
- [haskell/protocol/wire-types/CLAUDE.md](../haskell/protocol/wire-types/CLAUDE.md) - Wire protocol types
- [deploy/CLAUDE.md](../deploy/CLAUDE.md) - CF Workers deployment
