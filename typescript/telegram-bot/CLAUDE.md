# exomonad-telegram-ts - TypeScript Telegram Bot Handler

TypeScript Telegram bot integration for Cloudflare Workers deployment.

## What This Is

Provides Telegram bot webhook handling for ExoMonad WASM graphs running on Cloudflare Workers.

## Purpose

Handles the TypeScript side of the Telegram integration:
- Webhook endpoints for Telegram Bot API
- Message parsing and formatting
- Button/inline keyboard handling
- TypeScript effect interpreter for `EffTelegramAsk` and similar effects

## Usage

Deployed as part of the `deploy/` Cloudflare Worker. See `deploy/src/handlers/telegram.ts` for the effect handler implementation.

## Relationship to Other Packages

- **exomonad-core**: Defines `ExoMonad.Effects.Telegram` effect types (Haskell)
- **exomonad-telegram-hs**: Haskell integration (may be legacy)
- **exomonad-telegram-ts**: TypeScript bot handler (this package)
- **deploy**: Cloudflare Worker that uses telegram handlers
