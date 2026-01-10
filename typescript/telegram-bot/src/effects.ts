/**
 * Telegram effect types for WASM ↔ TypeScript communication.
 *
 * These types define the serialized effects that flow between
 * Haskell agent code (compiled to WASM) and the TypeScript runtime.
 *
 * Protocol:
 * 1. WASM yields a TelegramEffect
 * 2. TypeScript executes the effect (calls Telegram API)
 * 3. TypeScript returns a TelegramEffectResult
 * 4. WASM continues execution
 */

import type { OutgoingMessage, IncomingMessage } from './types.js';

// ══════════════════════════════════════════════════════════════
// EFFECTS (Haskell → TypeScript)
// ══════════════════════════════════════════════════════════════

/**
 * Telegram effects yielded by the agent.
 */
export type TelegramEffect =
  | SendEffect
  | ReceiveEffect
  | TryReceiveEffect;

/**
 * Send a message (fire and forget).
 *
 * Mirrors Haskell: Send :: OutgoingMessage -> Telegram m ()
 */
export interface SendEffect {
  type: 'telegram_send';
  message: OutgoingMessage;
}

/**
 * Block until at least one message arrives.
 *
 * The runtime should block until there's at least one message,
 * then return all pending messages.
 *
 * Mirrors Haskell: Receive :: Telegram m (NonEmpty IncomingMessage)
 */
export interface ReceiveEffect {
  type: 'telegram_receive';
}

/**
 * Non-blocking check for pending messages.
 *
 * Returns immediately with any pending messages (may be empty).
 *
 * Mirrors Haskell: TryReceive :: Telegram m [IncomingMessage]
 */
export interface TryReceiveEffect {
  type: 'telegram_try_receive';
}

// ══════════════════════════════════════════════════════════════
// RESULTS (TypeScript → Haskell)
// ══════════════════════════════════════════════════════════════

/**
 * Results returned to the agent after effect execution.
 */
export type TelegramEffectResult =
  | UnitResult
  | MessagesResult;

/**
 * Unit result (for Send).
 */
export interface UnitResult {
  type: 'unit';
}

/**
 * Messages result (for Receive and TryReceive).
 *
 * For Receive: guaranteed to have at least one message.
 * For TryReceive: may be empty.
 */
export interface MessagesResult {
  type: 'messages';
  messages: IncomingMessage[];
}

// ══════════════════════════════════════════════════════════════
// HELPERS
// ══════════════════════════════════════════════════════════════

/** Create a unit result. */
export function unitResult(): UnitResult {
  return { type: 'unit' };
}

/** Create a messages result. */
export function messagesResult(messages: IncomingMessage[]): MessagesResult {
  return { type: 'messages', messages };
}

/** Type guard for SendEffect. */
export function isSendEffect(effect: TelegramEffect): effect is SendEffect {
  return effect.type === 'telegram_send';
}

/** Type guard for ReceiveEffect. */
export function isReceiveEffect(effect: TelegramEffect): effect is ReceiveEffect {
  return effect.type === 'telegram_receive';
}

/** Type guard for TryReceiveEffect. */
export function isTryReceiveEffect(effect: TelegramEffect): effect is TryReceiveEffect {
  return effect.type === 'telegram_try_receive';
}
