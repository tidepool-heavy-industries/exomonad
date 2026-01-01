/**
 * Telegram effect handlers.
 *
 * Handles all Telegram effects (Send, Receive, TryReceive).
 * These handlers are called by TelegramDO when processing effects.
 */

import type {
  TelegramSendEffect,
  TelegramReceiveEffect,
  TelegramTryReceiveEffect,
  TelegramOutgoingMessage,
  TelegramIncomingMessage,
  EffectResult,
} from "../protocol.js";
import {
  successResult,
  errorResult,
  telegramUnitResult,
  telegramMessagesResult,
} from "../protocol.js";
import {
  sendMessage,
  sendMessageWithButtons,
  sendPhoto,
  sendDocument,
} from "../telegram/api.js";

/**
 * Environment bindings required for Telegram handlers.
 */
export interface TelegramHandlerEnv {
  TELEGRAM_TOKEN: string;
}

/**
 * Context passed to Telegram handlers from TelegramDO.
 */
export interface TelegramHandlerContext {
  /** The chat ID this DO is handling */
  chatId: number;
  /** Current pending messages (for Receive/TryReceive) */
  pendingMessages: TelegramIncomingMessage[];
}

// =============================================================================
// Effect Handlers
// =============================================================================

/**
 * Send an outgoing message based on its type.
 */
async function sendOutgoingMessage(
  token: string,
  chatId: number,
  message: TelegramOutgoingMessage
): Promise<boolean> {
  switch (message.type) {
    case "text": {
      const result = await sendMessage(token, chatId, message.text);
      return result !== null;
    }
    case "buttons": {
      const result = await sendMessageWithButtons(
        token,
        chatId,
        message.text,
        message.buttons
      );
      return result !== null;
    }
    case "photo": {
      const result = await sendPhoto(token, chatId, message.media, message.caption);
      return result !== null;
    }
    case "document": {
      const result = await sendDocument(
        token,
        chatId,
        message.media,
        message.filename
      );
      return result !== null;
    }
    default: {
      // TypeScript exhaustiveness check
      const _exhaustive: never = message;
      console.error("Unknown message type:", _exhaustive);
      return false;
    }
  }
}

/**
 * Handle telegram_send effect (fire and forget).
 *
 * Sends the OutgoingMessage to the chat.
 * Returns unit on success.
 */
export async function handleTelegramSend(
  effect: TelegramSendEffect,
  env: TelegramHandlerEnv,
  ctx: TelegramHandlerContext
): Promise<EffectResult> {
  try {
    const success = await sendOutgoingMessage(
      env.TELEGRAM_TOKEN,
      ctx.chatId,
      effect.message
    );

    if (!success) {
      return errorResult("Failed to send Telegram message");
    }

    return successResult(telegramUnitResult());
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    return errorResult(`Telegram API error: ${message}`);
  }
}

/**
 * Result of a Receive handler.
 *
 * Either:
 * - Messages are available → return them immediately
 * - No messages → caller should block/yield until messages arrive
 */
export type ReceiveHandlerResult =
  | { type: "messages"; result: EffectResult }
  | { type: "yield" };

/**
 * Handle telegram_receive effect (blocking).
 *
 * If pending messages exist, returns them immediately.
 * If no messages, returns { type: "yield" } to signal the caller
 * should block until messages arrive.
 *
 * Note: The TelegramDO is responsible for:
 * 1. Storing that we're waiting for a Receive
 * 2. Resuming when webhook delivers a message
 */
export function handleTelegramReceive(
  _effect: TelegramReceiveEffect,
  ctx: TelegramHandlerContext
): ReceiveHandlerResult {
  if (ctx.pendingMessages.length > 0) {
    // Messages available - return them
    return {
      type: "messages",
      result: successResult(telegramMessagesResult(ctx.pendingMessages)),
    };
  }

  // No messages - caller should yield/block
  return { type: "yield" };
}

/**
 * Handle telegram_try_receive effect (non-blocking).
 *
 * Returns all pending messages immediately, even if empty.
 */
export function handleTelegramTryReceive(
  _effect: TelegramTryReceiveEffect,
  ctx: TelegramHandlerContext
): EffectResult {
  // Always return immediately with current pending messages
  return successResult(telegramMessagesResult(ctx.pendingMessages));
}
