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
  TelegramConfirmEffect,
  TelegramConfirmResponse,
  TelegramOutgoingMessage,
  TelegramIncomingMessage,
  TelegramInlineButton,
  EffectResult,
} from "tidepool-generated-ts";
import {
  successResult,
  errorResult,
  telegramUnitResult,
  telegramMessagesResult,
} from "tidepool-generated-ts";
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

/**
 * Result of a Confirm handler.
 *
 * Either:
 * - Button click already in queue → return parsed response
 * - No button click yet → caller should block/yield until one arrives
 */
export type ConfirmHandlerResult =
  | { type: "result"; result: EffectResult }
  | { type: "yield" };

/**
 * Handle TelegramConfirm effect (blocking).
 *
 * 1. Sends message with inline keyboard buttons
 * 2. Checks if a button_click is already in pendingMessages
 *    - If yes: parses it into TelegramConfirmResponse and returns
 *    - If no: returns { type: "yield" } to block until button is clicked
 *
 * Note: The TelegramDO is responsible for:
 * 1. Sending the buttons on first call
 * 2. Storing that we're waiting for a confirm response
 * 3. Resuming when webhook delivers a callback_query
 */
export async function handleTelegramConfirm(
  effect: TelegramConfirmEffect,
  env: TelegramHandlerEnv,
  ctx: TelegramHandlerContext,
  buttonsSent: boolean
): Promise<ConfirmHandlerResult> {
  // If buttons haven't been sent yet, send them first
  if (!buttonsSent) {
    const buttons = convertButtonsToInlineKeyboard(effect.eff_buttons);
    const success = await sendMessageWithButtons(
      env.TELEGRAM_TOKEN,
      ctx.chatId,
      effect.eff_message,
      buttons
    );

    if (!success) {
      return {
        type: "result",
        result: errorResult("Failed to send confirmation buttons"),
      };
    }
  }

  // Check if there's already a button_click in pending messages
  const buttonClick = ctx.pendingMessages.find(
    (msg): msg is Extract<TelegramIncomingMessage, { type: "button_click" }> =>
      msg.type === "button_click"
  );

  if (buttonClick) {
    // Parse the button click data into TelegramConfirmResponse
    const response = parseButtonClickToConfirmResponse(
      buttonClick.data,
      effect.eff_buttons
    );
    return {
      type: "result",
      result: successResult(response),
    };
  }

  // No button click yet - caller should yield/block
  return { type: "yield" };
}

/**
 * Convert Haskell-style button pairs to Telegram inline keyboard format.
 * Each button goes in its own row for clear layout.
 */
function convertButtonsToInlineKeyboard(
  buttons: [string, string][]
): TelegramInlineButton[][] {
  // Put each button in its own row
  return buttons.map(([label, value]) => [{ text: label, data: value }]);
}

/**
 * Parse a button click's callback data into TelegramConfirmResponse.
 *
 * The callback data is the "value" from the button pair.
 * Maps "approved", "denied", "skipped" to the response format.
 */
function parseButtonClickToConfirmResponse(
  data: unknown,
  _buttons: [string, string][]
): TelegramConfirmResponse {
  // The callback data should be one of: "approved", "denied", "skipped"
  const value = typeof data === "string" ? data : String(data);

  switch (value) {
    case "approved":
      return { response: "approved" };
    case "denied":
      // For now, no feedback - could be enhanced later with a follow-up prompt
      return { response: "denied" };
    case "skipped":
      return { response: "skipped" };
    default:
      // Unknown value - treat as skipped
      console.warn(`[TelegramConfirm] Unknown button value: ${value}, treating as skipped`);
      return { response: "skipped" };
  }
}
