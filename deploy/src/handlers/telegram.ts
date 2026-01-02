/**
 * Telegram effect handlers.
 *
 * Handles all Telegram effects (Send, Receive, TryReceive, Ask).
 * These handlers are called by TelegramDO when processing effects.
 */

import type {
  TelegramSendEffect,
  TelegramReceiveEffect,
  TelegramTryReceiveEffect,
  TelegramAskEffect,
  TelegramAskResult,
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
 * Result of an Ask handler.
 *
 * Either:
 * - Response ready (button click, text, or stale button) → return result
 * - Nothing yet → caller should block/yield until input arrives
 *
 * When yielding, returns the nonce for storage so it can be validated
 * when the callback arrives.
 */
export type AskHandlerResult =
  | { type: "result"; result: EffectResult }
  | { type: "yield"; nonce: string };

/**
 * Button callback data format with nonce for validation.
 */
interface ButtonCallbackData {
  action: string;
  nonce: string;
}

/**
 * Parse button callback data from a button click.
 * Returns null if the data is not in the expected format.
 */
function parseButtonCallbackData(data: unknown): ButtonCallbackData | null {
  if (typeof data === "object" && data !== null) {
    const obj = data as Record<string, unknown>;
    if (typeof obj.action === "string" && typeof obj.nonce === "string") {
      return { action: obj.action, nonce: obj.nonce };
    }
  }
  // Legacy format: plain string (no nonce)
  if (typeof data === "string") {
    return null; // No nonce means we can't validate
  }
  return null;
}

/**
 * Handle TelegramAsk effect (blocking).
 *
 * This effect presents inline buttons and waits for user input.
 * The user can:
 * 1. Click a button → returns { type: "button", response: "..." }
 * 2. Send text instead → returns { type: "text", text: "..." }
 * 3. Click a stale button → returns { type: "stale_button" }
 *
 * Nonce-based validation:
 * - When buttons are sent, a nonce is embedded in callback_data
 * - When a button click arrives, the nonce is validated
 * - Mismatched nonces mean the button is from an old session
 *
 * @param effect - The TelegramAsk effect from Haskell
 * @param env - Environment with TELEGRAM_TOKEN
 * @param ctx - Context with chatId and pendingMessages
 * @param storedNonce - Nonce from previous yield, or null if buttons not sent
 */
export async function handleTelegramAsk(
  effect: TelegramAskEffect,
  env: TelegramHandlerEnv,
  ctx: TelegramHandlerContext,
  storedNonce: string | null
): Promise<AskHandlerResult> {
  // If no stored nonce, buttons haven't been sent yet - send them
  if (storedNonce === null) {
    const nonce = crypto.randomUUID().slice(0, 12);
    const buttons = convertButtonsToInlineKeyboardWithNonce(effect.eff_buttons, nonce);
    const success = await sendMessageWithButtons(
      env.TELEGRAM_TOKEN,
      ctx.chatId,
      effect.eff_tg_text,
      buttons
    );

    if (!success) {
      return {
        type: "result",
        result: errorResult("Failed to send buttons"),
      };
    }

    // Yield with the nonce for storage
    return { type: "yield", nonce };
  }

  // Buttons were sent - check for responses in pending messages

  // 1. Check for button click
  const buttonClick = ctx.pendingMessages.find(
    (msg): msg is Extract<TelegramIncomingMessage, { type: "button_click" }> =>
      msg.type === "button_click"
  );

  if (buttonClick) {
    const callbackData = parseButtonCallbackData(buttonClick.data);

    if (callbackData && callbackData.nonce === storedNonce) {
      // Valid button click with matching nonce
      const result: TelegramAskResult = {
        type: "button",
        response: callbackData.action,
      };
      return { type: "result", result: successResult(result) };
    } else {
      // Stale button click (no nonce or mismatched nonce)
      console.log(`[TelegramAsk] Stale button click detected. Expected nonce: ${storedNonce}, got: ${callbackData?.nonce ?? "none"}`);
      await sendMessage(
        env.TELEGRAM_TOKEN,
        ctx.chatId,
        "That button has expired. Please use the current buttons above, or type a message."
      );
      const result: TelegramAskResult = { type: "stale_button" };
      return { type: "result", result: successResult(result) };
    }
  }

  // 2. Check for text message (user typing instead of clicking)
  const textMsg = ctx.pendingMessages.find(
    (msg): msg is Extract<TelegramIncomingMessage, { type: "text" }> =>
      msg.type === "text"
  );

  if (textMsg) {
    const result: TelegramAskResult = { type: "text", text: textMsg.text };
    return { type: "result", result: successResult(result) };
  }

  // 3. Nothing yet - keep waiting
  return { type: "yield", nonce: storedNonce };
}

/**
 * Convert Haskell-style button pairs to Telegram inline keyboard format,
 * embedding a nonce in each button's callback data.
 */
function convertButtonsToInlineKeyboardWithNonce(
  buttons: [string, string][],
  nonce: string
): TelegramInlineButton[][] {
  // Put each button in its own row, with nonce embedded in data
  return buttons.map(([label, action]) => [
    {
      text: label,
      data: { action, nonce } satisfies ButtonCallbackData,
    },
  ]);
}
