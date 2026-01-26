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
} from "exomonad-generated-ts";
import {
  successResult,
  errorResult,
  telegramUnitResult,
  telegramMessagesResult,
} from "exomonad-generated-ts";
import {
  sendMessage,
  sendMessageWithButtons,
  sendPhoto,
  sendDocument,
  editMessageText,
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
  /** Thread ID for topic-based routing (Bot API 9.3+) */
  threadId?: number | null;
  /** Current pending messages (for Receive/TryReceive) */
  pendingMessages: TelegramIncomingMessage[];
  /** Button ID mapping (btn_0 → original data) for resolving callbacks */
  buttonMapping?: Record<string, unknown>;
  /** Message ID of the button message (for editing after selection) */
  buttonMessageId?: number | null;
  /** Original question text (for showing selection confirmation) */
  buttonQuestionText?: string | null;
}

// =============================================================================
// Effect Handlers
// =============================================================================

/**
 * Send an outgoing message based on its type.
 * Auto-injects threadId if provided in context.
 */
async function sendOutgoingMessage(
  token: string,
  chatId: number,
  message: TelegramOutgoingMessage,
  threadId?: number | null
): Promise<boolean> {
  // Convert null to undefined for API calls
  const thread = threadId ?? undefined;

  switch (message.type) {
    case "text": {
      const result = await sendMessage(token, chatId, message.text, undefined, thread);
      return result !== null;
    }
    case "buttons": {
      const result = await sendMessageWithButtons(
        token,
        chatId,
        message.text,
        message.buttons,
        "nonce-placeholder", // TODO: Get actual nonce from context
        thread
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
      effect.message,
      ctx.threadId  // Auto-inject threadId from context
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
 * when the callback arrives. Also returns buttonMapping for ID resolution,
 * messageId for editing after selection, and questionText for the confirmation.
 */
export type AskHandlerResult =
  | { type: "result"; result: EffectResult }
  | {
      type: "yield";
      nonce: string;
      buttonMapping: Record<string, unknown>;
      messageId: number;
      questionText: string;
    };

/**
 * Button callback data format with nonce for validation.
 */
interface ButtonCallbackData {
  action: string;  // Short button ID (btn_0, btn_1, ...)
  nonce: string;   // For detecting stale buttons
}

/**
 * Parse button callback data from a button click.
 * Supports compact format { a, n } and legacy format { action, nonce }.
 * Returns null if the data is not in the expected format.
 */
function parseButtonCallbackData(data: unknown): ButtonCallbackData | null {
  if (typeof data === "object" && data !== null) {
    const obj = data as Record<string, unknown>;
    // Compact format: { a: "btn_0", n: "nonce" }
    if (typeof obj.a === "string" && typeof obj.n === "string") {
      return { action: obj.a, nonce: obj.n };
    }
    // Legacy format: { action: "...", nonce: "..." }
    if (typeof obj.action === "string" && typeof obj.nonce === "string") {
      return { action: obj.action, nonce: obj.nonce };
    }
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
 * Button ID mapping:
 * - When buttons are sent, short IDs (btn_0, btn_1, ...) are used as callback_data
 * - The mapping from short IDs to original data is stored and returned
 * - When a button click arrives, the short ID is resolved back to original data
 *
 * Nonce-based validation:
 * - When buttons are sent, a nonce is embedded in callback_data alongside the short ID
 * - When a button click arrives, the nonce is validated
 * - Mismatched nonces mean the button is from an old session
 *
 * @param effect - The TelegramAsk effect from Haskell
 * @param env - Environment with TELEGRAM_TOKEN
 * @param ctx - Context with chatId, pendingMessages, and buttonMapping
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
    const buttons = convertButtonsToInlineKeyboard(effect.eff_buttons);
    const sendResult = await sendMessageWithButtons(
      env.TELEGRAM_TOKEN,
      ctx.chatId,
      effect.eff_tg_text,
      buttons,
      nonce,
      ctx.threadId ?? undefined  // Auto-inject threadId from context
    );

    if (!sendResult) {
      return {
        type: "result",
        result: errorResult("Failed to send buttons"),
      };
    }

    // Yield with the nonce, button mapping, message ID, and question text for storage
    return {
      type: "yield",
      nonce,
      buttonMapping: sendResult.buttonMapping,
      messageId: sendResult.message_id,
      questionText: effect.eff_tg_text,
    };
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
      // Resolve the short button ID back to original data
      const shortId = callbackData.action;
      const storedData = ctx.buttonMapping?.[shortId];

      if (storedData !== undefined) {
        // storedData is the original button data (typically a string)
        const responseValue = typeof storedData === "string"
          ? storedData
          : JSON.stringify(storedData);

        // Edit the original button message to show selection and remove buttons
        if (ctx.buttonMessageId) {
          const buttonLabel = findButtonLabel(effect.eff_buttons, responseValue);
          const displayText = buttonLabel ?? truncateForDisplay(responseValue, 50);
          const confirmationText = `${ctx.buttonQuestionText ?? "Question"}\n\n✓ ${displayText}`;
          try {
            await editMessageText(
              env.TELEGRAM_TOKEN,
              ctx.chatId,
              ctx.buttonMessageId,
              confirmationText
            );
          } catch (err) {
            // Non-critical: user's selection is still recorded even if edit fails
            console.error("[TelegramAsk] Failed to edit confirmation message:", {
              chatId: ctx.chatId,
              messageId: ctx.buttonMessageId,
              error: err,
            });
          }
        }

        const result: TelegramAskResult = {
          type: "button",
          response: responseValue,
        };
        return { type: "result", result: successResult(result) };
      } else {
        // Button ID not found in mapping - shouldn't happen but handle gracefully
        console.error(`[TelegramAsk] Button ID "${shortId}" not found in mapping:`, ctx.buttonMapping);
        const result: TelegramAskResult = {
          type: "button",
          response: shortId, // Fallback to the short ID
        };
        return { type: "result", result: successResult(result) };
      }
    } else {
      // Stale button click (no nonce or mismatched nonce)
      console.log(`[TelegramAsk] Stale button click detected. Expected nonce: ${storedNonce}, got: ${callbackData?.nonce ?? "none"}`);
      await sendMessage(
        env.TELEGRAM_TOKEN,
        ctx.chatId,
        "That button has expired. Please use the current buttons above, or type a message.",
        undefined,
        ctx.threadId ?? undefined
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
    // Edit the original button message to show text response and remove buttons
    if (ctx.buttonMessageId) {
      const confirmationText = `${ctx.buttonQuestionText ?? "Question"}\n\n✓ (typed) ${truncateForDisplay(textMsg.text, 50)}`;
      try {
        await editMessageText(
          env.TELEGRAM_TOKEN,
          ctx.chatId,
          ctx.buttonMessageId,
          confirmationText
        );
      } catch (err) {
        // Non-critical: user's response is still recorded even if edit fails
        console.error("[TelegramAsk] Failed to edit confirmation message:", {
          chatId: ctx.chatId,
          messageId: ctx.buttonMessageId,
          error: err,
        });
      }
    }

    const result: TelegramAskResult = { type: "text", text: textMsg.text };
    return { type: "result", result: successResult(result) };
  }

  // 3. Nothing yet - keep waiting (preserve existing state)
  // Note: If storedNonce exists but buttonMessageId is null/0, that's an inconsistent state
  // (buttons were sent but we don't know their message ID). Log a warning but continue.
  // The editMessageText calls are guarded by `if (ctx.buttonMessageId)` which will skip
  // editing if messageId is 0 (falsy), so using 0 as fallback is safe.
  if (!ctx.buttonMessageId) {
    console.warn("[TelegramAsk] Waiting with stored nonce but missing buttonMessageId:", {
      chatId: ctx.chatId,
      nonce: storedNonce,
    });
  }
  return {
    type: "yield",
    nonce: storedNonce,
    buttonMapping: ctx.buttonMapping ?? {},
    messageId: ctx.buttonMessageId ?? 0,
    questionText: ctx.buttonQuestionText ?? "",
  };
}

/**
 * Find the button label for a given action value.
 */
function findButtonLabel(buttons: [string, string][], action: string): string | null {
  for (const [label, value] of buttons) {
    if (value === action) {
      return label;
    }
  }
  return null;
}

/**
 * Truncate a string for display, adding ellipsis if too long.
 */
function truncateForDisplay(text: string, maxLength: number): string {
  if (text.length <= maxLength) {
    return text;
  }
  return text.slice(0, maxLength - 1) + "…";
}

/**
 * Convert Haskell-style button pairs to Telegram inline keyboard format.
 * Each button goes in its own row with the action as data.
 */
function convertButtonsToInlineKeyboard(
  buttons: [string, string][]
): TelegramInlineButton[][] {
  return buttons.map(([label, action]) => [
    {
      text: label,
      data: action,  // Original action value, will be stored in mapping
    },
  ]);
}
