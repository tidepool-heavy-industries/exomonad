/**
 * Telegram Bot API client.
 *
 * Minimal client for text messaging.
 * See: https://core.telegram.org/bots/api
 */

const API_BASE = "https://api.telegram.org/bot";

/**
 * Standard Telegram API response wrapper.
 */
interface TelegramApiResponse<T = unknown> {
  ok: boolean;
  result?: T;
  description?: string;
  error_code?: number;
}

/**
 * Call a Telegram Bot API method.
 * Handles network errors and non-200 responses gracefully.
 */
async function callTelegram<T>(
  token: string,
  method: string,
  body: Record<string, unknown>
): Promise<TelegramApiResponse<T>> {
  try {
    const response = await fetch(`${API_BASE}${token}/${method}`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body),
    });

    // Try to parse response as JSON regardless of status code
    // Telegram returns JSON error bodies for API errors
    const data = (await response.json()) as TelegramApiResponse<T>;
    return data;
  } catch (error) {
    // Network error or JSON parse error
    const message = error instanceof Error ? error.message : "Unknown error";
    return {
      ok: false,
      description: `Network error: ${message}`,
    };
  }
}

/**
 * Send result with message_id for tracking.
 */
export interface SendMessageResult {
  message_id: number;
}

/**
 * Send a text message to a chat.
 *
 * @param token - Bot API token
 * @param chatId - Target chat ID
 * @param text - Message text
 * @param parseMode - Optional: "HTML" or "Markdown"
 * @returns Message result with message_id, or null on failure
 */
export async function sendMessage(
  token: string,
  chatId: number,
  text: string,
  parseMode?: "HTML" | "Markdown"
): Promise<SendMessageResult | null> {
  const body: Record<string, unknown> = {
    chat_id: chatId,
    text,
  };
  if (parseMode) {
    body.parse_mode = parseMode;
  }

  const result = await callTelegram<SendMessageResult>(token, "sendMessage", body);
  if (!result.ok) {
    console.error("sendMessage failed:", result.description);
    return null;
  }
  return result.result ?? null;
}

/**
 * Send typing indicator to show bot is "working".
 * Indicator disappears after 5 seconds or when a message is sent.
 *
 * @param token - Bot API token
 * @param chatId - Target chat ID
 * @returns true on success, false on failure
 */
export async function sendTypingAction(
  token: string,
  chatId: number
): Promise<boolean> {
  const result = await callTelegram<boolean>(token, "sendChatAction", {
    chat_id: chatId,
    action: "typing",
  });
  if (!result.ok) {
    console.error("sendTypingAction failed:", result.description);
    return false;
  }
  return true;
}

/**
 * Answer a callback query to dismiss the loading state on inline keyboard.
 *
 * @param token - Bot API token
 * @param callbackQueryId - ID from the callback query
 * @param text - Optional notification text
 * @param showAlert - If true, show alert instead of toast
 */
export async function answerCallbackQuery(
  token: string,
  callbackQueryId: string,
  text?: string,
  showAlert?: boolean
): Promise<boolean> {
  const body: Record<string, unknown> = {
    callback_query_id: callbackQueryId,
  };
  if (text) {
    body.text = text;
  }
  if (showAlert) {
    body.show_alert = showAlert;
  }

  const result = await callTelegram<boolean>(token, "answerCallbackQuery", body);
  if (!result.ok) {
    console.error("answerCallbackQuery failed:", result.description);
    return false;
  }
  return true;
}

// =============================================================================
// Inline Keyboard Support
// =============================================================================

import type { TelegramInlineButton } from "tidepool-generated-ts";

/**
 * Result of sending buttons with ID mapping.
 */
export interface SendButtonsResult {
  /** Message ID from Telegram */
  message_id: number;
  /** Mapping from short IDs (btn_0, btn_1, ...) to original callback data */
  buttonMapping: Record<string, unknown>;
}

/**
 * Convert our InlineButton format to Telegram's InlineKeyboardButton,
 * generating short IDs for callback_data to avoid the 64-byte limit.
 *
 * The callback_data includes both the short ID and nonce for validation:
 * { "a": "btn_0", "n": "abc123" }
 *
 * @param button - The button to convert
 * @param index - Unique index for this button (used to generate short ID)
 * @param nonce - Nonce for validation (to detect stale buttons)
 * @param buttonMapping - Mutable mapping to populate with short ID → original data
 * @returns Telegram button with short callback_data containing ID and nonce
 */
function toTelegramButtonWithMapping(
  button: TelegramInlineButton,
  index: number,
  nonce: string,
  buttonMapping: Record<string, unknown>
): Record<string, string> {
  const shortId = `btn_${index}`;
  buttonMapping[shortId] = button.data;
  // Use short keys ("a" for action, "n" for nonce) to minimize callback_data size
  const callbackData = JSON.stringify({ a: shortId, n: nonce });
  return {
    text: button.text,
    callback_data: callbackData,
  };
}

/**
 * Send a text message with inline keyboard buttons.
 * Uses short IDs for callback_data to avoid Telegram's 64-byte limit.
 *
 * @param token - Bot API token
 * @param chatId - Target chat ID
 * @param text - Message text
 * @param buttons - 2D array of buttons (rows x columns)
 * @param nonce - Nonce for validation (to detect stale buttons)
 * @returns Result with message_id and buttonMapping, or null on failure
 */
export async function sendMessageWithButtons(
  token: string,
  chatId: number,
  text: string,
  buttons: TelegramInlineButton[][],
  nonce: string
): Promise<SendButtonsResult | null> {
  const buttonMapping: Record<string, unknown> = {};
  let buttonIndex = 0;

  const inlineKeyboard = buttons.map((row) =>
    row.map((button) => {
      const telegramButton = toTelegramButtonWithMapping(button, buttonIndex, nonce, buttonMapping);
      buttonIndex++;
      return telegramButton;
    })
  );

  const body: Record<string, unknown> = {
    chat_id: chatId,
    text,
    reply_markup: {
      inline_keyboard: inlineKeyboard,
    },
  };

  const result = await callTelegram<SendMessageResult>(token, "sendMessage", body);
  if (!result.ok) {
    console.error("sendMessageWithButtons failed:", result.description);
    return null;
  }
  if (!result.result) {
    return null;
  }
  return {
    message_id: result.result.message_id,
    buttonMapping,
  };
}

// =============================================================================
// Message Editing
// =============================================================================

/**
 * Edit a message's text and optionally remove its inline keyboard.
 * Used to update button messages after selection.
 *
 * @param token - Bot API token
 * @param chatId - Target chat ID
 * @param messageId - Message ID to edit
 * @param text - New message text
 * @param removeKeyboard - If true, removes the inline keyboard
 * @returns true on success, false on failure
 */
export async function editMessageText(
  token: string,
  chatId: number,
  messageId: number,
  text: string,
  removeKeyboard: boolean = true
): Promise<boolean> {
  const body: Record<string, unknown> = {
    chat_id: chatId,
    message_id: messageId,
    text,
  };

  if (removeKeyboard) {
    // Empty inline_keyboard array removes the keyboard
    body.reply_markup = {
      inline_keyboard: [],
    };
  }

  const result = await callTelegram<SendMessageResult>(token, "editMessageText", body);
  if (!result.ok) {
    console.error("editMessageText failed:", result.description);
    return false;
  }
  return true;
}

// =============================================================================
// Media Support (for future use)
// =============================================================================

/**
 * Send a photo by file_id (MediaHandle).
 *
 * @param token - Bot API token
 * @param chatId - Target chat ID
 * @param fileId - Telegram file_id obtained from incoming photo
 * @param caption - Optional caption
 * @returns Message result with message_id, or null on failure
 */
export async function sendPhoto(
  token: string,
  chatId: number,
  fileId: string,
  caption?: string
): Promise<SendMessageResult | null> {
  const body: Record<string, unknown> = {
    chat_id: chatId,
    photo: fileId,
  };
  if (caption) {
    body.caption = caption;
  }

  const result = await callTelegram<SendMessageResult>(token, "sendPhoto", body);
  if (!result.ok) {
    console.error("sendPhoto failed:", result.description);
    return null;
  }
  return result.result ?? null;
}

/**
 * Send a document by file_id (MediaHandle).
 *
 * @param token - Bot API token
 * @param chatId - Target chat ID
 * @param fileId - Telegram file_id obtained from incoming document
 * @param filename - Display filename
 * @returns Message result with message_id, or null on failure
 */
export async function sendDocument(
  token: string,
  chatId: number,
  fileId: string,
  filename: string
): Promise<SendMessageResult | null> {
  const body: Record<string, unknown> = {
    chat_id: chatId,
    document: fileId,
    caption: filename, // Use caption for filename display
  };

  const result = await callTelegram<SendMessageResult>(token, "sendDocument", body);
  if (!result.ok) {
    console.error("sendDocument failed:", result.description);
    return null;
  }
  return result.result ?? null;
}
