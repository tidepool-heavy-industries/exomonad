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
 */
async function callTelegram<T>(
  token: string,
  method: string,
  body: Record<string, unknown>
): Promise<TelegramApiResponse<T>> {
  const response = await fetch(`${API_BASE}${token}/${method}`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
  return response.json() as Promise<TelegramApiResponse<T>>;
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
 */
export async function sendTypingAction(
  token: string,
  chatId: number
): Promise<void> {
  await callTelegram(token, "sendChatAction", {
    chat_id: chatId,
    action: "typing",
  });
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
