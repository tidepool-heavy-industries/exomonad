/**
 * Telegram Bot API types.
 *
 * Minimal subset for text-only messaging.
 * Ported from shoal/anemone-telegram with simplifications.
 */

// =============================================================================
// Telegram API Types
// =============================================================================

/**
 * Incoming update from Telegram webhook.
 * See: https://core.telegram.org/bots/api#update
 */
export interface TelegramUpdate {
  update_id: number;
  message?: TelegramMessage;
  callback_query?: TelegramCallbackQuery;
}

/**
 * A Telegram message.
 * See: https://core.telegram.org/bots/api#message
 */
export interface TelegramMessage {
  message_id: number;
  chat: TelegramChat;
  from?: TelegramUser;
  text?: string;
  date: number;
  reply_to_message?: TelegramMessage;
}

/**
 * Callback query from inline keyboard button.
 * See: https://core.telegram.org/bots/api#callbackquery
 */
export interface TelegramCallbackQuery {
  id: string;
  from: TelegramUser;
  message?: TelegramMessage;
  chat_instance: string;
  data?: string;
}

/**
 * A Telegram chat.
 * See: https://core.telegram.org/bots/api#chat
 */
export interface TelegramChat {
  id: number;
  type: "private" | "group" | "supergroup" | "channel";
  title?: string;
  username?: string;
  first_name?: string;
}

/**
 * A Telegram user.
 * See: https://core.telegram.org/bots/api#user
 */
export interface TelegramUser {
  id: number;
  is_bot: boolean;
  first_name: string;
  username?: string;
}

// =============================================================================
// Environment Types
// =============================================================================

/**
 * Environment bindings for Telegram integration.
 */
export interface TelegramEnv {
  /** Bot API token from @BotFather */
  TELEGRAM_TOKEN: string;
  /** Secret for webhook verification */
  TELEGRAM_WEBHOOK_SECRET: string;
  /** Optional comma-separated list of allowed user IDs */
  ALLOWED_TELEGRAM_USERS?: string;
}

// =============================================================================
// Helper Functions
// =============================================================================

/**
 * Extract chat_id from any update type.
 */
export function extractChatId(update: TelegramUpdate): number | null {
  if (update.message?.chat?.id) {
    return update.message.chat.id;
  }
  if (update.callback_query?.message?.chat?.id) {
    return update.callback_query.message.chat.id;
  }
  return null;
}

/**
 * Extract text content from an update.
 * Returns message text or callback query data.
 */
export function extractText(update: TelegramUpdate): string | null {
  if (update.message?.text) {
    return update.message.text;
  }
  if (update.callback_query?.data) {
    return update.callback_query.data;
  }
  return null;
}

/**
 * Extract the user who sent the update.
 */
export function extractUser(update: TelegramUpdate): TelegramUser | null {
  if (update.message?.from) {
    return update.message.from;
  }
  if (update.callback_query?.from) {
    return update.callback_query.from;
  }
  return null;
}

/**
 * Check if a user is in the allowlist.
 * If the allowlist is empty or unset, all users are allowed.
 */
export function isAllowedUser(
  userId: number | undefined,
  allowedUsers: string | undefined
): boolean {
  if (!userId) return false;
  // No allowlist = everyone allowed
  if (!allowedUsers || allowedUsers.trim() === "") return true;
  const allowed = allowedUsers
    .split(",")
    .map((id) => id.trim())
    .filter(Boolean);
  if (allowed.length === 0) return true;
  return allowed.includes(String(userId));
}
