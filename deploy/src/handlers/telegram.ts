/**
 * Telegram effect handler.
 *
 * Handles TelegramSend effects by calling the Telegram Bot API.
 */

import type { TelegramSendEffect, EffectResult } from "../protocol.js";
import { successResult, errorResult } from "../protocol.js";
import { sendMessage } from "../telegram/api.js";

/**
 * Environment bindings required for Telegram handler.
 */
export interface TelegramHandlerEnv {
  TELEGRAM_TOKEN: string;
}

/**
 * Handle TelegramSend effect.
 *
 * Sends a message to the specified chat via Telegram Bot API.
 * Returns the message_id on success for tracking.
 */
export async function handleTelegramSend(
  effect: TelegramSendEffect,
  env: TelegramHandlerEnv
): Promise<EffectResult> {
  try {
    const result = await sendMessage(
      env.TELEGRAM_TOKEN,
      effect.eff_chat_id,
      effect.eff_text,
      effect.eff_parse_mode
    );

    if (!result) {
      return errorResult("Failed to send Telegram message");
    }

    return successResult({ message_id: result.message_id });
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    return errorResult(`Telegram API error: ${message}`);
  }
}
