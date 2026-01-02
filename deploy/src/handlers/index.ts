/**
 * Effect handler registry.
 *
 * Central dispatch for executing effects from WASM.
 * Each effect type maps to a handler function.
 */

import type {
  SerializableEffect,
  EffectResult,
  LlmCompleteEffect,
  LogInfoEffect,
  LogErrorEffect,
  HabiticaEffect,
} from "tidepool-generated";
import { errorResult } from "tidepool-generated";

import { handleLogInfo, handleLogError } from "./log.js";
import { handleLlmComplete, type LlmEnv } from "./llm.js";
import { handleHabitica, type HabiticaConfig } from "./habitica.js";

// Re-export handlers for direct testing
export { handleLogInfo, handleLogError } from "./log.js";
export { handleLlmComplete } from "./llm.js";
export { handleHabitica } from "./habitica.js";

// Telegram handlers (used by TelegramDO, not executeEffect - they need chat context)
export {
  handleTelegramSend,
  handleTelegramReceive,
  handleTelegramTryReceive,
  handleTelegramConfirm,
  type TelegramHandlerEnv,
  type TelegramHandlerContext,
  type ReceiveHandlerResult,
  type ConfirmHandlerResult,
} from "./telegram.js";

/**
 * Environment interface with all required bindings.
 * Note: Telegram effects are handled by TelegramDO, not executeEffect.
 */
export interface Env extends LlmEnv {
  HABITICA_USER_ID: string;
  HABITICA_API_TOKEN: string;
}

/**
 * Execute an effect and return the result.
 *
 * Dispatches to the appropriate handler based on effect type.
 * Wraps all handlers in try/catch for consistent error handling.
 */
export async function executeEffect(
  effect: SerializableEffect,
  env: Env
): Promise<EffectResult> {
  try {
    switch (effect.type) {
      case "LogInfo":
        return await handleLogInfo(effect as LogInfoEffect);

      case "LogError":
        return await handleLogError(effect as LogErrorEffect);

      case "LlmComplete":
        return await handleLlmComplete(effect as LlmCompleteEffect, env);

      case "Habitica": {
        const config: HabiticaConfig = {
          userId: env.HABITICA_USER_ID,
          apiToken: env.HABITICA_API_TOKEN,
        };
        return await handleHabitica(effect as HabiticaEffect, config);
      }

      // Telegram effects require TelegramDO context (message queue, chat ID)
      // They should be routed to TelegramDO via /effect endpoint, not executed here
      case "telegram_send":
      case "telegram_receive":
      case "telegram_try_receive":
      case "TelegramConfirm":
        return errorResult(
          `Telegram effect "${effect.type}" requires TelegramDO context. ` +
            "Route this effect to TelegramDO via the /effect endpoint."
        );

      default:
        return errorResult(`Unknown effect type: ${(effect as { type: string }).type}`);
    }
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    return errorResult(`Effect execution failed: ${message}`);
  }
}
