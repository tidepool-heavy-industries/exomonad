/**
 * Effect handler registry.
 *
 * Central dispatch for executing effects from WASM.
 * Uses generated dispatcher from tidepool-generated.
 */

import type {
  SerializableEffect,
  EffectResult,
  InternalEffectHandlers,
} from "tidepool-generated-ts";
import { dispatchInternalEffect, errorResult, isYieldedEffect } from "tidepool-generated-ts";

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
 * Handler registry - implementations for each internal effect type.
 * Wired to the generated InternalEffectHandlers interface.
 */
const internalHandlers: InternalEffectHandlers<Env> = {
  LogInfo: (effect, _env) => handleLogInfo(effect),
  LogError: (effect, _env) => handleLogError(effect),
  LlmComplete: (effect, env) => handleLlmComplete(effect, env),
  Habitica: (effect, env) => {
    const config: HabiticaConfig = {
      userId: env.HABITICA_USER_ID,
      apiToken: env.HABITICA_API_TOKEN,
    };
    return handleHabitica(effect, config);
  },
};

/**
 * Execute an effect and return the result.
 *
 * Dispatches to the appropriate handler based on effect type.
 * Uses generated dispatcher for internal effects.
 * Wraps all handlers in try/catch for consistent error handling.
 */
export async function executeEffect(
  effect: SerializableEffect,
  env: Env
): Promise<EffectResult> {
  try {
    // Yielded effects require caller context (e.g., TelegramDO for Telegram effects)
    if (isYieldedEffect(effect)) {
      return errorResult(
        `Yielded effect "${effect.type}" requires caller context. ` +
          "This effect should be yielded to the caller, not executed internally."
      );
    }

    // Use generated dispatcher for internal effects
    return await dispatchInternalEffect(effect, internalHandlers, env);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    return errorResult(`Effect execution failed: ${message}`);
  }
}
