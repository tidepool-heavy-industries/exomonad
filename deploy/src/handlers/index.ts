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
import { handleLlmComplete, handleLlmCall, type LlmEnv } from "./llm.js";
import { handleHabitica, type HabiticaConfig } from "./habitica.js";
import { logEffectExecution, type LogContext } from "../structured-log.js";

// Re-export handlers for direct testing
export { handleLogInfo, handleLogError } from "./log.js";
export { handleLlmComplete, handleLlmCall } from "./llm.js";
export { handleHabitica } from "./habitica.js";

// Telegram handlers (used by TelegramDO, not executeEffect - they need chat context)
export {
  handleTelegramSend,
  handleTelegramReceive,
  handleTelegramTryReceive,
  handleTelegramAsk,
  type TelegramHandlerEnv,
  type TelegramHandlerContext,
  type ReceiveHandlerResult,
  type AskHandlerResult,
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
  LlmCall: (effect, env) => handleLlmCall(effect, env),
  Habitica: (effect, env) => {
    const config: HabiticaConfig = {
      userId: env.HABITICA_USER_ID,
      apiToken: env.HABITICA_API_TOKEN,
    };
    return handleHabitica(effect, config);
  },
};

// Re-export LogContext for callers
export type { LogContext } from "../structured-log.js";

/**
 * Execute an effect and return the result.
 *
 * Dispatches to the appropriate handler based on effect type.
 * Uses generated dispatcher for internal effects.
 * Wraps all handlers in try/catch for consistent error handling.
 *
 * @param effect - The effect to execute
 * @param env - Environment with required bindings
 * @param logCtx - Optional context for structured logging (session_id, graph_id)
 */
export async function executeEffect(
  effect: SerializableEffect,
  env: Env,
  logCtx?: LogContext
): Promise<EffectResult> {
  const startTime = performance.now();

  try {
    // Yielded effects require caller context (e.g., TelegramDO for Telegram effects)
    if (isYieldedEffect(effect)) {
      const result = errorResult(
        `Yielded effect "${effect.type}" requires caller context. ` +
          "This effect should be yielded to the caller, not executed internally."
      );
      if (logCtx) {
        logEffectExecution(logCtx, effect, result, performance.now() - startTime);
      }
      return result;
    }

    // Use generated dispatcher for internal effects
    const result = await dispatchInternalEffect(effect, internalHandlers, env);

    // Log with structured format if context provided
    if (logCtx) {
      logEffectExecution(logCtx, effect, result, performance.now() - startTime);
    }

    return result;
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    const result = errorResult(`Effect execution failed: ${message}`);

    if (logCtx) {
      logEffectExecution(logCtx, effect, result, performance.now() - startTime);
    }

    return result;
  }
}
