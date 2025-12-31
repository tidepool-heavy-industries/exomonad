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
} from "../protocol.js";
import { errorResult } from "../protocol.js";

import { handleLogInfo, handleLogError } from "./log.js";
import { handleLlmComplete, type LlmEnv } from "./llm.js";
import { handleHabitica, type HabiticaConfig } from "./habitica.js";

// Re-export handlers for direct testing
export { handleLogInfo, handleLogError } from "./log.js";
export { handleLlmComplete } from "./llm.js";
export { handleHabitica } from "./habitica.js";

/**
 * Environment interface with all required bindings.
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

      default:
        return errorResult(`Unknown effect type: ${(effect as { type: string }).type}`);
    }
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    return errorResult(`Effect execution failed: ${message}`);
  }
}
