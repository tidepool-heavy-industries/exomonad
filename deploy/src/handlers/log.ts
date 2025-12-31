/**
 * Log effect handlers.
 *
 * Handles LogInfo and LogError effects from WASM by writing to console.
 */

import type { LogInfoEffect, LogErrorEffect, EffectResult } from "../protocol.js";
import { successResult } from "../protocol.js";

/**
 * Handle LogInfo effect - logs message at info level.
 */
export async function handleLogInfo(effect: LogInfoEffect): Promise<EffectResult> {
  console.log(`[Graph Log] ${effect.eff_message}`);
  return successResult(null);
}

/**
 * Handle LogError effect - logs message at error level.
 */
export async function handleLogError(effect: LogErrorEffect): Promise<EffectResult> {
  console.error(`[Graph Error] ${effect.eff_message}`);
  return successResult(null);
}
