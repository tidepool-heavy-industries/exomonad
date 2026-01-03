/**
 * Log effect handlers.
 *
 * Handles LogInfo and LogError effects from WASM by writing structured JSON.
 * Custom fields from the Haskell side are spread into the log entry.
 */

import type { LogInfoEffect, LogErrorEffect, EffectResult } from "tidepool-generated-ts";
import { successResult } from "tidepool-generated-ts";

/**
 * Handle LogInfo effect - logs message at info level with structured fields.
 *
 * If eff_fields is provided, they are spread into the JSON log entry for
 * queryability in Loki/Grafana.
 */
export async function handleLogInfo(effect: LogInfoEffect): Promise<EffectResult> {
  const fields = effect.eff_fields ?? {};
  // Spread fields first so level/msg can't be overridden by custom fields
  const entry = {
    ...fields,
    level: "info",
    msg: effect.eff_message,
  };
  console.log(JSON.stringify(entry));
  return successResult(null);
}

/**
 * Handle LogError effect - logs message at error level with structured fields.
 *
 * If eff_fields is provided, they are spread into the JSON log entry for
 * queryability in Loki/Grafana.
 */
export async function handleLogError(effect: LogErrorEffect): Promise<EffectResult> {
  const fields = effect.eff_fields ?? {};
  // Spread fields first so level/msg can't be overridden by custom fields
  const entry = {
    ...fields,
    level: "error",
    msg: effect.eff_message,
  };
  console.error(JSON.stringify(entry));
  return successResult(null);
}
