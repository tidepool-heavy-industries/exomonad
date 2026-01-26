/**
 * Time effect handlers.
 *
 * Handles GetTime effect for current UTC time.
 * Returns ISO8601 formatted timestamp.
 */

import type {
  GetTimeEffect,
  EffectResult,
} from "exomonad-generated-ts";
import { successResult, errorResult } from "exomonad-generated-ts";

/**
 * Handle GetTime effect - get current UTC time as ISO8601 string.
 *
 * Returns format like "2024-01-15T10:30:00.000Z".
 */
export async function handleGetTime(
  _effect: GetTimeEffect
): Promise<EffectResult> {
  try {
    const now = new Date();
    const isoString = now.toISOString();
    return successResult(isoString);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    return errorResult(`GetTime failed: ${message}`);
  }
}
