/**
 * Random effect handlers.
 *
 * Handles RandomInt effect for dice rolling and other random needs.
 * Uses crypto.getRandomValues for better randomness.
 */

import type {
  RandomIntEffect,
  EffectResult,
} from "tidepool-generated-ts";
import { successResult, errorResult } from "tidepool-generated-ts";

/**
 * Handle RandomInt effect - get random integer in range [min, max].
 *
 * Uses crypto.getRandomValues for cryptographically secure randomness.
 */
export async function handleRandomInt(
  effect: RandomIntEffect
): Promise<EffectResult> {
  try {
    const { eff_min: min, eff_max: max } = effect;

    // Validate range
    if (min > max) {
      return errorResult(`RandomInt: min (${min}) > max (${max})`);
    }

    // Generate random integer in range [min, max]
    const range = max - min + 1;

    // Use crypto.getRandomValues if available, otherwise Math.random
    let randomValue: number;
    if (typeof crypto !== "undefined" && crypto.getRandomValues) {
      const array = new Uint32Array(1);
      crypto.getRandomValues(array);
      randomValue = min + (array[0] % range);
    } else {
      randomValue = min + Math.floor(Math.random() * range);
    }

    return successResult(randomValue);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    return errorResult(`RandomInt failed: ${message}`);
  }
}
