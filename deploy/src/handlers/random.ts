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
      // Rejection sampling to avoid modulo bias when mapping 32-bit values into [min, max]
      // Without this, ranges that don't evenly divide 2^32 would have slight bias
      const maxUint32 = 0xFFFFFFFF;
      const maxValid = Math.floor((maxUint32 + 1) / range) * range - 1;
      let candidate: number;
      do {
        crypto.getRandomValues(array);
        candidate = array[0];
      } while (candidate > maxValid);
      randomValue = min + (candidate % range);
    } else {
      randomValue = min + Math.floor(Math.random() * range);
    }

    return successResult(randomValue);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    return errorResult(`RandomInt failed: ${message}`);
  }
}
