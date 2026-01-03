/**
 * Random effect handlers.
 *
 * Handles RandomInt effect for dice rolling and other random needs.
 * Uses Node.js crypto.randomInt for unbiased cryptographically secure randomness.
 */

import { randomInt } from "node:crypto";
import type {
  RandomIntEffect,
  EffectResult,
} from "tidepool-generated-ts";
import { successResult, errorResult } from "tidepool-generated-ts";

/**
 * Handle RandomInt effect - get random integer in range [min, max] (inclusive).
 *
 * Uses Node.js crypto.randomInt which provides:
 * - Cryptographically secure randomness
 * - Unbiased distribution (no modulo bias)
 * - Standard library implementation (not handrolled)
 *
 * Requires nodejs_compat flag in wrangler.toml for Cloudflare Workers.
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

    // Handle edge case: min === max
    if (min === max) {
      return successResult(min);
    }

    // crypto.randomInt(min, max) returns min <= result < max (exclusive max)
    // We want inclusive max, so add 1
    const randomValue = randomInt(min, max + 1);

    return successResult(randomValue);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    return errorResult(`RandomInt failed: ${message}`);
  }
}
