/**
 * State effect handlers.
 *
 * Handles GetState and SetState effects for stateful graphs like DM.
 * State is stored in Durable Object storage keyed by state key.
 */

import type {
  GetStateEffect,
  SetStateEffect,
  EffectResult,
} from "tidepool-generated-ts";
import { successResult, errorResult } from "tidepool-generated-ts";

/**
 * Environment with Durable Object storage binding.
 * Currently empty - in production, this would include DO storage bindings.
 */
// eslint-disable-next-line @typescript-eslint/no-empty-object-type
export interface StateEnv {
  // For now, we'll use a simple in-memory store
  // In production, this would be Durable Object storage
}

/**
 * In-memory state store (for development/testing).
 * In production, this would use Durable Object storage.
 */
const stateStore: Map<string, unknown> = new Map();

/**
 * Handle GetState effect - read state by key.
 *
 * Returns the state value, or null if not found.
 */
export async function handleGetState(
  effect: GetStateEffect,
  _env: StateEnv
): Promise<EffectResult> {
  try {
    const value = stateStore.get(effect.eff_state_key);
    return successResult(value ?? null);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    return errorResult(`GetState failed: ${message}`);
  }
}

/**
 * Handle SetState effect - write state by key.
 *
 * Fire-and-forget semantics - returns immediately.
 */
export async function handleSetState(
  effect: SetStateEffect,
  _env: StateEnv
): Promise<EffectResult> {
  try {
    stateStore.set(effect.eff_state_key, effect.eff_state_value);
    return successResult(null);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);
    return errorResult(`SetState failed: ${message}`);
  }
}

/**
 * Clear all state (for testing).
 */
export function clearState(): void {
  stateStore.clear();
}

/**
 * Get current state snapshot (for debugging).
 */
export function getStateSnapshot(): Record<string, unknown> {
  const snapshot: Record<string, unknown> = {};
  stateStore.forEach((value, key) => {
    snapshot[key] = value;
  });
  return snapshot;
}
