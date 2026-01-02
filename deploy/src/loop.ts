/**
 * Graph execution loop helper.
 *
 * Extracts the common effect interpretation loop used by both HTTP and WebSocket modes.
 * Uses an async generator pattern to yield events back to the caller.
 */

import type {
  SerializableEffect,
  EffectResult,
  StepOutput,
} from "tidepool-generated-ts";
import { isYieldedEffect } from "tidepool-generated-ts";

// =============================================================================
// Loop Event Types
// =============================================================================

/**
 * Events yielded by the graph loop.
 */
export type LoopEvent =
  | { type: "progress"; effect: SerializableEffect }
  | { type: "yield"; effect: SerializableEffect }
  | { type: "done"; result: unknown }
  | { type: "error"; error: string };

/**
 * Step function type - steps the graph with an effect result.
 */
export type StepFn = (result: EffectResult) => Promise<StepOutput>;

/**
 * Internal effect dispatcher - executes non-yielded effects.
 */
export type DispatchFn = (effect: SerializableEffect) => Promise<EffectResult>;

// =============================================================================
// Effect Loop Generator
// =============================================================================

/**
 * Run the graph effect loop as an async generator.
 *
 * Yields events for each effect encountered:
 * - "progress": Effect is being executed internally
 * - "yield": Effect should be handled by caller (Telegram effects)
 * - "done": Graph completed successfully
 * - "error": Graph failed
 *
 * After yielding a "yield" event, the caller should:
 * 1. Handle the effect externally
 * 2. Call the returned `resume` function with the result
 *
 * @param step - Function to step the graph with an effect result
 * @param initialOutput - Initial step output from initialize() or step()
 * @param dispatch - Function to dispatch internal effects (Log, LLM, Habitica)
 */
export async function* runEffectLoop(
  step: StepFn,
  initialOutput: StepOutput,
  dispatch: DispatchFn
): AsyncGenerator<LoopEvent, void, EffectResult | undefined> {
  let output = initialOutput;

  while (!output.done && output.effect) {
    const effect = output.effect;

    // Check if this is a yielded effect (e.g., Telegram)
    if (isYieldedEffect(effect)) {
      // Yield to caller - they'll provide the result via generator.next(result)
      const result = yield { type: "yield", effect };

      if (!result) {
        // If no result provided, caller should have handled externally
        // Return and let caller restart the loop when they have a result
        return;
      }

      output = await step(result);
      continue;
    }

    // Internal effect - dispatch and continue
    yield { type: "progress", effect };
    const result = await dispatch(effect);
    output = await step(result);
  }

  // Check completion status
  if (output.graphState.phase.type === "failed") {
    yield { type: "error", error: output.graphState.phase.error };
  } else {
    yield { type: "done", result: output.stepResult };
  }
}

// =============================================================================
// Simpler Non-Generator Loop
// =============================================================================

/**
 * Result from running the loop.
 */
export type LoopResult =
  | { type: "yield"; effect: SerializableEffect }
  | { type: "done"; result: unknown }
  | { type: "error"; error: string };

/**
 * Progress callback - called for each internal effect before execution.
 */
export type OnProgressFn = (effect: SerializableEffect) => void;

/**
 * Options for runLoop.
 */
export interface RunLoopOptions {
  /** Callback for progress updates before each internal effect */
  onProgress?: OnProgressFn;
  /** If true, yield on internal effect errors instead of continuing */
  yieldOnError?: boolean;
}

/**
 * Extended result that includes error-yield case.
 */
export type LoopResultWithErrorYield =
  | LoopResult
  | { type: "error_yield"; effect: SerializableEffect; error: string };

/**
 * Run the graph effect loop until completion or yield.
 *
 * Runs until:
 * - A yielded effect is encountered (returns "yield" for caller to handle)
 * - An internal effect errors AND yieldOnError is true (returns "error_yield")
 * - Graph completes (returns "done")
 * - Graph fails (returns "error")
 *
 * @param step - Function to step the graph with an effect result
 * @param initialOutput - Initial step output from initialize() or step()
 * @param dispatch - Function to dispatch internal effects (Log, LLM, Habitica)
 * @param options - Optional configuration
 */
export async function runLoop(
  step: StepFn,
  initialOutput: StepOutput,
  dispatch: DispatchFn,
  options?: RunLoopOptions
): Promise<LoopResultWithErrorYield> {
  const { onProgress, yieldOnError } = options ?? {};
  let output = initialOutput;

  while (!output.done && output.effect) {
    const effect = output.effect;

    // Check if this is a yielded effect (e.g., Telegram)
    if (isYieldedEffect(effect)) {
      return { type: "yield", effect };
    }

    // Internal effect - dispatch and continue
    onProgress?.(effect);
    const result = await dispatch(effect);

    // Check for error with yieldOnError option
    if (yieldOnError && result.type === "error") {
      return { type: "error_yield", effect, error: result.message };
    }

    output = await step(result);
  }

  // Check completion status
  if (output.graphState.phase.type === "failed") {
    return { type: "error", error: output.graphState.phase.error };
  }

  return { type: "done", result: output.stepResult };
}
