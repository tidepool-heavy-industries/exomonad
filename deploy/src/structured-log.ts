/**
 * Structured logging for effect execution.
 *
 * Outputs JSON logs that can be shipped to Loki/Grafana via Cloudflare Logpush.
 * Each log entry includes session context, effect type, timing, and result status.
 */

import type { SerializableEffect, EffectResult } from "tidepool-generated-ts";

/**
 * Structured log entry for effect execution.
 * Maximalist design: log everything useful for iteration, sanitize secrets.
 */
export interface EffectLogEntry {
  /** ISO timestamp */
  ts: string;
  /** Log level */
  level: "info" | "error";
  /** Log message */
  msg: string;
  /** Session ID (from Durable Object routing) */
  session_id: string;
  /** Graph ID if available */
  graph_id?: string;

  // Effect execution
  /** Effect type being executed */
  effect_type: string;
  /** Execution latency in milliseconds */
  latency_ms: number;
  /** Result type: success or error */
  result_type: "success" | "error";
  /** Error message if result_type is error */
  error?: string;

  // Graph context (added by caller)
  /** Which node executed this effect */
  node_name?: string;
  /** Node type: llm, logic, entry, exit */
  node_type?: "llm" | "logic" | "entry" | "exit";
  /** Turn number in this session (0-indexed) */
  turn_number?: number;

  // Effect payload (no secrets - they live in env bindings!)
  /** Effect payload for debugging. Currently logged as-is (N=1 dev env).
   * TODO: Audit for PII/sensitive data before multi-user production.
   * Note: Secrets (API tokens) are never in payloads - good design! */
  effect_payload?: unknown;
  /** Effect result for debugging */
  effect_result?: unknown;

  // LLM-specific metrics
  /** LLM model used */
  llm_model?: string;
  /** Prompt tokens consumed */
  llm_prompt_tokens?: number;
  /** Completion tokens generated */
  llm_completion_tokens?: number;
  /** Whether cache was hit */
  llm_cache_hit?: boolean;

  // Habitica-specific
  /** Habitica operation type */
  habitica_operation?: string;
  /** Task/item ID if applicable */
  habitica_item_id?: string;
}

/**
 * Context for structured logging.
 */
export interface LogContext {
  sessionId: string;
  graphId?: string;
  // Graph execution context
  nodeName?: string;
  nodeType?: "llm" | "logic" | "entry" | "exit";
  turnNumber?: number;
}

/**
 * Log an effect execution with structured JSON.
 * Maximalist logging: captures all available metadata for iteration.
 */
export function logEffectExecution(
  ctx: LogContext,
  effect: SerializableEffect,
  result: EffectResult,
  latencyMs: number
): void {
  const entry: EffectLogEntry = {
    ts: new Date().toISOString(),
    level: result.type === "error" ? "error" : "info",
    msg: `effect:${effect.type}`,
    session_id: ctx.sessionId,
    effect_type: effect.type,
    latency_ms: Math.round(latencyMs),
    result_type: result.type,
    ...(result.type === "error" && { error: result.message }),
    ...(ctx.graphId && { graph_id: ctx.graphId }),

    // Graph context
    ...(ctx.nodeName && { node_name: ctx.nodeName }),
    ...(ctx.nodeType && { node_type: ctx.nodeType }),
    ...(ctx.turnNumber !== undefined && { turn_number: ctx.turnNumber }),

    // Effect payload (logged as-is for now - see type comment)
    effect_payload: effect,
    ...(result.type === "success" && { effect_result: result.value }),
  };

  // Extract effect-specific metadata
  enrichWithEffectMetadata(entry, effect, result);

  // Output as JSON for Cloudflare Logpush
  console.log(JSON.stringify(entry));
}

/**
 * Enrich log entry with effect-specific metadata.
 * Extracts LLM metrics, Habitica operation info, etc.
 */
function enrichWithEffectMetadata(
  entry: EffectLogEntry,
  effect: SerializableEffect,
  result: EffectResult
): void {
  // LLM-specific metadata
  if (effect.type === "LlmComplete") {
    const llmEffect = effect as { model?: string };
    if (llmEffect.model) {
      entry.llm_model = llmEffect.model;
    }

    // Extract token usage from result if available
    if (result.type === "success") {
      const usage = (result.value as any)?.usage;
      if (usage) {
        entry.llm_prompt_tokens = usage.input_tokens;
        entry.llm_completion_tokens = usage.output_tokens;
        // Check for cache creation/read tokens
        if (usage.cache_creation_input_tokens || usage.cache_read_input_tokens) {
          entry.llm_cache_hit = (usage.cache_read_input_tokens ?? 0) > 0;
        }
      }
    }
  }

  // Habitica-specific metadata
  if (effect.type === "Habitica") {
    const habiticaEffect = effect as any;
    // Extract operation type from the effect variant
    if (habiticaEffect.GetTasks) {
      entry.habitica_operation = "GetTasks";
    } else if (habiticaEffect.ScoreTask) {
      entry.habitica_operation = "ScoreTask";
      entry.habitica_item_id = habiticaEffect.ScoreTask.taskId;
    } else if (habiticaEffect.GetUser) {
      entry.habitica_operation = "GetUser";
    } else if (habiticaEffect.CreateTask) {
      entry.habitica_operation = "CreateTask";
    } else if (habiticaEffect.UpdateTask) {
      entry.habitica_operation = "UpdateTask";
      entry.habitica_item_id = habiticaEffect.UpdateTask.taskId;
    }
  }
}

/**
 * Log graph lifecycle events.
 */
export function logGraphEvent(
  ctx: LogContext,
  event: "start" | "complete" | "error" | "yield",
  details?: { error?: string; node?: string; effect_type?: string }
): void {
  const entry = {
    ts: new Date().toISOString(),
    level: event === "error" ? "error" : "info",
    msg: `graph:${event}`,
    session_id: ctx.sessionId,
    ...(ctx.graphId && { graph_id: ctx.graphId }),
    ...details,
  };

  console.log(JSON.stringify(entry));
}
