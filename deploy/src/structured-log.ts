/**
 * Structured logging for effect execution.
 *
 * Outputs JSON logs that can be shipped to Loki/Grafana via Cloudflare Logpush.
 * Each log entry includes session context, effect type, timing, and result status.
 */

import type { SerializableEffect, EffectResult } from "tidepool-generated-ts";

/**
 * Structured log entry for effect execution.
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
  /** Effect type being executed */
  effect_type: string;
  /** Execution latency in milliseconds */
  latency_ms: number;
  /** Result type: success or error */
  result_type: "success" | "error";
  /** Error message if result_type is error */
  error?: string;
  /** Graph ID if available */
  graph_id?: string;
}

/**
 * Context for structured logging.
 */
export interface LogContext {
  sessionId: string;
  graphId?: string;
}

/**
 * Log an effect execution with structured JSON.
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
  };

  // Output as JSON for Cloudflare Logpush
  console.log(JSON.stringify(entry));
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
