/**
 * HTTP fetch effect handler.
 *
 * Handles HttpFetch effects by calling fetch() with timeout.
 */

import type { HttpFetchEffect, EffectResult } from "../protocol.js";
import { successResult, errorResult } from "../protocol.js";

/** Default timeout in milliseconds (25s, leaving buffer for CF 30s limit) */
const DEFAULT_TIMEOUT_MS = 25000;

/**
 * Result from HTTP fetch.
 */
export interface HttpResult {
  status: number;
  body: unknown;
  headers?: Record<string, string>;
}

/**
 * Handle HttpFetch effect by calling fetch() with timeout.
 *
 * Parses response as JSON if Content-Type is application/json,
 * otherwise returns text.
 */
export async function handleHttpFetch(
  effect: HttpFetchEffect,
  _env?: unknown,
  timeoutMs: number = DEFAULT_TIMEOUT_MS
): Promise<EffectResult> {
  try {
    // Create abort controller for timeout
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), timeoutMs);

    try {
      const resp = await fetch(effect.eff_url, {
        method: effect.eff_method,
        signal: controller.signal,
      });

      const contentType = resp.headers.get("content-type") ?? "";
      let body: unknown;

      if (contentType.includes("application/json")) {
        body = await resp.json();
      } else {
        body = await resp.text();
      }

      const result: HttpResult = {
        status: resp.status,
        body,
      };

      return successResult(result);
    } finally {
      clearTimeout(timeoutId);
    }
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);

    // Check for abort/timeout
    if (err instanceof Error && err.name === "AbortError") {
      return errorResult(`HTTP timeout after ${timeoutMs}ms: ${effect.eff_url}`);
    }

    // Network errors
    if (message.includes("fetch failed") || message.includes("network")) {
      return errorResult(`HTTP network error: ${message}`);
    }

    return errorResult(`HTTP error: ${message}`);
  }
}
