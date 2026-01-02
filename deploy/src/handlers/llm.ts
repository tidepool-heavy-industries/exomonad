/**
 * LLM completion effect handler.
 *
 * Handles LlmComplete effects by calling Cloudflare AI.
 */

import type { LlmCompleteEffect, EffectResult } from "tidepool-generated-ts";
import { successResult, errorResult } from "tidepool-generated-ts";

/**
 * Rate limit configuration.
 */
const RATE_LIMITS = {
  perMinute: 5,
  perHour: 15,
} as const;

/**
 * Environment with AI and KV bindings.
 * Uses Cloudflare's Ai type from @cloudflare/workers-types.
 */
export interface LlmEnv {
  AI: Ai;
  RATE_LIMIT_KV?: KVNamespace;
}

/**
 * Get the current minute timestamp (floored to minute boundary).
 */
function getCurrentMinuteTimestamp(): number {
  return Math.floor(Date.now() / 60000);
}

/**
 * Get the current hour timestamp (floored to hour boundary).
 */
function getCurrentHourTimestamp(): number {
  return Math.floor(Date.now() / 3600000);
}

/**
 * Check and update rate limits using sliding windows.
 * Returns null if within limits, or an error message if exceeded.
 */
async function checkRateLimit(kv: KVNamespace): Promise<string | null> {
  const minuteTs = getCurrentMinuteTimestamp();
  const hourTs = getCurrentHourTimestamp();

  const minuteKey = `llm-rate:min:${minuteTs}`;
  const hourKey = `llm-rate:hr:${hourTs}`;

  // Read current counts
  const [minuteCountStr, hourCountStr] = await Promise.all([
    kv.get(minuteKey),
    kv.get(hourKey),
  ]);

  const minuteCount = minuteCountStr ? parseInt(minuteCountStr, 10) : 0;
  const hourCount = hourCountStr ? parseInt(hourCountStr, 10) : 0;

  // Check limits
  if (minuteCount >= RATE_LIMITS.perMinute) {
    return `Rate limit exceeded: ${RATE_LIMITS.perMinute} calls per minute`;
  }

  if (hourCount >= RATE_LIMITS.perHour) {
    return `Rate limit exceeded: ${RATE_LIMITS.perHour} calls per hour`;
  }

  // Increment counts with appropriate TTLs
  await Promise.all([
    kv.put(minuteKey, String(minuteCount + 1), { expirationTtl: 120 }), // 2 min TTL
    kv.put(hourKey, String(hourCount + 1), { expirationTtl: 7200 }), // 2 hour TTL
  ]);

  return null;
}

/**
 * Handle LlmComplete effect by calling Cloudflare AI.
 *
 * Supports both free-form text and JSON schema-constrained output.
 * Returns parsed output in the result.
 */
export async function handleLlmComplete(
  effect: LlmCompleteEffect,
  env: LlmEnv
): Promise<EffectResult> {
  // Check rate limits if KV is available
  if (env.RATE_LIMIT_KV) {
    const rateLimitError = await checkRateLimit(env.RATE_LIMIT_KV);
    if (rateLimitError) {
      return errorResult(rateLimitError);
    }
  }

  try {
    const messages: Array<{ role: "system" | "user" | "assistant"; content: string }> = [
      { role: "system", content: effect.eff_system_prompt },
      { role: "user", content: effect.eff_user_content },
    ];

    // Build request options
    const options: Record<string, unknown> = {
      messages,
      max_tokens: 2048,
    };

    // Use JSON schema mode if schema provided
    if (effect.eff_schema) {
      options.response_format = {
        type: "json_schema",
        json_schema: {
          name: "effect_output",
          strict: true,
          schema: effect.eff_schema,
        },
      };
    }

    const response = await env.AI.run(
      "@cf/meta/llama-3.3-70b-instruct-fp8-fast",
      options
    ) as { response?: string | object };

    // Handle response
    const output = parseAiResponse(response.response);

    return successResult(output);
  } catch (err) {
    // Handle specific error types
    const message = err instanceof Error ? err.message : String(err);

    // Check for rate limiting
    if (message.includes("rate limit") || message.includes("429")) {
      return errorResult(`LLM rate limited: ${message}`);
    }

    // Check for timeout
    if (message.includes("timeout") || message.includes("timed out")) {
      return errorResult(`LLM timeout: ${message}`);
    }

    return errorResult(`LLM error: ${message}`);
  }
}

/**
 * Parse AI response, handling both object and string responses.
 * Strips markdown code blocks if present.
 */
function parseAiResponse(response: unknown): unknown {
  // Already an object (JSON schema mode returned parsed)
  if (typeof response === "object" && response !== null) {
    return response;
  }

  // Parse JSON from string response
  if (typeof response === "string") {
    let jsonStr = response.trim();

    // Strip markdown code blocks if present
    if (jsonStr.startsWith("```json")) {
      jsonStr = jsonStr.slice(7);
    } else if (jsonStr.startsWith("```")) {
      jsonStr = jsonStr.slice(3);
    }
    if (jsonStr.endsWith("```")) {
      jsonStr = jsonStr.slice(0, -3);
    }
    jsonStr = jsonStr.trim();

    try {
      return JSON.parse(jsonStr);
    } catch {
      // Return raw text if JSON parse fails
      return { text: response };
    }
  }

  // Fallback for unexpected response types
  return {};
}
