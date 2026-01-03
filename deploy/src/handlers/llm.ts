/**
 * LLM effect handlers.
 *
 * Handles LlmComplete and LlmCall effects by calling Cloudflare AI.
 */

import type {
  LlmCompleteEffect,
  LlmCallEffect,
  EffectResult,
  WireContentBlock,
  WireToolCall,
  LlmCallResult,
} from "tidepool-generated-ts";
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
 * Check and update rate limits using fixed time windows (minute/hour buckets).
 * Returns null if within limits, or an error message if exceeded.
 *
 * Note: This is best-effort rate limiting. Due to KV's eventual consistency,
 * concurrent requests may briefly exceed limits. Acceptable for non-critical
 * use cases like alert throttling.
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

/**
 * Handle LlmCall effect - tool-aware LLM API call.
 *
 * Returns a discriminated result:
 * - `done`: LLM finished with final content
 * - `needs_tools`: LLM wants to call tools, caller should execute and continue
 *
 * The caller (graph handler) is responsible for the tool loop:
 * 1. Execute tools with full effect access (can yield TelegramAsk, etc.)
 * 2. Continue LLM conversation with tool results
 * 3. Repeat until `done`
 */
export async function handleLlmCall(
  effect: LlmCallEffect,
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
    // Convert WireMessage format to Cloudflare AI format
    const messages = effect.eff_messages.map((msg) => ({
      role: msg.role as "system" | "user" | "assistant",
      content: convertContentBlocks(msg.content),
    }));

    // Build request options
    const options: Record<string, unknown> = {
      messages,
      max_tokens: 4096,
    };

    // Add tools if provided
    if (effect.eff_tools && effect.eff_tools.length > 0) {
      options.tools = effect.eff_tools;
    }

    // Add response schema if provided (for structured output on final response)
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

    const response = (await env.AI.run(
      "@cf/meta/llama-3.3-70b-instruct-fp8-fast",
      options
    )) as AiResponse;

    // Check if LLM wants to use tools
    if (response.stop_reason === "tool_use") {
      const result: LlmCallResult = {
        type: "needs_tools",
        tool_calls: extractToolCalls(response.content),
        content: extractTextContent(response.content),
      };
      return successResult(result);
    }

    // LLM finished - return final content
    const result: LlmCallResult = {
      type: "done",
      content: extractAllContent(response.content),
    };
    return successResult(result);
  } catch (err) {
    const message = err instanceof Error ? err.message : String(err);

    if (message.includes("rate limit") || message.includes("429")) {
      return errorResult(`LLM rate limited: ${message}`);
    }

    if (message.includes("timeout") || message.includes("timed out")) {
      return errorResult(`LLM timeout: ${message}`);
    }

    return errorResult(`LLM error: ${message}`);
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// HELPER TYPES AND FUNCTIONS FOR LlmCall
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Cloudflare AI response structure (simplified).
 */
interface AiResponse {
  stop_reason?: "end_turn" | "tool_use" | string;
  content?: AiContentBlock[] | string;
  response?: string;
}

/**
 * Content block in AI response.
 */
type AiContentBlock =
  | { type: "text"; text: string }
  | { type: "tool_use"; id: string; name: string; input: unknown };

/**
 * Convert WireContentBlock array to format expected by Cloudflare AI.
 */
function convertContentBlocks(
  blocks: WireContentBlock[]
): string | AiContentBlock[] {
  // If single text block, return as string for simpler API
  if (blocks.length === 1 && blocks[0].type === "text") {
    return blocks[0].text;
  }

  // Convert to Cloudflare AI format
  return blocks.map((block) => {
    switch (block.type) {
      case "text":
        return { type: "text" as const, text: block.text };
      case "tool_use":
        return {
          type: "tool_use" as const,
          id: block.id,
          name: block.name,
          input: block.input,
        };
      case "tool_result":
        // Tool results are sent as text with special formatting
        return {
          type: "text" as const,
          text: `[Tool Result for ${block.tool_use_id}]: ${block.content}`,
        };
    }
  });
}

/**
 * Extract tool calls from AI response content.
 */
function extractToolCalls(
  content: AiContentBlock[] | string | undefined
): WireToolCall[] {
  if (!content || typeof content === "string") {
    return [];
  }

  return content
    .filter((block): block is Extract<AiContentBlock, { type: "tool_use" }> =>
      block.type === "tool_use"
    )
    .map((block) => ({
      id: block.id,
      name: block.name,
      input: block.input,
    }));
}

/**
 * Extract text content blocks from AI response (excluding tool_use).
 */
function extractTextContent(
  content: AiContentBlock[] | string | undefined
): WireContentBlock[] {
  if (!content) {
    return [];
  }

  if (typeof content === "string") {
    return [{ type: "text", text: content }];
  }

  return content
    .filter((block): block is Extract<AiContentBlock, { type: "text" }> =>
      block.type === "text"
    )
    .map((block) => ({ type: "text", text: block.text }));
}

/**
 * Extract all content blocks from AI response.
 */
function extractAllContent(
  content: AiContentBlock[] | string | undefined
): WireContentBlock[] {
  if (!content) {
    return [];
  }

  if (typeof content === "string") {
    return [{ type: "text", text: content }];
  }

  return content.map((block) => {
    if (block.type === "text") {
      return { type: "text" as const, text: block.text };
    } else {
      return {
        type: "tool_use" as const,
        id: block.id,
        name: block.name,
        input: block.input,
      };
    }
  });
}
