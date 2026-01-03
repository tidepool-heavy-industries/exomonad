/**
 * LLM effect handlers.
 *
 * Handles LlmComplete and LlmCall effects by calling Cloudflare AI.
 * Uses direct env.AI.run for tool-aware calls (WASM handles the tool loop).
 */

import type {
  LlmCompleteEffect,
  LlmCallEffect,
  EffectResult,
  WireContentBlock,
  LlmCallResult,
} from "tidepool-generated-ts";
import { successResult, errorResult } from "tidepool-generated-ts";
import type { RoleScopedChatInput } from "@cloudflare/workers-types";

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
 * Uses direct env.AI.run for single-shot LLM calls.
 * WASM handles the tool loop via runToolLoop in Automat.Graph.hs.
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
    // Convert WireMessage format to Cloudflare AI RoleScopedChatInput format
    const messages: RoleScopedChatInput[] = effect.eff_messages.map((msg) => ({
      role: msg.role as "system" | "user" | "assistant",
      content: convertContentBlocks(msg.content),
    }));

    // Convert WASM tool definitions to CF AI format
    // WASM sends OpenAI format: {type: "function", function: {name, description, parameters}}
    // CF AI wants: {name, description, parameters}
    const tools = convertToolsForCfAi(effect.eff_tools ?? []);

    // Use llama-4-scout for tool calls - best function calling support on CF AI
    // llama-3.3 returns malformed tool_calls: [{}]
    // hermes-2-pro has max_tokens limit of 1024
    const model = "@cf/meta/llama-4-scout-17b-16e-instruct";

    console.log("[LlmCall] Calling AI.run with", tools.length, "tools");

    // Direct AI.run call - single invocation, no recursive loop
    // This is intentionally NOT using runWithTools which makes a "final response" call
    const response = (await env.AI.run(model, {
      messages,
      tools: tools.length > 0 ? tools : undefined,
      max_tokens: 2048,
    })) as CfAiResponse;

    // Debug: log raw response
    console.log("[LlmCall] AI.run response:", JSON.stringify(response));

    // Filter out malformed tool calls (CF AI sometimes returns [{}])
    const validToolCalls = (response.tool_calls ?? []).filter(
      (tc) => tc.name !== undefined && tc.name !== null
    );

    // Helper to convert response to string
    const responseText = response.response
      ? typeof response.response === "string"
        ? response.response
        : JSON.stringify(response.response)
      : null;

    if (validToolCalls.length > 0) {
      const result: LlmCallResult = {
        type: "needs_tools",
        tool_calls: validToolCalls.map((tc, idx) => ({
          // Generate deterministic ID since CF AI doesn't provide one
          id: `tool_${idx}_${tc.name}`,
          name: tc.name,
          input: tc.arguments,
        })),
        // Include any text response as content
        content: responseText ? [{ type: "text" as const, text: responseText }] : [],
      };
      return successResult(result);
    }

    // LLM finished - return final content
    const result: LlmCallResult = {
      type: "done",
      content: responseText ? [{ type: "text" as const, text: responseText }] : [],
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

/**
 * Tool definition format for Cloudflare AI.
 * CF AI expects OpenAI format: {type: "function", function: {...}}
 */
interface CfAiToolDef {
  type: "function";
  function: {
    name: string;
    description: string;
    parameters?: {
      type: "object" | string;
      properties: Record<string, { type: string; description?: string }>;
      required: string[];
    };
  };
}

/**
 * Convert WASM tool definitions to Cloudflare AI format.
 *
 * WASM already sends OpenAI format, but we normalize/validate here.
 * CF AI expects: {type: "function", function: {name, description, parameters}}
 */
function convertToolsForCfAi(wasmTools: unknown[]): CfAiToolDef[] {
  return wasmTools
    .map((tool) => {
      const t = tool as {
        type?: string;
        function?: { name: string; description: string; parameters?: unknown };
        name?: string;
        description?: string;
        parameters?: unknown;
      };

      // Handle OpenAI format: {type: "function", function: {...}}
      // This is what WASM sends, just pass through
      if (t.type === "function" && t.function) {
        return {
          type: "function" as const,
          function: {
            name: t.function.name,
            description: t.function.description,
            parameters: t.function.parameters as CfAiToolDef["function"]["parameters"],
          },
        };
      }

      // Handle flat format: wrap in OpenAI format
      if (t.name) {
        return {
          type: "function" as const,
          function: {
            name: t.name,
            description: t.description ?? "",
            parameters: t.parameters as CfAiToolDef["function"]["parameters"],
          },
        };
      }

      console.warn("[LlmCall] Unknown tool format:", tool);
      return null;
    })
    .filter((t): t is NonNullable<typeof t> => t !== null);
}

// ═══════════════════════════════════════════════════════════════════════════
// HELPER TYPES AND FUNCTIONS FOR LlmCall
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Cloudflare Workers AI response structure.
 *
 * When tools are provided and the model wants to call them,
 * `tool_calls` array will be populated. Otherwise, `response`
 * contains the text output.
 */
interface CfAiResponse {
  /** Text response from the model */
  response?: string;
  /** Tool calls the model wants to make */
  tool_calls?: CfToolCall[];
}

/**
 * Tool call in CF AI response.
 *
 * Note: CF AI doesn't provide an ID, we generate one.
 */
interface CfToolCall {
  name: string;
  arguments: unknown;
}

/**
 * Convert WireContentBlock array to format expected by Cloudflare AI.
 *
 * For CF AI, messages are typically just strings. We extract text content
 * and concatenate. Tool results are formatted as text for context.
 */
function convertContentBlocks(blocks: WireContentBlock[]): string {
  return blocks
    .map((block) => {
      switch (block.type) {
        case "text":
          return block.text;
        case "tool_use":
          // Include tool use context for multi-turn conversations
          return `[Tool Call: ${block.name}(${JSON.stringify(block.input)})]`;
        case "tool_result":
          return `[Tool Result for ${block.tool_use_id}]: ${block.content}`;
      }
    })
    .join("\n");
}
