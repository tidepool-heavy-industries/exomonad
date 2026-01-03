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
// Default model for LLM calls
const DEFAULT_MODEL = "@cf/meta/llama-3.3-70b-instruct-fp8-fast";

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

    // Use model from effect, or fall back to default
    const model = effect.eff_model ?? DEFAULT_MODEL;

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const response = await (env.AI.run as any)(
      model,
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
 * Uses 2-phase approach when both tools and schema are provided:
 * 1. Phase 1: Call with tools only (let model decide to use tools)
 * 2. Phase 2: If no tools called, call with schema for structured output
 *
 * This prevents the conflict where "return JSON" instructions compete with tool use.
 *
 * Returns a discriminated result:
 * - `done`: LLM finished with final content
 * - `needs_tools`: LLM wants to call tools, caller should execute and continue
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
    const messages: RoleScopedChatInput[] = effect.eff_messages.map((msg) => ({
      role: msg.role as "system" | "user" | "assistant",
      content: convertContentBlocks(msg.content),
    }));

    const tools = convertToolsForCfAi(effect.eff_tools ?? []);
    const hasTools = tools.length > 0;
    const hasSchema = effect.eff_schema !== null && effect.eff_schema !== undefined;

    // Use model from effect, or fall back to default
    // Note: llama-4-scout outputs tool calls as text instead of populating tool_calls array
    // Note: hermes-2-pro has 1024 token context limit (too small for our prompts)
    // Note: llama-3.3 sometimes returns malformed [{}] in tool_calls (we filter these)
    const model = effect.eff_model ?? DEFAULT_MODEL;
    const maxTokens = 2048;

    // ═══════════════════════════════════════════════════════════════════════
    // PHASE 1: Tool decision pass (if tools provided)
    // ═══════════════════════════════════════════════════════════════════════
    if (hasTools) {
      console.log("[LlmCall] Phase 1: Tool decision pass with", tools.length, "tools");

      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const toolResponse = (await (env.AI.run as any)(model, {
        messages,
        tools,
        max_tokens: maxTokens,
      })) as CfAiResponse;

      console.log("[LlmCall] Phase 1 response:", JSON.stringify(toolResponse));

      // Filter malformed tool calls (CF AI sometimes returns [{}])
      const validToolCalls = (toolResponse.tool_calls ?? []).filter(
        (tc) => tc.name !== undefined && tc.name !== null
      );

      if (validToolCalls.length > 0) {
        // Model wants to use tools - return for WASM to handle
        const responseText = toolResponse.response
          ? typeof toolResponse.response === "string"
            ? toolResponse.response
            : JSON.stringify(toolResponse.response)
          : null;

        const result: LlmCallResult = {
          type: "needs_tools",
          tool_calls: validToolCalls.map((tc, idx) => ({
            id: `tool_${idx}_${tc.name}`,
            name: tc.name,
            input: tc.arguments,
          })),
          content: responseText ? [{ type: "text" as const, text: responseText }] : [],
        };
        return successResult(result);
      }

      // No tools called - fall through to Phase 2 if schema exists
      // If no schema, return the text response as-is
      if (!hasSchema) {
        const responseText = toolResponse.response
          ? typeof toolResponse.response === "string"
            ? toolResponse.response
            : JSON.stringify(toolResponse.response)
          : null;

        const result: LlmCallResult = {
          type: "done",
          content: responseText ? [{ type: "text" as const, text: responseText }] : [],
        };
        return successResult(result);
      }

      console.log("[LlmCall] No tools called, proceeding to Phase 2 for structured output");
    }

    // ═══════════════════════════════════════════════════════════════════════
    // PHASE 2: Structured output pass (if schema provided)
    // ═══════════════════════════════════════════════════════════════════════
    if (hasSchema) {
      console.log("[LlmCall] Phase 2: Structured output with schema");

      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const schemaResponse = (await (env.AI.run as any)(model, {
        messages,
        max_tokens: maxTokens,
        response_format: {
          type: "json_schema",
          json_schema: {
            name: "effect_output",
            strict: true,
            schema: effect.eff_schema,
          },
        },
      })) as CfAiResponse;

      console.log("[LlmCall] Phase 2 response:", JSON.stringify(schemaResponse));

      const responseText = schemaResponse.response
        ? typeof schemaResponse.response === "string"
          ? schemaResponse.response
          : JSON.stringify(schemaResponse.response)
        : null;

      const result: LlmCallResult = {
        type: "done",
        content: responseText ? [{ type: "text" as const, text: responseText }] : [],
      };
      return successResult(result);
    }

    // ═══════════════════════════════════════════════════════════════════════
    // No tools, no schema - simple text completion
    // ═══════════════════════════════════════════════════════════════════════
    console.log("[LlmCall] Simple text completion (no tools, no schema)");

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const response = (await (env.AI.run as any)(model, {
      messages,
      max_tokens: maxTokens,
    })) as CfAiResponse;

    const responseText = response.response
      ? typeof response.response === "string"
        ? response.response
        : JSON.stringify(response.response)
      : null;

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
