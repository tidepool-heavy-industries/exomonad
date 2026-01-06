/**
 * LLM effect handlers.
 *
 * Handles LlmComplete and LlmCall effects by calling Cloudflare AI.
 * TypeScript manages the tool loop internally, returning final TurnResult.
 */

import type {
  LlmCompleteEffect,
  LlmCallEffect,
  EffectResult,
  WireContentBlock,
} from "tidepool-generated-ts";
import { successResult, errorResult } from "tidepool-generated-ts";

// ═══════════════════════════════════════════════════════════════════════════
// TURN RESULT TYPES (TypeScript manages tool loop, returns final result)
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Record of a single tool invocation during the turn.
 */
export interface ToolInvocation {
  /** Tool name that was called */
  name: string;
  /** Arguments passed to the tool */
  input: unknown;
  /** Result of the tool execution */
  result: unknown;
  /** Whether the tool execution failed */
  isError: boolean;
}

/**
 * Final result from an LLM turn after all tool calls are resolved.
 * TypeScript handles the tool loop; this is the final output.
 */
export interface TurnResult {
  /** Final content blocks from the LLM */
  content: WireContentBlock[];
  /** All tools that were invoked during this turn */
  toolsInvoked: ToolInvocation[];
  /** Extracted text content (narrative) */
  narrative: string | null;
  /** Extracted thinking content (if any) */
  thinking: string | null;
}

/**
 * Tools that require external handling (yielded to WASM/UI).
 * These tools cannot be executed internally by TypeScript.
 */
const UI_REQUIRING_TOOLS = new Set([
  "ask_user",
  "ask_user_input",
  "request_input",
  "request_text_input",
  "request_choice",
]);

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

// ═══════════════════════════════════════════════════════════════════════════
// TOOL EXECUTION
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Execute a tool call and return the result.
 *
 * For tools that require UI interaction (ask_user, etc.), returns an error
 * result indicating the tool cannot be executed server-side.
 *
 * For unknown tools, returns an error result to the model so it can try
 * a different approach.
 */
async function executeToolCall(
  toolName: string,
  _toolInput: unknown,
  _env: LlmEnv
): Promise<{ result: unknown; isError: boolean }> {
  // Tools requiring UI cannot be executed server-side
  if (UI_REQUIRING_TOOLS.has(toolName)) {
    return {
      result: `Tool '${toolName}' requires user interaction and cannot be executed server-side.`,
      isError: true,
    };
  }

  // Unknown tools return an error to the model
  // This allows the model to try a different approach
  return {
    result: `Unknown tool: ${toolName}. Available tools do not include this.`,
    isError: true,
  };
}

/**
 * Extract text narrative from content blocks.
 */
function extractNarrative(content: WireContentBlock[]): string | null {
  const textBlocks = content
    .filter((b): b is { type: "text"; text: string } => b.type === "text")
    .map((b) => b.text);

  return textBlocks.length > 0 ? textBlocks.join("\n") : null;
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
 * Handle LlmCall effect - tool-aware LLM API call with internal tool loop.
 *
 * TypeScript manages the tool loop internally:
 * 1. Call LLM with tools
 * 2. If model requests tools, execute them and continue
 * 3. Repeat until model finishes (no more tool calls)
 * 4. If schema provided, do a final structured output pass
 *
 * Returns TurnResult with all tool invocations tracked.
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
    // Build initial messages from effect
    const conversationMessages: CfAiMessage[] = effect.eff_messages.map((msg) => ({
      role: msg.role as "system" | "user" | "assistant",
      content: convertContentBlocks(msg.content),
    }));

    const tools = convertToolsForCfAi(effect.eff_tools ?? []);
    const hasTools = tools.length > 0;
    const hasSchema = effect.eff_schema !== null && effect.eff_schema !== undefined;

    // Use model from effect, or fall back to default
    const model = effect.eff_model ?? DEFAULT_MODEL;
    const maxTokens = 2048;

    // Track all tool invocations across the loop
    const allToolInvocations: ToolInvocation[] = [];

    // Maximum tool loop iterations to prevent infinite loops
    const MAX_TOOL_ITERATIONS = 10;
    let iterations = 0;

    // Final content from the last LLM response
    let finalContent: WireContentBlock[] = [];

    // ═══════════════════════════════════════════════════════════════════════
    // TOOL LOOP: Continue until model stops calling tools
    // ═══════════════════════════════════════════════════════════════════════
    if (hasTools) {
      while (iterations < MAX_TOOL_ITERATIONS) {
        iterations++;
        console.log(`[LlmCall] Tool loop iteration ${iterations}`);

        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        const toolResponse = (await (env.AI.run as any)(model, {
          messages: conversationMessages,
          tools,
          max_tokens: maxTokens,
        })) as CfAiResponse;

        console.log(`[LlmCall] Iteration ${iterations} response:`, JSON.stringify(toolResponse));

        // Filter malformed tool calls (CF AI sometimes returns [{}])
        const validToolCalls = (toolResponse.tool_calls ?? []).filter(
          (tc) => tc.name !== undefined && tc.name !== null
        );

        // Extract response text for content
        const responseText = toolResponse.response
          ? typeof toolResponse.response === "string"
            ? toolResponse.response
            : JSON.stringify(toolResponse.response)
          : null;

        // No tool calls - we're done with the tool loop
        if (validToolCalls.length === 0) {
          finalContent = responseText ? [{ type: "text" as const, text: responseText }] : [];
          break;
        }

        // Model wants to call tools - execute them
        // Add assistant message with tool calls to conversation
        const assistantContent: string | ContentItem[] = responseText
          ? [{ type: "text" as const, text: responseText }]
          : [];

        // Build tool call representations for conversation history
        const toolCallMessages: string[] = validToolCalls.map((tc, idx) =>
          `[Tool Call ${idx}: ${tc.name}(${JSON.stringify(tc.arguments)})]`
        );

        conversationMessages.push({
          role: "assistant",
          content: typeof assistantContent === "string"
            ? assistantContent + "\n" + toolCallMessages.join("\n")
            : toolCallMessages.join("\n"),
        });

        // Execute each tool and collect results
        const toolResults: string[] = [];

        for (let idx = 0; idx < validToolCalls.length; idx++) {
          const tc = validToolCalls[idx];
          const toolId = `tool_${iterations}_${idx}_${tc.name}`;

          console.log(`[LlmCall] Executing tool: ${tc.name}`);
          const { result, isError } = await executeToolCall(tc.name, tc.arguments, env);

          // Track the invocation
          allToolInvocations.push({
            name: tc.name,
            input: tc.arguments,
            result,
            isError,
          });

          // Format result for conversation
          const resultStr = typeof result === "string" ? result : JSON.stringify(result);
          toolResults.push(
            isError
              ? `[Tool Error ${toolId}]: ${resultStr}`
              : `[Tool Result ${toolId}]: ${resultStr}`
          );
        }

        // Add tool results as user message
        conversationMessages.push({
          role: "user",
          content: toolResults.join("\n"),
        });
      }

      // Check if we hit the iteration limit
      if (iterations >= MAX_TOOL_ITERATIONS) {
        console.warn(`[LlmCall] Hit max tool iterations (${MAX_TOOL_ITERATIONS})`);
      }
    }

    // ═══════════════════════════════════════════════════════════════════════
    // STRUCTURED OUTPUT: If schema provided, do final pass
    // ═══════════════════════════════════════════════════════════════════════
    if (hasSchema) {
      console.log("[LlmCall] Final structured output pass with schema");

      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const schemaResponse = (await (env.AI.run as any)(model, {
        messages: conversationMessages,
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

      console.log("[LlmCall] Schema response:", JSON.stringify(schemaResponse));

      const responseText = schemaResponse.response
        ? typeof schemaResponse.response === "string"
          ? schemaResponse.response
          : JSON.stringify(schemaResponse.response)
        : null;

      finalContent = responseText ? [{ type: "text" as const, text: responseText }] : [];
    } else if (!hasTools) {
      // No tools, no schema - simple text completion
      console.log("[LlmCall] Simple text completion (no tools, no schema)");

      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const response = (await (env.AI.run as any)(model, {
        messages: conversationMessages,
        max_tokens: maxTokens,
      })) as CfAiResponse;

      const responseText = response.response
        ? typeof response.response === "string"
          ? response.response
          : JSON.stringify(response.response)
        : null;

      finalContent = responseText ? [{ type: "text" as const, text: responseText }] : [];
    }

    // ═══════════════════════════════════════════════════════════════════════
    // BUILD TURN RESULT
    // ═══════════════════════════════════════════════════════════════════════
    const result: TurnResult = {
      content: finalContent,
      toolsInvoked: allToolInvocations,
      narrative: extractNarrative(finalContent),
      thinking: null, // CF AI doesn't expose thinking blocks
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
 * Message format for Cloudflare AI conversation.
 */
interface CfAiMessage {
  role: "system" | "user" | "assistant";
  content: string | ContentItem[];
}

/**
 * Content item for Cloudflare AI multimodal messages.
 * Supports text and images for vision-capable models like Llama 4.
 */
type ContentItem =
  | { type: "text"; text: string }
  | { type: "image"; image: string };

/**
 * Convert WireContentBlock array to Cloudflare AI message content format.
 * Supports multimodal (text + images) for vision models like Llama 4.
 *
 * Returns string for simple text-only messages, or array format for
 * messages with images or multiple blocks.
 */
function convertContentBlocks(blocks: WireContentBlock[]): string | ContentItem[] {
  // If only one text block, return as string (simpler format)
  if (blocks.length === 1 && blocks[0].type === "text") {
    return blocks[0].text;
  }

  // Multiple blocks or has images → use array format
  return blocks.map((block): ContentItem => {
    switch (block.type) {
      case "text":
        return { type: "text", text: block.text };
      case "image": {
        const source = block.source;
        if (source.type === "base64") {
          // Cloudflare AI expects base64 data URI format
          const dataUri = `data:${source.media_type};base64,${source.data}`;
          return { type: "image", image: dataUri };
        } else {
          // URL format (if CF AI supports it, otherwise error)
          console.warn("[LLM] URL images may not be supported by CF AI, prefer base64");
          return { type: "image", image: source.url };
        }
      }
      case "tool_use":
        // Tool calls are handled separately in the API, not in content
        return {
          type: "text",
          text: `[Tool Call: ${block.name}(${JSON.stringify(block.input)})]`,
        };
      case "tool_result":
        return {
          type: "text",
          text: `[Tool Result for ${block.tool_use_id}]: ${block.content}`,
        };
    }
  });
}
