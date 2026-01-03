/**
 * Tests for LLM effect handlers.
 */

import { describe, it, expect, vi } from "vitest";
import { handleLlmComplete, handleLlmCall, type LlmEnv } from "../llm.js";
import type { LlmCompleteEffect, LlmCallEffect, LlmCallResult } from "tidepool-generated-ts";

// Mock AI binding
function createMockEnv(response: unknown): LlmEnv {
  return {
    AI: {
      run: vi.fn().mockResolvedValue({ response }),
    } as unknown as Ai,
  };
}

describe("handleLlmComplete", () => {
  const baseEffect: LlmCompleteEffect = {
    type: "LlmComplete",
    eff_node: "test-node",
    eff_system_prompt: "You are a helpful assistant",
    eff_user_content: "Hello",
    eff_schema: null,
  };

  it("handles object response (JSON schema mode)", async () => {
    const mockOutput = { intent: "greeting" };
    const env = createMockEnv(mockOutput);

    const result = await handleLlmComplete(baseEffect, env);

    expect(result).toEqual({
      type: "success",
      value: mockOutput,
    });
  });

  it("handles string JSON response", async () => {
    const env = createMockEnv('{"intent": "greeting"}');

    const result = await handleLlmComplete(baseEffect, env);

    expect(result).toEqual({
      type: "success",
      value: { intent: "greeting" },
    });
  });

  it("strips markdown code blocks from response", async () => {
    const env = createMockEnv('```json\n{"intent": "greeting"}\n```');

    const result = await handleLlmComplete(baseEffect, env);

    expect(result).toEqual({
      type: "success",
      value: { intent: "greeting" },
    });
  });

  it("handles plain code block prefix", async () => {
    const env = createMockEnv('```\n{"data": 123}\n```');

    const result = await handleLlmComplete(baseEffect, env);

    expect(result).toEqual({
      type: "success",
      value: { data: 123 },
    });
  });

  it("returns raw text when JSON parse fails", async () => {
    const env = createMockEnv("This is not JSON");

    const result = await handleLlmComplete(baseEffect, env);

    expect(result).toEqual({
      type: "success",
      value: { text: "This is not JSON" },
    });
  });

  it("handles empty object for undefined response", async () => {
    const env = createMockEnv(undefined);

    const result = await handleLlmComplete(baseEffect, env);

    expect(result).toEqual({
      type: "success",
      value: {},
    });
  });

  it("passes schema to AI when provided", async () => {
    const effectWithSchema: LlmCompleteEffect = {
      ...baseEffect,
      eff_schema: {
        type: "object",
        properties: { intent: { type: "string" } },
      },
    };
    const env = createMockEnv({ intent: "test" });

    await handleLlmComplete(effectWithSchema, env);

    expect(env.AI.run).toHaveBeenCalledWith(
      "@cf/meta/llama-3.3-70b-instruct-fp8-fast",
      expect.objectContaining({
        response_format: {
          type: "json_schema",
          json_schema: {
            name: "effect_output",
            strict: true,
            schema: effectWithSchema.eff_schema,
          },
        },
      })
    );
  });

  it("returns error result on rate limit", async () => {
    const env: LlmEnv = {
      AI: {
        run: vi.fn().mockRejectedValue(new Error("rate limit exceeded")),
      } as unknown as Ai,
    };

    const result = await handleLlmComplete(baseEffect, env);

    expect(result).toEqual({
      type: "error",
      message: "LLM rate limited: rate limit exceeded",
    });
  });

  it("returns error result on timeout", async () => {
    const env: LlmEnv = {
      AI: {
        run: vi.fn().mockRejectedValue(new Error("request timed out")),
      } as unknown as Ai,
    };

    const result = await handleLlmComplete(baseEffect, env);

    expect(result).toEqual({
      type: "error",
      message: "LLM timeout: request timed out",
    });
  });

  it("returns generic error for other failures", async () => {
    const env: LlmEnv = {
      AI: {
        run: vi.fn().mockRejectedValue(new Error("Unknown error")),
      } as unknown as Ai,
    };

    const result = await handleLlmComplete(baseEffect, env);

    expect(result).toEqual({
      type: "error",
      message: "LLM error: Unknown error",
    });
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// handleLlmCall tests - Tool-aware LLM calls
// ═══════════════════════════════════════════════════════════════════════════

describe("handleLlmCall", () => {
  const baseEffect: LlmCallEffect = {
    type: "LlmCall",
    eff_node: "test-node",
    eff_messages: [
      { role: "system", content: [{ type: "text", text: "You are a helpful assistant" }] },
      { role: "user", content: [{ type: "text", text: "Hello" }] },
    ],
    eff_schema: null,
    eff_tools: [],
  };

  // Helper to create mock env with CF AI response format
  function createMockLlmCallEnv(response: {
    response?: string;
    tool_calls?: Array<{ name: string; arguments: unknown }>;
  }): LlmEnv {
    return {
      AI: {
        run: vi.fn().mockResolvedValue(response),
      } as unknown as Ai,
    };
  }

  describe("end_turn responses (done)", () => {
    it("returns done with text content", async () => {
      // CF AI returns text in the `response` field
      const env = createMockLlmCallEnv({
        response: "Hello! How can I help you?",
      });

      const result = await handleLlmCall(baseEffect, env);

      expect(result.type).toBe("success");
      const llmResult = (result as { type: "success"; value: LlmCallResult }).value;
      expect(llmResult.type).toBe("done");
      expect(llmResult.content).toEqual([{ type: "text", text: "Hello! How can I help you?" }]);
    });

    it("handles empty response", async () => {
      const env = createMockLlmCallEnv({
        response: undefined,
      });

      const result = await handleLlmCall(baseEffect, env);

      expect(result.type).toBe("success");
      const llmResult = (result as { type: "success"; value: LlmCallResult }).value;
      expect(llmResult.type).toBe("done");
      expect(llmResult.content).toEqual([]);
    });
  });

  describe("tool_use responses (needs_tools)", () => {
    it("returns needs_tools with tool calls", async () => {
      // CF AI returns tool_calls array at top level
      const env = createMockLlmCallEnv({
        response: "I need to ask you something",
        tool_calls: [
          {
            name: "ask_user",
            arguments: { question: "What would you like to do?" },
          },
        ],
      });

      const result = await handleLlmCall(baseEffect, env);

      expect(result.type).toBe("success");
      const llmResult = (result as { type: "success"; value: LlmCallResult }).value;
      expect(llmResult.type).toBe("needs_tools");
      if (llmResult.type === "needs_tools") {
        expect(llmResult.tool_calls).toEqual([
          {
            id: "tool_0_ask_user",  // Generated ID since CF AI doesn't provide one
            name: "ask_user",
            input: { question: "What would you like to do?" },
          },
        ]);
        expect(llmResult.content).toEqual([{ type: "text", text: "I need to ask you something" }]);
      }
    });

    it("handles multiple tool calls", async () => {
      const env = createMockLlmCallEnv({
        tool_calls: [
          {
            name: "ask_user",
            arguments: { question: "First question?" },
          },
          {
            name: "ask_user",
            arguments: { question: "Second question?" },
          },
        ],
      });

      const result = await handleLlmCall(baseEffect, env);

      expect(result.type).toBe("success");
      const llmResult = (result as { type: "success"; value: LlmCallResult }).value;
      expect(llmResult.type).toBe("needs_tools");
      if (llmResult.type === "needs_tools") {
        expect(llmResult.tool_calls).toHaveLength(2);
        // Generated IDs include index and tool name
        expect(llmResult.tool_calls[0].id).toBe("tool_0_ask_user");
        expect(llmResult.tool_calls[1].id).toBe("tool_1_ask_user");
      }
    });
  });

  describe("message conversion", () => {
    it("converts single text block to string", async () => {
      const effect: LlmCallEffect = {
        ...baseEffect,
        eff_messages: [
          { role: "user", content: [{ type: "text", text: "Hello" }] },
        ],
      };
      const env = createMockLlmCallEnv({ response: "" });

      await handleLlmCall(effect, env);

      expect(env.AI.run).toHaveBeenCalledWith(
        "@cf/meta/llama-3.3-70b-instruct-fp8-fast",
        expect.objectContaining({
          messages: [{ role: "user", content: "Hello" }],
        })
      );
    });

    it("passes tools to the model when provided", async () => {
      const tools = [
        { name: "ask_user", description: "Ask user a question" },
      ];
      const effect: LlmCallEffect = {
        ...baseEffect,
        eff_tools: tools,
      };
      const env = createMockLlmCallEnv({ response: "" });

      await handleLlmCall(effect, env);

      // Always uses llama model, passes tools through
      expect(env.AI.run).toHaveBeenCalledWith(
        "@cf/meta/llama-3.3-70b-instruct-fp8-fast",
        expect.objectContaining({
          tools,
        })
      );
    });
  });

  describe("error handling", () => {
    it("returns error on rate limit", async () => {
      const env: LlmEnv = {
        AI: {
          run: vi.fn().mockRejectedValue(new Error("rate limit exceeded")),
        } as unknown as Ai,
      };

      const result = await handleLlmCall(baseEffect, env);

      expect(result).toEqual({
        type: "error",
        message: "LLM rate limited: rate limit exceeded",
      });
    });

    it("returns error on timeout", async () => {
      const env: LlmEnv = {
        AI: {
          run: vi.fn().mockRejectedValue(new Error("request timed out")),
        } as unknown as Ai,
      };

      const result = await handleLlmCall(baseEffect, env);

      expect(result).toEqual({
        type: "error",
        message: "LLM timeout: request timed out",
      });
    });

    it("returns generic error for other failures", async () => {
      const env: LlmEnv = {
        AI: {
          run: vi.fn().mockRejectedValue(new Error("Unknown error")),
        } as unknown as Ai,
      };

      const result = await handleLlmCall(baseEffect, env);

      expect(result).toEqual({
        type: "error",
        message: "LLM error: Unknown error",
      });
    });
  });
});
