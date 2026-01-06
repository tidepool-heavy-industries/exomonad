/**
 * Tests for LLM effect handlers.
 *
 * Tests mock env.AI.run which is used internally.
 * handleLlmCall now returns TurnResult (TypeScript manages tool loop).
 */

import { describe, it, expect, vi } from "vitest";
import { handleLlmComplete, handleLlmCall, type LlmEnv, type TurnResult } from "../llm.js";
import type { LlmCompleteEffect, LlmCallEffect } from "tidepool-generated-ts";

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

  describe("model selection", () => {
    it("uses default model when eff_model is not provided", async () => {
      const env = createMockEnv({ intent: "test" });

      await handleLlmComplete(baseEffect, env);

      expect(env.AI.run).toHaveBeenCalledWith(
        "@cf/meta/llama-3.3-70b-instruct-fp8-fast",
        expect.any(Object)
      );
    });

    it("uses specified model when eff_model is provided", async () => {
      const effectWithModel: LlmCompleteEffect = {
        ...baseEffect,
        eff_model: "@cf/meta/llama-3.2-1b-instruct",
      };
      const env = createMockEnv({ intent: "test" });

      await handleLlmComplete(effectWithModel, env);

      expect(env.AI.run).toHaveBeenCalledWith(
        "@cf/meta/llama-3.2-1b-instruct",
        expect.any(Object)
      );
    });
  });
});

// ═══════════════════════════════════════════════════════════════════════════
// handleLlmCall tests - Returns TurnResult (TypeScript manages tool loop)
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
    response?: string | object;
    tool_calls?: Array<{ name: string; arguments: unknown }>;
  }): LlmEnv {
    return {
      AI: {
        run: vi.fn().mockResolvedValue(response),
      } as unknown as Ai,
    };
  }

  describe("TurnResult format", () => {
    it("returns TurnResult with text content", async () => {
      const env = createMockLlmCallEnv({
        response: "Hello! How can I help you?",
      });

      const result = await handleLlmCall(baseEffect, env);

      expect(result.type).toBe("success");
      const turnResult = (result as { type: "success"; value: TurnResult }).value;
      expect(turnResult.content).toEqual([{ type: "text", text: "Hello! How can I help you?" }]);
      expect(turnResult.toolsInvoked).toEqual([]);
      expect(turnResult.narrative).toBe("Hello! How can I help you?");
      expect(turnResult.thinking).toBeNull();
    });

    it("handles empty response", async () => {
      const env = createMockLlmCallEnv({
        response: undefined,
      });

      const result = await handleLlmCall(baseEffect, env);

      expect(result.type).toBe("success");
      const turnResult = (result as { type: "success"; value: TurnResult }).value;
      expect(turnResult.content).toEqual([]);
      expect(turnResult.toolsInvoked).toEqual([]);
      expect(turnResult.narrative).toBeNull();
    });

    it("handles object response (structured output)", async () => {
      const env = createMockLlmCallEnv({
        response: { intents: [], unparsed: "hello" },
      });

      const result = await handleLlmCall(baseEffect, env);

      expect(result.type).toBe("success");
      const turnResult = (result as { type: "success"; value: TurnResult }).value;
      expect(turnResult.content).toEqual([{ type: "text", text: '{"intents":[],"unparsed":"hello"}' }]);
      expect(turnResult.narrative).toBe('{"intents":[],"unparsed":"hello"}');
    });
  });

  describe("tool loop execution", () => {
    // Effect with tools - now TypeScript handles the loop
    const effectWithTools: LlmCallEffect = {
      ...baseEffect,
      eff_tools: [
        {
          type: "function",
          function: {
            name: "ask_user",
            description: "Ask user a question",
            parameters: { type: "object", properties: {}, required: [] },
          },
        },
      ],
    };

    it("executes tool and tracks invocation (UI-requiring tool returns error)", async () => {
      // Model calls tool, then gets error response and finishes
      const env: LlmEnv = {
        AI: {
          run: vi.fn()
            .mockResolvedValueOnce({
              response: "I need to ask you something",
              tool_calls: [{ name: "ask_user", arguments: { question: "What?" } }],
            })
            .mockResolvedValueOnce({
              response: "Understood, I cannot ask the user directly.",
            }),
        } as unknown as Ai,
      };

      const result = await handleLlmCall(effectWithTools, env);

      expect(result.type).toBe("success");
      const turnResult = (result as { type: "success"; value: TurnResult }).value;

      // Tool was invoked but returned error
      expect(turnResult.toolsInvoked).toHaveLength(1);
      expect(turnResult.toolsInvoked[0].name).toBe("ask_user");
      expect(turnResult.toolsInvoked[0].isError).toBe(true);
      expect(turnResult.toolsInvoked[0].result).toContain("requires user interaction");

      // Final content from the second LLM call
      expect(turnResult.content).toEqual([
        { type: "text", text: "Understood, I cannot ask the user directly." },
      ]);
    });

    it("handles unknown tools with error result", async () => {
      const effectWithUnknownTool: LlmCallEffect = {
        ...baseEffect,
        eff_tools: [
          {
            type: "function",
            function: {
              name: "unknown_tool",
              description: "An unknown tool",
              parameters: { type: "object", properties: {}, required: [] },
            },
          },
        ],
      };

      const env: LlmEnv = {
        AI: {
          run: vi.fn()
            .mockResolvedValueOnce({
              tool_calls: [{ name: "unknown_tool", arguments: {} }],
            })
            .mockResolvedValueOnce({
              response: "I see that tool is not available.",
            }),
        } as unknown as Ai,
      };

      const result = await handleLlmCall(effectWithUnknownTool, env);

      expect(result.type).toBe("success");
      const turnResult = (result as { type: "success"; value: TurnResult }).value;

      expect(turnResult.toolsInvoked).toHaveLength(1);
      expect(turnResult.toolsInvoked[0].name).toBe("unknown_tool");
      expect(turnResult.toolsInvoked[0].isError).toBe(true);
      expect(turnResult.toolsInvoked[0].result).toContain("Unknown tool");
    });

    it("filters out malformed tool_calls with missing name", async () => {
      const env: LlmEnv = {
        AI: {
          run: vi.fn()
            .mockResolvedValueOnce({
              response: '{"intents": []}',
              tool_calls: [
                {} as { name: string; arguments: unknown }, // malformed
                { name: "ask_user", arguments: { question: "test" } },
              ],
            })
            .mockResolvedValueOnce({
              response: "Final response",
            }),
        } as unknown as Ai,
      };

      const result = await handleLlmCall(effectWithTools, env);

      expect(result.type).toBe("success");
      const turnResult = (result as { type: "success"; value: TurnResult }).value;

      // Only valid tool call should be executed
      expect(turnResult.toolsInvoked).toHaveLength(1);
      expect(turnResult.toolsInvoked[0].name).toBe("ask_user");
    });

    it("returns immediately when all tool_calls are malformed", async () => {
      const env = createMockLlmCallEnv({
        response: '{"intents": [], "unparsed": "test"}',
        tool_calls: [{}] as Array<{ name: string; arguments: unknown }>,
      });

      const result = await handleLlmCall(effectWithTools, env);

      expect(result.type).toBe("success");
      const turnResult = (result as { type: "success"; value: TurnResult }).value;

      // No tools executed
      expect(turnResult.toolsInvoked).toEqual([]);
      // Content from the response
      expect(turnResult.content).toHaveLength(1);
      if (turnResult.content[0].type === "text") {
        expect(turnResult.content[0].text).toContain("intents");
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

    it("passes tools in OpenAI format to CF AI", async () => {
      const tools = [
        {
          type: "function",
          function: {
            name: "ask_user",
            description: "Ask user a question",
            parameters: {
              type: "object",
              properties: { question: { type: "string" } },
              required: ["question"],
            },
          },
        },
      ];
      const effect: LlmCallEffect = {
        ...baseEffect,
        eff_tools: tools,
      };
      const env = createMockLlmCallEnv({ response: "" });

      await handleLlmCall(effect, env);

      expect(env.AI.run).toHaveBeenCalled();
      const callArgs = (env.AI.run as ReturnType<typeof vi.fn>).mock.calls[0][1];
      expect(callArgs.tools).toBeDefined();
      expect(callArgs.tools[0].type).toBe("function");
      expect(callArgs.tools[0].function.name).toBe("ask_user");
      expect(callArgs.tools[0].function.description).toBe("Ask user a question");
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

      expect(result.type).toBe("error");
      if (result.type === "error") {
        expect(result.message).toContain("LLM rate limited");
        expect(result.message).toContain("rate limit exceeded");
      }
    });

    it("returns error on timeout", async () => {
      const env: LlmEnv = {
        AI: {
          run: vi.fn().mockRejectedValue(new Error("request timed out")),
        } as unknown as Ai,
      };

      const result = await handleLlmCall(baseEffect, env);

      expect(result.type).toBe("error");
      if (result.type === "error") {
        expect(result.message).toContain("LLM timeout");
        expect(result.message).toContain("timed out");
      }
    });

    it("returns generic error for other failures", async () => {
      const env: LlmEnv = {
        AI: {
          run: vi.fn().mockRejectedValue(new Error("Unknown error")),
        } as unknown as Ai,
      };

      const result = await handleLlmCall(baseEffect, env);

      expect(result.type).toBe("error");
      if (result.type === "error") {
        expect(result.message).toContain("LLM error");
        expect(result.message).toContain("Unknown error");
      }
    });
  });

  describe("model selection", () => {
    it("uses default model when eff_model is not provided", async () => {
      const env = createMockLlmCallEnv({ response: "test" });

      await handleLlmCall(baseEffect, env);

      expect(env.AI.run).toHaveBeenCalledWith(
        "@cf/meta/llama-3.3-70b-instruct-fp8-fast",
        expect.any(Object)
      );
    });

    it("uses specified model when eff_model is provided", async () => {
      const effectWithModel: LlmCallEffect = {
        ...baseEffect,
        eff_model: "@cf/meta/llama-3.2-1b-instruct",
      };
      const env = createMockLlmCallEnv({ response: "test" });

      await handleLlmCall(effectWithModel, env);

      expect(env.AI.run).toHaveBeenCalledWith(
        "@cf/meta/llama-3.2-1b-instruct",
        expect.any(Object)
      );
    });

    it("uses specified model for tool loop and schema phases", async () => {
      const customModel = "@cf/meta/llama-3.2-1b-instruct";
      const effectWithModelAndTools: LlmCallEffect = {
        ...baseEffect,
        eff_model: customModel,
        eff_schema: { type: "object", properties: { result: { type: "string" } } },
        eff_tools: [
          {
            type: "function",
            function: {
              name: "test_tool",
              description: "A test tool",
              parameters: { type: "object", properties: {}, required: [] },
            },
          },
        ],
      };
      // Tool loop iteration 1: no tools called, then schema pass
      const env: LlmEnv = {
        AI: {
          run: vi.fn()
            .mockResolvedValueOnce({ response: "" })  // Tool loop: no tools called
            .mockResolvedValueOnce({ response: '{"result": "done"}' }),  // Schema pass
        } as unknown as Ai,
      };

      await handleLlmCall(effectWithModelAndTools, env);

      expect(env.AI.run).toHaveBeenCalledTimes(2);
      expect(env.AI.run).toHaveBeenNthCalledWith(1, customModel, expect.any(Object));
      expect(env.AI.run).toHaveBeenNthCalledWith(2, customModel, expect.any(Object));
    });
  });
});
