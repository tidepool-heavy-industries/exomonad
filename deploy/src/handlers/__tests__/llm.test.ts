/**
 * Tests for LLM effect handlers.
 *
 * Tests mock env.AI.run which is used internally by runWithTools.
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
// handleLlmCall tests - Tool-aware LLM calls with runWithTools
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

    it("handles object response (structured output)", async () => {
      const env = createMockLlmCallEnv({
        response: { intents: [], unparsed: "hello" },
      });

      const result = await handleLlmCall(baseEffect, env);

      expect(result.type).toBe("success");
      const llmResult = (result as { type: "success"; value: LlmCallResult }).value;
      expect(llmResult.type).toBe("done");
      expect(llmResult.content).toEqual([{ type: "text", text: '{"intents":[],"unparsed":"hello"}' }]);
    });
  });

  describe("tool_use responses (needs_tools)", () => {
    // Effect with tools - triggers Phase 1 (tool decision pass)
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

      const result = await handleLlmCall(effectWithTools, env);

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

      const result = await handleLlmCall(effectWithTools, env);

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

    it("filters out malformed tool_calls with missing name", async () => {
      // CF AI sometimes returns [{}] with empty objects (llama-3.3 bug)
      const env = createMockLlmCallEnv({
        response: '{"intents": []}',
        tool_calls: [
          {} as { name: string; arguments: unknown }, // malformed - no name
          { name: "ask_user", arguments: { question: "test" } },
          { name: undefined as unknown as string, arguments: {} }, // malformed - undefined name
        ],
      });

      const result = await handleLlmCall(effectWithTools, env);

      expect(result.type).toBe("success");
      const llmResult = (result as { type: "success"; value: LlmCallResult }).value;
      expect(llmResult.type).toBe("needs_tools");
      if (llmResult.type === "needs_tools") {
        // Only the valid tool call should be included
        expect(llmResult.tool_calls).toHaveLength(1);
        expect(llmResult.tool_calls[0].name).toBe("ask_user");
      }
    });

    it("returns done when all tool_calls are malformed", async () => {
      const env = createMockLlmCallEnv({
        response: '{"intents": [], "unparsed": "test"}',
        tool_calls: [{}] as Array<{ name: string; arguments: unknown }>, // All malformed
      });

      const result = await handleLlmCall(effectWithTools, env);

      expect(result.type).toBe("success");
      const llmResult = (result as { type: "success"; value: LlmCallResult }).value;
      expect(llmResult.type).toBe("done");
      expect(llmResult.content).toHaveLength(1);
      if (llmResult.content[0].type === "text") {
        expect(llmResult.content[0].text).toContain("intents");
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
      // WASM sends OpenAI format: {type: "function", function: {...}}
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

      // Verify AI.run was called with tools in OpenAI format
      expect(env.AI.run).toHaveBeenCalled();
      const callArgs = (env.AI.run as ReturnType<typeof vi.fn>).mock.calls[0][1];
      expect(callArgs.tools).toBeDefined();
      // Tools should be in OpenAI format: {type: "function", function: {...}}
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

    it("uses specified model for all phases when tools and schema provided", async () => {
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
      // Simulate Phase 1 returning no tool calls, triggering Phase 2
      const env: LlmEnv = {
        AI: {
          run: vi.fn()
            .mockResolvedValueOnce({ response: "" })        // Phase 1: no tools called
            .mockResolvedValueOnce({ response: '{"result": "done"}' }),  // Phase 2: structured output
        } as unknown as Ai,
      };

      await handleLlmCall(effectWithModelAndTools, env);

      // Both phases should use the custom model
      expect(env.AI.run).toHaveBeenCalledTimes(2);
      expect(env.AI.run).toHaveBeenNthCalledWith(1, customModel, expect.any(Object));
      expect(env.AI.run).toHaveBeenNthCalledWith(2, customModel, expect.any(Object));
    });
  });

  describe("tool transition handling", () => {
    /**
     * Note: Tool transitions (ToolTransition ToolResultOutcome) are handled in WASM.
     * This test verifies that the LlmCall handler correctly returns "needs_tools"
     * when the LLM decides to invoke a tool, allowing the graph executor to handle
     * tool results and potential transitions.
     */

    it("returns needs_tools when LLM wants to use a tool", async () => {
      const effect: LlmCallEffect = {
        ...baseEffect,
        eff_tools: [
          {
            type: "function",
            function: {
              name: "transition_tool",
              description: "Tool that can trigger graph transitions",
              parameters: {
                type: "object",
                properties: { target: { type: "string" } },
                required: ["target"],
              },
            },
          },
        ],
      };

      const env: LlmEnv = {
        AI: {
          run: vi.fn().mockResolvedValue({
            response: "",
            tool_calls: [
              {
                name: "transition_tool",
                arguments: { target: "success" },
              },
            ],
          }),
        } as unknown as Ai,
      };

      const result = await handleLlmCall(effect, env);

      expect(result.type).toBe("success");
      if (result.type === "success" && result.value) {
        const llmResult = result.value as {
          type: "needs_tools" | "done";
          tool_calls?: Array<{ name: string }>;
        };
        expect(llmResult.type).toBe("needs_tools");
        expect(llmResult.tool_calls).toBeDefined();
        if (llmResult.tool_calls) {
          expect(llmResult.tool_calls[0].name).toBe("transition_tool");
        }
      }
    });

    it("completes LLM turn when tool succeeds without further tools", async () => {
      // When tool execution succeeds and returns ToolSuccess,
      // the graph executor processes the result and may route to next node.
      // LlmCall just returns the tool call to executor.
      const effect: LlmCallEffect = {
        ...baseEffect,
        eff_tools: [
          {
            type: "function",
            function: {
              name: "simple_tool",
              description: "A simple tool",
              parameters: {
                type: "object",
                properties: { query: { type: "string" } },
                required: ["query"],
              },
            },
          },
        ],
      };

      const env = createMockLlmCallEnv({
        response: "",
        tool_calls: [{ name: "simple_tool", arguments: { query: "test" } }],
      });

      const result = await handleLlmCall(effect, env);

      expect(result.type).toBe("success");
      if (result.type === "success" && result.value) {
        const llmResult = result.value as { type: "needs_tools" | "done" };
        // Should indicate tool use - executor handles tool results and transitions
        expect(llmResult.type).toBe("needs_tools");
      }
    });

    it("ends LLM turn when model doesn't call tools", async () => {
      const effect: LlmCallEffect = {
        ...baseEffect,
        eff_tools: [
          {
            type: "function",
            function: {
              name: "some_tool",
              description: "Available tool",
              parameters: { type: "object", properties: {}, required: [] },
            },
          },
        ],
      };

      // Model decides not to use tool
      const env = createMockLlmCallEnv({
        response: "No tools needed, here is the answer.",
      });

      const result = await handleLlmCall(effect, env);

      expect(result.type).toBe("success");
      if (result.type === "success" && result.value) {
        const llmResult = result.value as { type: "needs_tools" | "done" };
        expect(llmResult.type).toBe("done");
      }
    });
  });
});
