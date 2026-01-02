/**
 * Tests for LLM effect handler.
 */

import { describe, it, expect, vi } from "vitest";
import { handleLlmComplete, type LlmEnv } from "../llm.js";
import type { LlmCompleteEffect } from "tidepool-generated-ts";

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
