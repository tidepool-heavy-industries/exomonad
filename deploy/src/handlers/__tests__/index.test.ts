/**
 * Tests for effect handler registry.
 */

import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { executeEffect, type Env } from "../index.js";
import type { SerializableEffect, LogInfoEffect, LlmCompleteEffect } from "../../protocol.js";

// Store originals
const originalLog = console.log;
const originalError = console.error;

function createMockEnv(): Env {
  return {
    AI: {
      run: vi.fn().mockResolvedValue({ response: { output: "test" } }),
    } as unknown as Ai,
    HABITICA_USER_ID: "test-user",
    HABITICA_API_TOKEN: "test-token",
  };
}

describe("executeEffect", () => {
  beforeEach(() => {
    console.log = vi.fn();
    console.error = vi.fn();
  });

  afterEach(() => {
    console.log = originalLog;
    console.error = originalError;
  });

  it("dispatches LogInfo to log handler", async () => {
    const effect: LogInfoEffect = {
      type: "LogInfo",
      eff_message: "Test log",
    };
    const env = createMockEnv();

    const result = await executeEffect(effect, env);

    expect(result).toEqual({ type: "success", value: null });
    expect(console.log).toHaveBeenCalledWith("[Graph Log] Test log");
  });

  it("dispatches LogError to log handler", async () => {
    const effect: SerializableEffect = {
      type: "LogError",
      eff_message: "Test error",
    };
    const env = createMockEnv();

    const result = await executeEffect(effect, env);

    expect(result).toEqual({ type: "success", value: null });
    expect(console.error).toHaveBeenCalledWith("[Graph Error] Test error");
  });

  it("dispatches LlmComplete to llm handler", async () => {
    const effect: LlmCompleteEffect = {
      type: "LlmComplete",
      eff_node: "test",
      eff_system_prompt: "System",
      eff_user_content: "User",
      eff_schema: null,
    };
    const env = createMockEnv();

    const result = await executeEffect(effect, env);

    expect(result.type).toBe("success");
    expect(env.AI.run).toHaveBeenCalled();
  });

  it("returns error for unknown effect type", async () => {
    const effect = { type: "UnknownEffect" } as unknown as SerializableEffect;
    const env = createMockEnv();

    const result = await executeEffect(effect, env);

    expect(result).toEqual({
      type: "error",
      message: "Unknown effect type: UnknownEffect",
    });
  });

  it("catches and wraps handler exceptions", async () => {
    const effect: LlmCompleteEffect = {
      type: "LlmComplete",
      eff_node: "test",
      eff_system_prompt: "System",
      eff_user_content: "User",
      eff_schema: null,
    };
    const env: Env = {
      AI: {
        run: vi.fn().mockImplementation(() => {
          throw new Error("Unexpected crash");
        }),
      } as unknown as Ai,
      HABITICA_USER_ID: "test",
      HABITICA_API_TOKEN: "test",
    };

    const result = await executeEffect(effect, env);

    expect(result.type).toBe("error");
    expect((result as { message: string }).message).toContain("Unexpected crash");
  });
});
