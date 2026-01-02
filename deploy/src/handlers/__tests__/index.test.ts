/**
 * Tests for effect handler registry.
 */

import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { executeEffect, type Env } from "../index.js";
import type { SerializableEffect, LogInfoEffect, LlmCompleteEffect } from "tidepool-generated-ts";

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
      message: "Unknown internal effect type: UnknownEffect",
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

  describe("structured logging", () => {
    it("creates log entries with correct format when logCtx is provided", async () => {
      const effect: LogInfoEffect = {
        type: "LogInfo",
        eff_message: "Test log",
      };
      const env = createMockEnv();
      const logCtx = {
        sessionId: "test-session-123",
        graphId: "test-graph-456",
      };

      await executeEffect(effect, env, logCtx);

      // console.log should be called twice: once for the graph log, once for structured log
      expect(console.log).toHaveBeenCalledTimes(2);
      
      // Get the structured log entry (second call)
      const structuredLogCall = (console.log as ReturnType<typeof vi.fn>).mock.calls[1][0];
      const logEntry = JSON.parse(structuredLogCall);

      // Verify log entry has correct structure
      expect(logEntry).toMatchObject({
        level: "info",
        msg: "effect:LogInfo",
        session_id: "test-session-123",
        graph_id: "test-graph-456",
        effect_type: "LogInfo",
        result_type: "success",
      });
      expect(logEntry.ts).toBeDefined();
      expect(logEntry.latency_ms).toBeGreaterThanOrEqual(0);
    });

    it("includes all expected fields in log entries", async () => {
      const effect: LogInfoEffect = {
        type: "LogInfo",
        eff_message: "Test log",
      };
      const env = createMockEnv();
      const logCtx = {
        sessionId: "session-abc",
      };

      await executeEffect(effect, env, logCtx);

      const structuredLogCall = (console.log as ReturnType<typeof vi.fn>).mock.calls[1][0];
      const logEntry = JSON.parse(structuredLogCall);

      // Verify all required fields are present
      expect(logEntry).toHaveProperty("ts");
      expect(logEntry).toHaveProperty("level");
      expect(logEntry).toHaveProperty("msg");
      expect(logEntry).toHaveProperty("session_id", "session-abc");
      expect(logEntry).toHaveProperty("effect_type", "LogInfo");
      expect(logEntry).toHaveProperty("latency_ms");
      expect(logEntry).toHaveProperty("result_type", "success");
      
      // graph_id is optional, should not be present when not provided
      expect(logEntry).not.toHaveProperty("graph_id");
    });

    it("produces logs with error field populated for error cases", async () => {
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
            throw new Error("LLM API failure");
          }),
        } as unknown as Ai,
        HABITICA_USER_ID: "test",
        HABITICA_API_TOKEN: "test",
      };
      const logCtx = {
        sessionId: "session-error-test",
        graphId: "graph-error-test",
      };

      await executeEffect(effect, env, logCtx);

      // Get the structured log entry
      const structuredLogCall = (console.log as ReturnType<typeof vi.fn>).mock.calls[0][0];
      const logEntry = JSON.parse(structuredLogCall);

      // Verify error fields
      expect(logEntry).toMatchObject({
        level: "error",
        msg: "effect:LlmComplete",
        session_id: "session-error-test",
        graph_id: "graph-error-test",
        effect_type: "LlmComplete",
        result_type: "error",
      });
      expect(logEntry.error).toContain("LLM API failure");
      expect(logEntry.latency_ms).toBeGreaterThanOrEqual(0);
    });

    it("does not produce structured logs when logCtx is not provided", async () => {
      const effect: LogInfoEffect = {
        type: "LogInfo",
        eff_message: "Test log",
      };
      const env = createMockEnv();

      // Clear any previous calls
      (console.log as ReturnType<typeof vi.fn>).mockClear();

      await executeEffect(effect, env);

      // Should only have the graph log, not the structured log
      expect(console.log).toHaveBeenCalledTimes(1);
      expect(console.log).toHaveBeenCalledWith("[Graph Log] Test log");
    });

    it("logs yielded effects with error result", async () => {
      const effect: SerializableEffect = {
        type: "TelegramConfirm",
        eff_chat_id: "123",
        eff_text: "test",
        eff_buttons: [],
      } as SerializableEffect;
      const env = createMockEnv();
      const logCtx = {
        sessionId: "session-yielded",
      };

      await executeEffect(effect, env, logCtx);

      // Get the structured log entry
      const structuredLogCall = (console.log as ReturnType<typeof vi.fn>).mock.calls[0][0];
      const logEntry = JSON.parse(structuredLogCall);

      // Verify yielded effect error is logged
      expect(logEntry).toMatchObject({
        level: "error",
        msg: "effect:TelegramConfirm",
        session_id: "session-yielded",
        effect_type: "TelegramConfirm",
        result_type: "error",
      });
      expect(logEntry.error).toContain("Yielded effect");
      expect(logEntry.error).toContain("requires caller context");
    });
  });
});
