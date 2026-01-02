/**
 * Tests for structured logging module.
 *
 * Verifies JSON output format, timestamp format, error handling,
 * and optional field inclusion for Cloudflare Logpush compatibility.
 */

import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import {
  logEffectExecution,
  logGraphEvent,
  type EffectLogEntry,
  type LogContext,
} from "../structured-log.js";
import type { SerializableEffect, EffectResult } from "tidepool-generated-ts";

describe("logEffectExecution", () => {
  const originalLog = console.log;
  let logSpy: ReturnType<typeof vi.fn>;

  beforeEach(() => {
    logSpy = vi.fn();
    console.log = logSpy;
  });

  afterEach(() => {
    console.log = originalLog;
  });

  it("produces valid JSON output with all required fields", () => {
    const ctx: LogContext = {
      sessionId: "test-session-123",
    };

    const effect: SerializableEffect = {
      type: "LogInfo",
      eff_message: "test message",
    };

    const result: EffectResult = {
      type: "success",
      value: null,
    };

    logEffectExecution(ctx, effect, result, 42.7);

    expect(logSpy).toHaveBeenCalledOnce();
    const loggedJson = logSpy.mock.calls[0][0];
    const parsed: EffectLogEntry = JSON.parse(loggedJson);

    // Verify all required fields are present
    expect(parsed).toHaveProperty("ts");
    expect(parsed).toHaveProperty("level");
    expect(parsed).toHaveProperty("msg");
    expect(parsed).toHaveProperty("session_id");
    expect(parsed).toHaveProperty("effect_type");
    expect(parsed).toHaveProperty("latency_ms");
    expect(parsed).toHaveProperty("result_type");

    // Verify values
    expect(parsed.session_id).toBe("test-session-123");
    expect(parsed.effect_type).toBe("LogInfo");
    expect(parsed.result_type).toBe("success");
    expect(parsed.level).toBe("info");
    expect(parsed.msg).toBe("effect:LogInfo");
    expect(parsed.latency_ms).toBe(43); // Rounded from 42.7
  });

  it("produces timestamps in ISO format", () => {
    const ctx: LogContext = {
      sessionId: "test-session",
    };

    const effect: SerializableEffect = {
      type: "LogInfo",
      eff_message: "test",
    };

    const result: EffectResult = {
      type: "success",
      value: null,
    };

    logEffectExecution(ctx, effect, result, 10);

    const loggedJson = logSpy.mock.calls[0][0];
    const parsed: EffectLogEntry = JSON.parse(loggedJson);

    // Verify ISO 8601 format (e.g., "2024-01-15T10:30:00.000Z")
    expect(parsed.ts).toMatch(/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/);

    // Verify it's a valid date
    const date = new Date(parsed.ts);
    expect(date.toISOString()).toBe(parsed.ts);
  });

  it("correctly populates error field for error results", () => {
    const ctx: LogContext = {
      sessionId: "test-session",
    };

    const effect: SerializableEffect = {
      type: "LogError",
      eff_message: "error message",
    };

    const result: EffectResult = {
      type: "error",
      message: "Something went wrong",
    };

    logEffectExecution(ctx, effect, result, 100);

    const loggedJson = logSpy.mock.calls[0][0];
    const parsed: EffectLogEntry = JSON.parse(loggedJson);

    // Verify error-specific fields
    expect(parsed.result_type).toBe("error");
    expect(parsed.level).toBe("error");
    expect(parsed.error).toBe("Something went wrong");
  });

  it("does not include error field for success results", () => {
    const ctx: LogContext = {
      sessionId: "test-session",
    };

    const effect: SerializableEffect = {
      type: "LogInfo",
      eff_message: "success message",
    };

    const result: EffectResult = {
      type: "success",
      value: { data: "test" },
    };

    logEffectExecution(ctx, effect, result, 50);

    const loggedJson = logSpy.mock.calls[0][0];
    const parsed: EffectLogEntry = JSON.parse(loggedJson);

    // Verify error field is not present
    expect(parsed).not.toHaveProperty("error");
    expect(parsed.result_type).toBe("success");
    expect(parsed.level).toBe("info");
  });

  it("includes graph_id when provided in context", () => {
    const ctx: LogContext = {
      sessionId: "test-session",
      graphId: "graph-abc-123",
    };

    const effect: SerializableEffect = {
      type: "LogInfo",
      eff_message: "test",
    };

    const result: EffectResult = {
      type: "success",
      value: null,
    };

    logEffectExecution(ctx, effect, result, 25);

    const loggedJson = logSpy.mock.calls[0][0];
    const parsed: EffectLogEntry = JSON.parse(loggedJson);

    expect(parsed.graph_id).toBe("graph-abc-123");
  });

  it("omits graph_id when not provided in context", () => {
    const ctx: LogContext = {
      sessionId: "test-session",
    };

    const effect: SerializableEffect = {
      type: "LogInfo",
      eff_message: "test",
    };

    const result: EffectResult = {
      type: "success",
      value: null,
    };

    logEffectExecution(ctx, effect, result, 25);

    const loggedJson = logSpy.mock.calls[0][0];
    const parsed: EffectLogEntry = JSON.parse(loggedJson);

    expect(parsed).not.toHaveProperty("graph_id");
  });

  it("rounds latency to nearest millisecond", () => {
    const ctx: LogContext = {
      sessionId: "test-session",
    };

    const effect: SerializableEffect = {
      type: "LogInfo",
      eff_message: "test",
    };

    const result: EffectResult = {
      type: "success",
      value: null,
    };

    // Test various rounding scenarios
    const testCases = [
      { input: 42.4, expected: 42 },
      { input: 42.5, expected: 43 },
      { input: 42.9, expected: 43 },
      { input: 0.1, expected: 0 },
      { input: 999.99, expected: 1000 },
    ];

    testCases.forEach(({ input, expected }) => {
      logSpy.mockClear();
      logEffectExecution(ctx, effect, result, input);

      const loggedJson = logSpy.mock.calls[0][0];
      const parsed: EffectLogEntry = JSON.parse(loggedJson);

      expect(parsed.latency_ms).toBe(expected);
    });
  });

  it("handles different effect types correctly", () => {
    const ctx: LogContext = {
      sessionId: "test-session",
    };

    const result: EffectResult = {
      type: "success",
      value: null,
    };

    const effectTypes: SerializableEffect[] = [
      { type: "LogInfo", eff_message: "info" },
      { type: "LogError", eff_message: "error" },
      {
        type: "LlmComplete",
        eff_node: "node1",
        eff_system_prompt: "system",
        eff_user_content: "user",
        eff_schema: null,
      },
      {
        type: "Habitica",
        eff_hab_op: "GetUser",
        eff_hab_payload: {},
      },
    ];

    effectTypes.forEach((effect) => {
      logSpy.mockClear();
      logEffectExecution(ctx, effect, result, 10);

      const loggedJson = logSpy.mock.calls[0][0];
      const parsed: EffectLogEntry = JSON.parse(loggedJson);

      expect(parsed.effect_type).toBe(effect.type);
      expect(parsed.msg).toBe(`effect:${effect.type}`);
    });
  });
});

describe("logGraphEvent", () => {
  const originalLog = console.log;
  let logSpy: ReturnType<typeof vi.fn>;

  beforeEach(() => {
    logSpy = vi.fn();
    console.log = logSpy;
  });

  afterEach(() => {
    console.log = originalLog;
  });

  it("produces valid JSON output for start event", () => {
    const ctx: LogContext = {
      sessionId: "test-session",
    };

    logGraphEvent(ctx, "start");

    expect(logSpy).toHaveBeenCalledOnce();
    const loggedJson = logSpy.mock.calls[0][0];
    const parsed = JSON.parse(loggedJson);

    expect(parsed).toHaveProperty("ts");
    expect(parsed).toHaveProperty("level");
    expect(parsed).toHaveProperty("msg");
    expect(parsed).toHaveProperty("session_id");
    expect(parsed.level).toBe("info");
    expect(parsed.msg).toBe("graph:start");
    expect(parsed.session_id).toBe("test-session");
  });

  it("produces valid JSON output for complete event", () => {
    const ctx: LogContext = {
      sessionId: "test-session",
    };

    logGraphEvent(ctx, "complete");

    const loggedJson = logSpy.mock.calls[0][0];
    const parsed = JSON.parse(loggedJson);

    expect(parsed.level).toBe("info");
    expect(parsed.msg).toBe("graph:complete");
  });

  it("produces valid JSON output for yield event", () => {
    const ctx: LogContext = {
      sessionId: "test-session",
    };

    logGraphEvent(ctx, "yield", { node: "node1", effect_type: "LlmComplete" });

    const loggedJson = logSpy.mock.calls[0][0];
    const parsed = JSON.parse(loggedJson);

    expect(parsed.level).toBe("info");
    expect(parsed.msg).toBe("graph:yield");
    expect(parsed.node).toBe("node1");
    expect(parsed.effect_type).toBe("LlmComplete");
  });

  it("produces valid JSON output for error event", () => {
    const ctx: LogContext = {
      sessionId: "test-session",
    };

    logGraphEvent(ctx, "error", { error: "Graph execution failed" });

    const loggedJson = logSpy.mock.calls[0][0];
    const parsed = JSON.parse(loggedJson);

    expect(parsed.level).toBe("error");
    expect(parsed.msg).toBe("graph:error");
    expect(parsed.error).toBe("Graph execution failed");
  });

  it("includes graph_id when provided in context", () => {
    const ctx: LogContext = {
      sessionId: "test-session",
      graphId: "graph-xyz",
    };

    logGraphEvent(ctx, "start");

    const loggedJson = logSpy.mock.calls[0][0];
    const parsed = JSON.parse(loggedJson);

    expect(parsed.graph_id).toBe("graph-xyz");
  });

  it("omits graph_id when not provided in context", () => {
    const ctx: LogContext = {
      sessionId: "test-session",
    };

    logGraphEvent(ctx, "start");

    const loggedJson = logSpy.mock.calls[0][0];
    const parsed = JSON.parse(loggedJson);

    expect(parsed).not.toHaveProperty("graph_id");
  });

  it("includes optional details when provided", () => {
    const ctx: LogContext = {
      sessionId: "test-session",
    };

    logGraphEvent(ctx, "yield", {
      node: "node2",
      effect_type: "HttpFetch",
    });

    const loggedJson = logSpy.mock.calls[0][0];
    const parsed = JSON.parse(loggedJson);

    expect(parsed.node).toBe("node2");
    expect(parsed.effect_type).toBe("HttpFetch");
  });

  it("does not include optional details when not provided", () => {
    const ctx: LogContext = {
      sessionId: "test-session",
    };

    logGraphEvent(ctx, "complete");

    const loggedJson = logSpy.mock.calls[0][0];
    const parsed = JSON.parse(loggedJson);

    expect(parsed).not.toHaveProperty("error");
    expect(parsed).not.toHaveProperty("node");
    expect(parsed).not.toHaveProperty("effect_type");
  });

  it("produces timestamps in ISO format", () => {
    const ctx: LogContext = {
      sessionId: "test-session",
    };

    logGraphEvent(ctx, "start");

    const loggedJson = logSpy.mock.calls[0][0];
    const parsed = JSON.parse(loggedJson);

    // Verify ISO 8601 format
    expect(parsed.ts).toMatch(/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/);

    // Verify it's a valid date
    const date = new Date(parsed.ts);
    expect(date.toISOString()).toBe(parsed.ts);
  });

  it("sets level to error only for error event type", () => {
    const ctx: LogContext = {
      sessionId: "test-session",
    };

    const testCases: Array<{
      event: "start" | "complete" | "error" | "yield";
      expectedLevel: "info" | "error";
    }> = [
      { event: "start", expectedLevel: "info" },
      { event: "complete", expectedLevel: "info" },
      { event: "yield", expectedLevel: "info" },
      { event: "error", expectedLevel: "error" },
    ];

    testCases.forEach(({ event, expectedLevel }) => {
      logSpy.mockClear();
      logGraphEvent(ctx, event);

      const loggedJson = logSpy.mock.calls[0][0];
      const parsed = JSON.parse(loggedJson);

      expect(parsed.level).toBe(expectedLevel);
    });
  });
});
