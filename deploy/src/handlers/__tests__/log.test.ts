/**
 * Tests for log effect handlers.
 */

import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { handleLogInfo, handleLogError } from "../log.js";
import type { LogInfoEffect, LogErrorEffect } from "tidepool-generated-ts";

describe("handleLogInfo", () => {
  const originalLog = console.log;

  beforeEach(() => {
    console.log = vi.fn();
  });

  afterEach(() => {
    console.log = originalLog;
  });

  it("logs message as JSON without fields", async () => {
    const effect: LogInfoEffect = {
      type: "LogInfo",
      eff_message: "Test message",
    };

    const result = await handleLogInfo(effect);

    expect(console.log).toHaveBeenCalledWith(
      JSON.stringify({ level: "info", msg: "Test message" })
    );
    expect(result).toEqual({ type: "success", value: null });
  });

  it("logs message with structured fields", async () => {
    const effect: LogInfoEffect = {
      type: "LogInfo",
      eff_message: "Scoring daily",
      eff_fields: {
        taskId: "abc123",
        direction: "up",
      },
    };

    const result = await handleLogInfo(effect);

    expect(console.log).toHaveBeenCalledWith(
      JSON.stringify({
        taskId: "abc123",
        direction: "up",
        level: "info",
        msg: "Scoring daily",
      })
    );
    expect(result).toEqual({ type: "success", value: null });
  });

  it("logs message with numeric fields", async () => {
    const effect: LogInfoEffect = {
      type: "LogInfo",
      eff_message: "LLM tool calls",
      eff_fields: {
        count: 3,
        tools: ["ThinkAsDM", "SpeakAsNPC", "AskPlayer"],
      },
    };

    const result = await handleLogInfo(effect);

    expect(console.log).toHaveBeenCalledWith(
      JSON.stringify({
        count: 3,
        tools: ["ThinkAsDM", "SpeakAsNPC", "AskPlayer"],
        level: "info",
        msg: "LLM tool calls",
      })
    );
    expect(result).toEqual({ type: "success", value: null });
  });

  it("handles empty message", async () => {
    const effect: LogInfoEffect = {
      type: "LogInfo",
      eff_message: "",
    };

    const result = await handleLogInfo(effect);

    expect(console.log).toHaveBeenCalledWith(
      JSON.stringify({ level: "info", msg: "" })
    );
    expect(result).toEqual({ type: "success", value: null });
  });
});

describe("handleLogError", () => {
  const originalError = console.error;

  beforeEach(() => {
    console.error = vi.fn();
  });

  afterEach(() => {
    console.error = originalError;
  });

  it("logs error message as JSON without fields", async () => {
    const effect: LogErrorEffect = {
      type: "LogError",
      eff_message: "Error message",
    };

    const result = await handleLogError(effect);

    expect(console.error).toHaveBeenCalledWith(
      JSON.stringify({ level: "error", msg: "Error message" })
    );
    expect(result).toEqual({ type: "success", value: null });
  });

  it("logs error with structured fields", async () => {
    const effect: LogErrorEffect = {
      type: "LogError",
      eff_message: "API call failed",
      eff_fields: {
        endpoint: "/api/tasks",
        status: 500,
        retryCount: 3,
      },
    };

    const result = await handleLogError(effect);

    expect(console.error).toHaveBeenCalledWith(
      JSON.stringify({
        endpoint: "/api/tasks",
        status: 500,
        retryCount: 3,
        level: "error",
        msg: "API call failed",
      })
    );
    expect(result).toEqual({ type: "success", value: null });
  });

  it("handles multiline message", async () => {
    const effect: LogErrorEffect = {
      type: "LogError",
      eff_message: "Line 1\nLine 2",
    };

    const result = await handleLogError(effect);

    expect(console.error).toHaveBeenCalledWith(
      JSON.stringify({ level: "error", msg: "Line 1\nLine 2" })
    );
    expect(result).toEqual({ type: "success", value: null });
  });
});
