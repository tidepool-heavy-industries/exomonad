/**
 * Tests for log effect handlers.
 */

import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { handleLogInfo, handleLogError } from "../log.js";
import type { LogInfoEffect, LogErrorEffect } from "../../protocol.js";

describe("handleLogInfo", () => {
  const originalLog = console.log;

  beforeEach(() => {
    console.log = vi.fn();
  });

  afterEach(() => {
    console.log = originalLog;
  });

  it("logs message with [Graph Log] prefix", async () => {
    const effect: LogInfoEffect = {
      type: "LogInfo",
      eff_message: "Test message",
    };

    const result = await handleLogInfo(effect);

    expect(console.log).toHaveBeenCalledWith("[Graph Log] Test message");
    expect(result).toEqual({ type: "success", value: null });
  });

  it("handles empty message", async () => {
    const effect: LogInfoEffect = {
      type: "LogInfo",
      eff_message: "",
    };

    const result = await handleLogInfo(effect);

    expect(console.log).toHaveBeenCalledWith("[Graph Log] ");
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

  it("logs message with [Graph Error] prefix", async () => {
    const effect: LogErrorEffect = {
      type: "LogError",
      eff_message: "Error message",
    };

    const result = await handleLogError(effect);

    expect(console.error).toHaveBeenCalledWith("[Graph Error] Error message");
    expect(result).toEqual({ type: "success", value: null });
  });

  it("handles multiline message", async () => {
    const effect: LogErrorEffect = {
      type: "LogError",
      eff_message: "Line 1\nLine 2",
    };

    const result = await handleLogError(effect);

    expect(console.error).toHaveBeenCalledWith("[Graph Error] Line 1\nLine 2");
    expect(result).toEqual({ type: "success", value: null });
  });
});
