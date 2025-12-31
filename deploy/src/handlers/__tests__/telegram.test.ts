/**
 * Tests for Telegram effect handler.
 */

import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { handleTelegramSend, type TelegramHandlerEnv } from "../telegram.js";
import type { TelegramSendEffect } from "../../protocol.js";

// Store original fetch
const originalFetch = globalThis.fetch;

// Mock environment
function createMockEnv(): TelegramHandlerEnv {
  return {
    TELEGRAM_TOKEN: "test-bot-token",
  };
}

describe("handleTelegramSend", () => {
  const baseEffect: TelegramSendEffect = {
    type: "TelegramSend",
    eff_chat_id: 123456,
    eff_text: "Hello, world!",
  };

  beforeEach(() => {
    // Reset fetch mock before each test
    globalThis.fetch = vi.fn();
  });

  afterEach(() => {
    globalThis.fetch = originalFetch;
  });

  it("sends message successfully and returns message_id", async () => {
    const mockResponse = {
      ok: true,
      result: { message_id: 42 },
    };
    vi.mocked(globalThis.fetch).mockResolvedValue({
      json: () => Promise.resolve(mockResponse),
    } as Response);

    const result = await handleTelegramSend(baseEffect, createMockEnv());

    expect(result).toEqual({
      type: "success",
      value: { message_id: 42 },
    });

    // Verify fetch was called correctly
    expect(globalThis.fetch).toHaveBeenCalledWith(
      "https://api.telegram.org/bottest-bot-token/sendMessage",
      expect.objectContaining({
        method: "POST",
        headers: { "Content-Type": "application/json" },
      })
    );
  });

  it("includes parse_mode when specified", async () => {
    const effectWithParseMode: TelegramSendEffect = {
      ...baseEffect,
      eff_parse_mode: "HTML",
    };
    const mockResponse = {
      ok: true,
      result: { message_id: 42 },
    };
    vi.mocked(globalThis.fetch).mockResolvedValue({
      json: () => Promise.resolve(mockResponse),
    } as Response);

    await handleTelegramSend(effectWithParseMode, createMockEnv());

    const fetchCall = vi.mocked(globalThis.fetch).mock.calls[0];
    const body = JSON.parse(fetchCall[1]?.body as string);
    expect(body.parse_mode).toBe("HTML");
  });

  it("returns error when API returns ok: false", async () => {
    const mockResponse = {
      ok: false,
      description: "Chat not found",
      error_code: 400,
    };
    vi.mocked(globalThis.fetch).mockResolvedValue({
      json: () => Promise.resolve(mockResponse),
    } as Response);

    const result = await handleTelegramSend(baseEffect, createMockEnv());

    expect(result).toEqual({
      type: "error",
      message: "Failed to send Telegram message",
    });
  });

  it("handles network errors gracefully", async () => {
    vi.mocked(globalThis.fetch).mockRejectedValue(new Error("Network failure"));

    const result = await handleTelegramSend(baseEffect, createMockEnv());

    // Network errors are caught by api.ts and logged, handler returns generic error
    expect(result).toEqual({
      type: "error",
      message: "Failed to send Telegram message",
    });
  });

  it("handles JSON parse errors gracefully", async () => {
    vi.mocked(globalThis.fetch).mockResolvedValue({
      json: () => Promise.reject(new Error("Invalid JSON")),
    } as Response);

    const result = await handleTelegramSend(baseEffect, createMockEnv());

    // JSON errors are caught by api.ts and logged, handler returns generic error
    expect(result).toEqual({
      type: "error",
      message: "Failed to send Telegram message",
    });
  });
});
