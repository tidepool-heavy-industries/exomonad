/**
 * Tests for Telegram effect handlers.
 */

import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import {
  handleTelegramSend,
  handleTelegramReceive,
  handleTelegramTryReceive,
  type TelegramHandlerEnv,
  type TelegramHandlerContext,
} from "../telegram.js";
import type { TelegramSendEffect, TelegramIncomingMessage } from "tidepool-generated-ts";

// Store original fetch
const originalFetch = globalThis.fetch;

// Mock environment
function createMockEnv(): TelegramHandlerEnv {
  return {
    TELEGRAM_TOKEN: "test-bot-token",
  };
}

// Mock context
function createMockContext(
  pendingMessages: TelegramIncomingMessage[] = []
): TelegramHandlerContext {
  return {
    chatId: 123456,
    pendingMessages,
  };
}

describe("handleTelegramSend", () => {
  const baseEffect: TelegramSendEffect = {
    type: "telegram_send",
    message: { type: "text", text: "Hello, world!" },
  };

  beforeEach(() => {
    // Reset fetch mock before each test
    globalThis.fetch = vi.fn();
  });

  afterEach(() => {
    globalThis.fetch = originalFetch;
  });

  it("sends text message successfully and returns unit", async () => {
    const mockResponse = {
      ok: true,
      result: { message_id: 42 },
    };
    vi.mocked(globalThis.fetch).mockResolvedValue({
      json: () => Promise.resolve(mockResponse),
    } as Response);

    const result = await handleTelegramSend(baseEffect, createMockEnv(), createMockContext());

    expect(result).toEqual({
      type: "success",
      value: { type: "unit" },
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

  it("sends buttons message correctly", async () => {
    const buttonsEffect: TelegramSendEffect = {
      type: "telegram_send",
      message: {
        type: "buttons",
        text: "Choose an option:",
        buttons: [[{ text: "Option A", data: "a" }, { text: "Option B", data: "b" }]],
      },
    };
    const mockResponse = {
      ok: true,
      result: { message_id: 42 },
    };
    vi.mocked(globalThis.fetch).mockResolvedValue({
      json: () => Promise.resolve(mockResponse),
    } as Response);

    await handleTelegramSend(buttonsEffect, createMockEnv(), createMockContext());

    const fetchCall = vi.mocked(globalThis.fetch).mock.calls[0];
    const body = JSON.parse(fetchCall[1]?.body as string);
    expect(body.reply_markup).toBeDefined();
    expect(body.reply_markup.inline_keyboard).toHaveLength(1);
    expect(body.reply_markup.inline_keyboard[0]).toHaveLength(2);
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

    const result = await handleTelegramSend(baseEffect, createMockEnv(), createMockContext());

    expect(result).toEqual({
      type: "error",
      message: "Failed to send Telegram message",
    });
  });

  it("handles network errors gracefully", async () => {
    vi.mocked(globalThis.fetch).mockRejectedValue(new Error("Network failure"));

    const result = await handleTelegramSend(baseEffect, createMockEnv(), createMockContext());

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

    const result = await handleTelegramSend(baseEffect, createMockEnv(), createMockContext());

    // JSON errors are caught by api.ts and logged, handler returns generic error
    expect(result).toEqual({
      type: "error",
      message: "Failed to send Telegram message",
    });
  });
});

describe("handleTelegramReceive", () => {
  it("returns messages when pending messages exist", () => {
    const pendingMessages: TelegramIncomingMessage[] = [
      { type: "text", text: "Hello" },
      { type: "text", text: "World" },
    ];

    const result = handleTelegramReceive(
      { type: "telegram_receive" },
      createMockContext(pendingMessages)
    );

    expect(result).toEqual({
      type: "messages",
      result: {
        type: "success",
        value: { type: "messages", messages: pendingMessages },
      },
    });
  });

  it("returns yield when no pending messages", () => {
    const result = handleTelegramReceive(
      { type: "telegram_receive" },
      createMockContext([])
    );

    expect(result).toEqual({ type: "yield" });
  });
});

describe("handleTelegramTryReceive", () => {
  it("returns pending messages when they exist", () => {
    const pendingMessages: TelegramIncomingMessage[] = [
      { type: "text", text: "Hello" },
    ];

    const result = handleTelegramTryReceive(
      { type: "telegram_try_receive" },
      createMockContext(pendingMessages)
    );

    expect(result).toEqual({
      type: "success",
      value: { type: "messages", messages: pendingMessages },
    });
  });

  it("returns empty messages when none pending", () => {
    const result = handleTelegramTryReceive(
      { type: "telegram_try_receive" },
      createMockContext([])
    );

    expect(result).toEqual({
      type: "success",
      value: { type: "messages", messages: [] },
    });
  });
});
