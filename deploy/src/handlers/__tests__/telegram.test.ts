/**
 * Tests for Telegram effect handlers.
 */

import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import {
  handleTelegramSend,
  handleTelegramReceive,
  handleTelegramTryReceive,
  handleTelegramAsk,
  type TelegramHandlerEnv,
  type TelegramHandlerContext,
} from "../telegram.js";
import type { TelegramSendEffect, TelegramAskEffect, TelegramIncomingMessage } from "tidepool-generated-ts";

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

describe("handleTelegramAsk button ID mapping", () => {
  const askEffect: TelegramAskEffect = {
    type: "TelegramAsk",
    eff_tg_text: "Choose a task:",
    eff_buttons: [
      ["First option", "Reach out to someone I haven't talked to in a little while"],
      ["Second option", "This is another very long option text that exceeds 64 bytes easily"],
    ],
  };

  beforeEach(() => {
    globalThis.fetch = vi.fn();
  });

  afterEach(() => {
    globalThis.fetch = originalFetch;
  });

  it("sends buttons with short IDs and returns mapping with message info", async () => {
    const mockResponse = {
      ok: true,
      result: { message_id: 42 },
    };
    vi.mocked(globalThis.fetch).mockResolvedValue({
      json: () => Promise.resolve(mockResponse),
    } as Response);

    const result = await handleTelegramAsk(
      askEffect,
      createMockEnv(),
      createMockContext([]),
      null // No stored nonce - first call
    );

    // Should yield with nonce, buttonMapping, messageId, and questionText
    expect(result.type).toBe("yield");
    if (result.type === "yield") {
      expect(result.nonce).toBeDefined();
      expect(result.messageId).toBe(42);
      expect(result.questionText).toBe("Choose a task:");
      // buttonMapping stores the full button data (including action and nonce)
      expect(result.buttonMapping.btn_0).toMatchObject({
        action: "Reach out to someone I haven't talked to in a little while",
        nonce: result.nonce,
      });
      expect(result.buttonMapping.btn_1).toMatchObject({
        action: "This is another very long option text that exceeds 64 bytes easily",
        nonce: result.nonce,
      });
    }

    // Verify fetch was called with short IDs in callback_data
    const fetchCall = vi.mocked(globalThis.fetch).mock.calls[0];
    const body = JSON.parse(fetchCall[1]?.body as string);
    expect(body.reply_markup.inline_keyboard[0][0].callback_data).toBe("btn_0");
    expect(body.reply_markup.inline_keyboard[1][0].callback_data).toBe("btn_1");
  });

  it("resolves button ID and edits message to show selection", async () => {
    const nonce = "test-nonce-1";
    // buttonMapping stores the full button data with {action, nonce}
    const buttonMapping = {
      btn_0: { action: "Reach out to someone I haven't talked to in a little while", nonce },
      btn_1: { action: "Another long option", nonce },
    };

    // Mock the editMessageText call
    const mockResponse = { ok: true, result: { message_id: 42 } };
    vi.mocked(globalThis.fetch).mockResolvedValue({
      json: () => Promise.resolve(mockResponse),
    } as Response);

    // Simulate a button click with the short ID
    const pendingMessages: TelegramIncomingMessage[] = [
      {
        type: "button_click",
        data: { action: "btn_0", nonce: nonce },
      },
    ];

    const ctx: TelegramHandlerContext = {
      chatId: 123456,
      pendingMessages,
      buttonMapping,
      buttonMessageId: 42,
      buttonQuestionText: "Choose a task:",
    };

    const result = await handleTelegramAsk(
      askEffect,
      createMockEnv(),
      ctx,
      nonce // Stored nonce from previous yield
    );

    // Should return result with the ORIGINAL long data, not the short ID
    expect(result.type).toBe("result");
    if (result.type === "result") {
      expect(result.result).toEqual({
        type: "success",
        value: {
          type: "button",
          response: "Reach out to someone I haven't talked to in a little while",
        },
      });
    }

    // Verify editMessageText was called to update the message
    const fetchCall = vi.mocked(globalThis.fetch).mock.calls[0];
    expect(fetchCall[0]).toContain("editMessageText");
    const body = JSON.parse(fetchCall[1]?.body as string);
    expect(body.message_id).toBe(42);
    expect(body.text).toContain("Choose a task:");
    expect(body.text).toContain("✓");
    expect(body.text).toContain("First option"); // Uses button label
    // Should have empty keyboard to remove buttons
    expect(body.reply_markup.inline_keyboard).toEqual([]);
  });

  it("handles string data directly in button mapping", async () => {
    const nonce = "test-nonce-2";
    // In some cases, the mapping might store a plain string
    // (for backwards compatibility or simpler use cases)
    const buttonMapping = {
      btn_0: "A plain string value",
    };

    const pendingMessages: TelegramIncomingMessage[] = [
      {
        type: "button_click",
        data: { action: "btn_0", nonce: nonce },
      },
    ];

    const ctx: TelegramHandlerContext = {
      chatId: 123456,
      pendingMessages,
      buttonMapping,
    };

    const result = await handleTelegramAsk(
      askEffect,
      createMockEnv(),
      ctx,
      nonce
    );

    expect(result.type).toBe("result");
    if (result.type === "result") {
      expect(result.result).toEqual({
        type: "success",
        value: {
          type: "button",
          response: "A plain string value",
        },
      });
    }
  });
});
