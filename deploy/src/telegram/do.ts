/**
 * Telegram Durable Object.
 *
 * Handles Telegram updates for a single chat.
 * Currently implements echo bot; will later bridge to StateMachineDO.
 */

import { DurableObject } from "cloudflare:workers";
import type { TelegramUpdate, TelegramEnv } from "./types.js";
import { extractChatId, extractText, extractUser, isAllowedUser } from "./types.js";
import { sendMessage, sendTypingAction } from "./api.js";

/**
 * Environment bindings for TelegramDO.
 */
export interface TelegramDOEnv extends TelegramEnv {
  STATE_MACHINE: DurableObjectNamespace;
}

/**
 * Conversation state stored in DO storage.
 */
interface ConversationState {
  chatId: number;
  /** Session ID for future StateMachineDO integration */
  wasmSessionId: string | null;
  lastActivity: number;
}

/**
 * Durable Object for handling Telegram conversations.
 *
 * One instance per chat_id, identified by "chat:{chatId}" name.
 */
export class TelegramDO extends DurableObject<TelegramDOEnv> {
  private chatId: number | null = null;

  /**
   * Handle incoming requests from webhook router.
   */
  async fetch(request: Request): Promise<Response> {
    const url = new URL(request.url);

    // Handle webhook update
    if (url.pathname === "/update") {
      const update = (await request.json()) as TelegramUpdate;
      return this.handleUpdate(update);
    }

    return new Response("Not found", { status: 404 });
  }

  /**
   * Process a Telegram update.
   */
  private async handleUpdate(update: TelegramUpdate): Promise<Response> {
    const chatId = extractChatId(update);
    if (!chatId) {
      return new Response("OK");
    }

    this.chatId = chatId;
    const text = extractText(update);
    const user = extractUser(update);

    // Ignore non-text messages for now
    if (!text) {
      console.log(`[TelegramDO] No text in update from chat ${chatId}`);
      return new Response("OK");
    }

    // Check user authorization
    if (!isAllowedUser(user?.id, this.env.ALLOWED_TELEGRAM_USERS)) {
      console.log(`[TelegramDO] Unauthorized user: ${user?.id}`);
      await sendMessage(
        this.env.TELEGRAM_TOKEN,
        chatId,
        "Sorry, you're not authorized to use this bot."
      );
      return new Response("OK");
    }

    console.log(`[TelegramDO] Received message from ${user?.username ?? user?.id}: ${text}`);

    // Update conversation state
    await this.updateConversationState(chatId);

    // Show typing indicator
    await sendTypingAction(this.env.TELEGRAM_TOKEN, chatId);

    // Echo the message back
    // TODO: Replace with StateMachineDO integration
    const response = `Echo: ${text}`;
    const result = await sendMessage(this.env.TELEGRAM_TOKEN, chatId, response);

    if (result) {
      console.log(`[TelegramDO] Sent response, message_id: ${result.message_id}`);
    } else {
      console.error(`[TelegramDO] Failed to send response`);
    }

    return new Response("OK");
  }

  /**
   * Update or create conversation state.
   */
  private async updateConversationState(chatId: number): Promise<ConversationState> {
    const key = "state";
    let state = await this.ctx.storage.get<ConversationState>(key);

    if (!state) {
      state = {
        chatId,
        wasmSessionId: null,
        lastActivity: Date.now(),
      };
    } else {
      state.lastActivity = Date.now();
    }

    await this.ctx.storage.put(key, state);
    return state;
  }
}
