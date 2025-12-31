/**
 * Telegram Durable Object.
 *
 * Handles Telegram updates for a single chat, bridging to StateMachineDO.
 *
 * Architecture:
 * - One TelegramDO per chat_id
 * - Manages message queue for pending user messages
 * - Handles blocking Receive by waiting for webhook updates
 * - Bridges Telegram effects from WASM to Telegram API
 */

import { DurableObject } from "cloudflare:workers";
import type { TelegramUpdate, TelegramEnv } from "./types.js";
import {
  extractChatId,
  extractUser,
  isAllowedUser,
  updateToIncomingMessage,
} from "./types.js";
import { sendMessage, sendTypingAction, answerCallbackQuery, sendPhoto, sendDocument } from "./api.js";
import type {
  TelegramIncomingMessage,
  SerializableEffect,
  EffectResult,
} from "../protocol.js";
import {
  handleTelegramSend,
  handleTelegramReceive,
  handleTelegramTryReceive,
  type TelegramHandlerContext,
} from "../handlers/telegram.js";

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
  /** Messages awaiting pickup by WASM (for Receive/TryReceive) */
  pendingMessages: TelegramIncomingMessage[];
  /** Is WASM blocked on a Receive effect? */
  waitingForReceive: boolean;
  /** The effect we're waiting on, if any */
  pendingEffect: SerializableEffect | null;
  /** Last activity timestamp */
  lastActivity: number;
}

/**
 * Default empty state.
 */
function emptyState(chatId: number): ConversationState {
  return {
    chatId,
    wasmSessionId: null,
    pendingMessages: [],
    waitingForReceive: false,
    pendingEffect: null,
    lastActivity: Date.now(),
  };
}

/**
 * Durable Object for handling Telegram conversations.
 *
 * One instance per chat_id, identified by "chat:{chatId}" name.
 */
export class TelegramDO extends DurableObject<TelegramDOEnv> {
  private chatId: number | null = null;
  private state: ConversationState | null = null;

  /**
   * Load state from storage.
   */
  private async loadState(): Promise<ConversationState> {
    if (this.state) return this.state;

    const stored = await this.ctx.storage.get<ConversationState>("state");
    if (stored) {
      this.state = stored;
      this.chatId = stored.chatId;
      return stored;
    }

    // Will be initialized when we get the first update
    return emptyState(0);
  }

  /**
   * Save state to storage.
   */
  private async saveState(): Promise<void> {
    if (this.state) {
      await this.ctx.storage.put("state", this.state);
    }
  }

  /**
   * Handle incoming requests from webhook router or StateMachineDO.
   */
  async fetch(request: Request): Promise<Response> {
    const url = new URL(request.url);

    switch (url.pathname) {
      case "/update": {
        // Webhook update from Telegram
        const update = (await request.json()) as TelegramUpdate;
        return this.handleUpdate(update);
      }

      case "/effect": {
        // Effect from StateMachineDO (future use)
        const effectRequest = (await request.json()) as {
          effect: SerializableEffect;
          sessionId: string;
        };
        return this.handleEffect(effectRequest.effect, effectRequest.sessionId);
      }

      case "/status": {
        // Debug endpoint
        const state = await this.loadState();
        return Response.json({
          chatId: state.chatId,
          pendingMessages: state.pendingMessages.length,
          waitingForReceive: state.waitingForReceive,
          hasSession: state.wasmSessionId !== null,
        });
      }

      default:
        return new Response("Not found", { status: 404 });
    }
  }

  /**
   * Process a Telegram update from webhook.
   */
  private async handleUpdate(update: TelegramUpdate): Promise<Response> {
    const chatId = extractChatId(update);
    if (!chatId) {
      return new Response("OK");
    }

    // Initialize or load state
    let state = await this.loadState();
    if (state.chatId === 0) {
      state = emptyState(chatId);
      this.state = state;
      this.chatId = chatId;
    }

    const user = extractUser(update);

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

    // Answer callback query if present (to remove loading indicator)
    if (update.callback_query?.id) {
      await answerCallbackQuery(this.env.TELEGRAM_TOKEN, update.callback_query.id);
    }

    // Convert update to IncomingMessage
    const incomingMessage = updateToIncomingMessage(update);
    if (!incomingMessage) {
      console.log(`[TelegramDO] Could not convert update to IncomingMessage`);
      return new Response("OK");
    }

    console.log(
      `[TelegramDO] Received message from ${user?.username ?? user?.id}: ${JSON.stringify(incomingMessage)}`
    );

    // Add to pending messages queue
    state.pendingMessages.push(incomingMessage);
    state.lastActivity = Date.now();

    // If we're waiting for a Receive, resume processing
    if (state.waitingForReceive && state.pendingEffect) {
      console.log(`[TelegramDO] Resuming blocked Receive`);
      state.waitingForReceive = false;
      const effect = state.pendingEffect;
      state.pendingEffect = null;

      await this.saveState();

      // Process the effect now that we have messages
      return this.processEffect(effect, state);
    }

    await this.saveState();

    // If no active session, start echo mode (for testing)
    // TODO: Replace with StateMachineDO integration
    if (!state.wasmSessionId) {
      await this.handleEchoMode(chatId, incomingMessage);
    }

    return new Response("OK");
  }

  /**
   * Handle an effect from StateMachineDO.
   */
  private async handleEffect(
    effect: SerializableEffect,
    sessionId: string
  ): Promise<Response> {
    const state = await this.loadState();
    state.wasmSessionId = sessionId;
    state.lastActivity = Date.now();

    await this.saveState();
    return this.processEffect(effect, state);
  }

  /**
   * Process a Telegram effect.
   */
  private async processEffect(
    effect: SerializableEffect,
    state: ConversationState
  ): Promise<Response> {
    const ctx: TelegramHandlerContext = {
      chatId: state.chatId,
      pendingMessages: state.pendingMessages,
    };

    let result: EffectResult;

    switch (effect.type) {
      case "telegram_send":
        result = await handleTelegramSend(effect, this.env, ctx);
        break;

      case "telegram_receive": {
        const receiveResult = handleTelegramReceive(effect, ctx);
        if (receiveResult.type === "yield") {
          // Need to block until messages arrive
          console.log(`[TelegramDO] Blocking on Receive`);
          state.waitingForReceive = true;
          state.pendingEffect = effect;
          await this.saveState();
          return Response.json({ type: "yield", effect });
        }
        // Messages available, clear the queue
        state.pendingMessages = [];
        await this.saveState();
        result = receiveResult.result;
        break;
      }

      case "telegram_try_receive":
        result = handleTelegramTryReceive(effect, ctx);
        // Clear the queue after reading
        state.pendingMessages = [];
        await this.saveState();
        break;

      default:
        // Not a Telegram effect - shouldn't happen
        console.error(`[TelegramDO] Unknown effect type: ${(effect as { type: string }).type}`);
        return Response.json(
          { type: "error", message: "Unknown effect type" },
          { status: 400 }
        );
    }

    return Response.json({ type: "result", result });
  }

  /**
   * Echo mode for testing (when no WASM session is active).
   * Echoes back the same type of message: text, photo, document, or button click.
   */
  private async handleEchoMode(
    chatId: number,
    message: TelegramIncomingMessage
  ): Promise<void> {
    await sendTypingAction(this.env.TELEGRAM_TOKEN, chatId);

    let result: { message_id: number } | null = null;

    switch (message.type) {
      case "text":
        result = await sendMessage(
          this.env.TELEGRAM_TOKEN,
          chatId,
          `Echo: ${message.text}`
        );
        break;

      case "photo":
        // Echo back the same photo with an "Echo:" caption
        result = await sendPhoto(
          this.env.TELEGRAM_TOKEN,
          chatId,
          message.media,
          message.caption ? `Echo: ${message.caption}` : "Echo!"
        );
        break;

      case "document":
        // Echo back the same document
        result = await sendDocument(
          this.env.TELEGRAM_TOKEN,
          chatId,
          message.media,
          `Echo: ${message.filename}`
        );
        break;

      case "button_click":
        result = await sendMessage(
          this.env.TELEGRAM_TOKEN,
          chatId,
          `Button clicked: ${JSON.stringify(message.data)}`
        );
        break;

      default: {
        const _exhaustive: never = message;
        console.error("[TelegramDO] Unknown message type:", _exhaustive);
        result = await sendMessage(
          this.env.TELEGRAM_TOKEN,
          chatId,
          "Received unknown message type"
        );
      }
    }

    if (result) {
      console.log(`[TelegramDO] Sent echo response, message_id: ${result.message_id}`);
    }

    // Clear pending messages after echo
    if (this.state) {
      this.state.pendingMessages = [];
      await this.saveState();
    }
  }
}
