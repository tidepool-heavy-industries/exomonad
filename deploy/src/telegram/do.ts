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
import { z } from "zod";
import type { TelegramUpdate, TelegramEnv } from "./types.js";
import {
  extractChatId,
  extractUser,
  isAllowedUser,
  updateToIncomingMessage,
} from "./types.js";
import { sendMessage, answerCallbackQuery, sendMessageWithButtons } from "./api.js";
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

// =============================================================================
// Zod Schemas - Runtime validation at storage boundary
// =============================================================================

/**
 * Schema for incoming messages.
 * Discriminated union on 'type' field.
 */
const IncomingMessageSchema = z.discriminatedUnion("type", [
  z.object({ type: z.literal("text"), text: z.string() }),
  z.object({ type: z.literal("photo"), media: z.string(), caption: z.string().optional() }),
  z.object({ type: z.literal("document"), media: z.string(), filename: z.string() }),
  z.object({ type: z.literal("button_click"), data: z.unknown() }),
]);

/**
 * Conversation state schema with defaults for missing fields.
 *
 * This is the source of truth for state shape. When loaded from storage:
 * - Missing fields get defaults (schema evolution)
 * - Invalid data throws (corruption detection)
 * - Type safety is enforced at runtime, not just compile time
 *
 * Note: pendingEffect uses z.unknown() because SerializableEffect is a complex
 * union that we just need to persist and restore, not deeply validate.
 */
const ConversationStateSchema = z.object({
  chatId: z.number(),
  wasmSessionId: z.string().nullable().default(null),
  graphId: z.string().default("habitica"),
  pendingMessages: z.array(IncomingMessageSchema).default([]),
  waitingForReceive: z.boolean().default(false),
  waitingForConfirm: z.boolean().default(false),
  pendingEffect: z.unknown().default(null),
  lastActivity: z.number().default(() => Date.now()),
});

/**
 * Conversation state type.
 * We override pendingEffect to use the proper SerializableEffect type
 * since zod can't express the full union.
 */
interface ConversationState extends Omit<z.infer<typeof ConversationStateSchema>, 'pendingEffect'> {
  pendingEffect: SerializableEffect | null;
}

/** Type guard for TelegramConfirmEffect */
function isTelegramConfirmEffect(effect: SerializableEffect): effect is import("../protocol.js").TelegramConfirmEffect {
  return effect.type === "TelegramConfirm";
}

/**
 * Parse storage data into validated state.
 * Throws on invalid data (corruption), provides defaults for missing fields (evolution).
 */
function parseState(stored: unknown, defaultChatId: number): ConversationState {
  // If nothing stored, create fresh state
  if (stored === undefined || stored === null) {
    return ConversationStateSchema.parse({ chatId: defaultChatId }) as ConversationState;
  }

  // Merge with default chatId if missing, then validate
  const withDefaults = { chatId: defaultChatId, ...(stored as object) };
  return ConversationStateSchema.parse(withDefaults) as ConversationState;
}

/**
 * Create an empty state for a new conversation.
 */
function emptyState(chatId: number): ConversationState {
  return ConversationStateSchema.parse({ chatId }) as ConversationState;
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
   * Uses zod schema to validate and provide defaults for missing fields.
   */
  private async loadState(): Promise<ConversationState> {
    if (this.state) return this.state;

    // Get raw storage data - don't trust the type, let zod validate it
    const stored = await this.ctx.storage.get("state");
    if (stored !== undefined) {
      // parseState validates shape and provides defaults for missing fields
      this.state = parseState(stored, 0);
      this.chatId = this.state.chatId;
      return this.state;
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
    console.log(`[TelegramDO] fetch() called with path: ${url.pathname}`);

    switch (url.pathname) {
      case "/update": {
        // Webhook update from Telegram
        console.log("[TelegramDO] Processing /update");
        const update = (await request.json()) as TelegramUpdate;
        console.log(`[TelegramDO] Update parsed: ${JSON.stringify(update).slice(0, 300)}`);
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

    // If we're waiting for confirmation and got a button click, resume
    if (state.waitingForConfirm && state.pendingEffect && incomingMessage.type === "button_click") {
      console.log(`[TelegramDO] Resuming after confirmation button click`);
      state.waitingForConfirm = false;
      const effect = state.pendingEffect;
      state.pendingEffect = null;

      await this.saveState();

      // Process the TelegramConfirm effect - it will find the button_click in pendingMessages
      return this.processEffect(effect, state);
    }

    await this.saveState();

    // If no active session, start a Habitica routing session
    if (!state.wasmSessionId && incomingMessage.type === "text") {
      await this.startHabiticaSession(chatId, incomingMessage.text);
    }

    return new Response("OK");
  }

  /**
   * Start a new Habitica routing session.
   * Creates a StateMachineDO session and initiates the graph.
   */
  private async startHabiticaSession(chatId: number, messageText: string): Promise<void> {
    const sessionId = `telegram:${chatId}:${Date.now()}`;
    console.log(`[TelegramDO] Starting Habitica session: ${sessionId}`);

    // Update state with new session ID
    if (this.state) {
      this.state.wasmSessionId = sessionId;
      this.state.graphId = "habitica";
      await this.saveState();
    }

    // Get StateMachineDO stub
    const doId = this.env.STATE_MACHINE.idFromName(sessionId);
    const stub = this.env.STATE_MACHINE.get(doId);

    // Call the /run endpoint to start the graph
    try {
      const response = await stub.fetch(
        new Request("https://internal/run", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({
            graphId: "habitica",
            input: messageText,
            chatId: chatId,
          }),
        })
      );

      if (!response.ok) {
        const errorText = await response.text();
        console.error(`[TelegramDO] StateMachine /run failed: ${response.status} ${errorText}`);
        await sendMessage(
          this.env.TELEGRAM_TOKEN,
          chatId,
          `Error starting session: ${errorText}`
        );
        // Clear session on error
        if (this.state) {
          this.state.wasmSessionId = null;
          await this.saveState();
        }
        return;
      }

      const result = await response.json() as { type: string; message?: string };
      console.log(`[TelegramDO] StateMachine /run result: ${JSON.stringify(result)}`);

      // If completed, clear the session
      if (result.type === "done") {
        if (this.state) {
          this.state.wasmSessionId = null;
          await this.saveState();
        }
      }
    } catch (err) {
      console.error(`[TelegramDO] Error calling StateMachine:`, err);
      await sendMessage(
        this.env.TELEGRAM_TOKEN,
        chatId,
        `Error: ${err instanceof Error ? err.message : String(err)}`
      );
      // Clear session on error
      if (this.state) {
        this.state.wasmSessionId = null;
        await this.saveState();
      }
    }
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

      case "TelegramConfirm": {
        // TelegramConfirm effect - show buttons and wait for user to click
        if (isTelegramConfirmEffect(effect)) {
          // Check if we already have a button click in the pending messages
          const buttonClick = state.pendingMessages.find(m => m.type === "button_click");
          if (buttonClick) {
            // We have a response - clear pending messages and return the result
            state.pendingMessages = state.pendingMessages.filter(m => m.type !== "button_click");
            state.waitingForConfirm = false;
            await this.saveState();

            result = {
              type: "success",
              value: { response: buttonClick.data },
            };
            break;
          }

          // No response yet - send buttons and yield
          console.log(`[TelegramDO] Sending confirmation buttons: ${effect.eff_message}`);
          const buttons = effect.eff_buttons.map(([label, value]) => [{ text: label, data: value }]);
          await sendMessageWithButtons(
            this.env.TELEGRAM_TOKEN,
            state.chatId,
            effect.eff_message,
            buttons
          );

          // Mark as waiting for confirmation
          state.waitingForConfirm = true;
          state.pendingEffect = effect;
          await this.saveState();

          return Response.json({ type: "yield", effect });
        }
        // Fallthrough to default if not TelegramConfirmEffect
        console.error(`[TelegramDO] Invalid TelegramConfirm effect`);
        return Response.json(
          { type: "error", message: "Invalid TelegramConfirm effect" },
          { status: 400 }
        );
      }

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
}
