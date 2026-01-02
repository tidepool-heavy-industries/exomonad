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
import { sendMessage, sendTypingAction, answerCallbackQuery, sendPhoto, sendDocument } from "./api.js";
import type {
  TelegramIncomingMessage,
  SerializableEffect,
  EffectResult,
} from "tidepool-generated-ts";
import {
  handleTelegramSend,
  handleTelegramReceive,
  handleTelegramTryReceive,
  handleTelegramConfirm,
  type TelegramHandlerContext,
} from "../handlers/telegram.js";

/**
 * Result from StateMachineDO's /start and /resume endpoints.
 * Mirrors the ServerMessage type but for HTTP responses.
 */
type GraphStartResult =
  | { type: "yield"; effect: SerializableEffect; sessionId: string }
  | { type: "done"; result: unknown }
  | { type: "error"; message: string };

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
  pendingMessages: z.array(IncomingMessageSchema).default([]),
  waitingForReceive: z.boolean().default(false),
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

    // Log state for debugging resume logic
    console.log(`[TelegramDO] state: waiting=${state.waitingForReceive}, effect=${state.pendingEffect?.type ?? 'none'}, session=${state.wasmSessionId ?? 'none'}`);

    // Add to pending messages queue
    state.pendingMessages.push(incomingMessage);
    state.lastActivity = Date.now();

    // If we're waiting for a Receive/Confirm, resume processing
    if (state.waitingForReceive && state.pendingEffect && state.wasmSessionId) {
      console.log(`[TelegramDO] Resuming blocked effect: ${state.pendingEffect.type}`);
      state.waitingForReceive = false;
      const effect = state.pendingEffect;
      // Don't clear pendingEffect yet - handleYieldedEffect may need to check if buttons sent

      this.state = state;
      await this.saveState();

      // Get StateMachineDO stub for resuming
      const doId = this.env.STATE_MACHINE.idFromName(state.wasmSessionId);
      const stub = this.env.STATE_MACHINE.get(doId);

      // Re-run the effect loop - handleYieldedEffect will process the pending effect
      // with the new messages in the queue
      await this.handleGraphResult(state.chatId, state.wasmSessionId, stub, {
        type: "yield",
        effect,
        sessionId: state.wasmSessionId,
      });
      return new Response("OK");
    }

    await this.saveState();

    // If no active session OR we have a stale session (not waiting for input),
    // start a new graph session for text messages
    if (incomingMessage.type === "text") {
      if (!state.wasmSessionId || !state.waitingForReceive) {
        // Clear any stale session state
        state.wasmSessionId = null;
        state.pendingEffect = null;
        state.pendingMessages = [incomingMessage]; // Keep only current message
        this.state = state;
        await this.saveState();
        await this.startGraphSession(chatId, incomingMessage);
      }
    }

    return new Response("OK");
  }

  /**
   * Start a new StateMachineDO session with the habitica graph.
   *
   * Converts the incoming message to RawInput and initiates graph execution.
   * TelegramDO then polls the StateMachineDO (via HTTP /resume) to retrieve
   * and execute Telegram-related effects.
   */
  private async startGraphSession(
    chatId: number,
    message: TelegramIncomingMessage
  ): Promise<void> {
    // Convert message to RawInput text
    const rawInput = this.messageToRawInput(message);
    if (!rawInput) {
      console.log(`[TelegramDO] Message type ${message.type} cannot be converted to RawInput`);
      await sendMessage(
        this.env.TELEGRAM_TOKEN,
        chatId,
        "Sorry, I can only process text messages right now."
      );
      return;
    }

    // Generate session ID based on chat ID + timestamp for uniqueness
    const sessionId = `telegram-${chatId}-${Date.now()}`;
    console.log(`[TelegramDO] Starting graph session: ${sessionId}`);

    // Show typing indicator while processing
    await sendTypingAction(this.env.TELEGRAM_TOKEN, chatId);

    // Store session ID before starting (in case we need to resume)
    if (this.state) {
      this.state.wasmSessionId = sessionId;
      // Do not clear pendingMessages here; preserve any queued messages
      await this.saveState();
    }

    try {
      // Get StateMachineDO stub
      const doId = this.env.STATE_MACHINE.idFromName(sessionId);
      const stub = this.env.STATE_MACHINE.get(doId);

      // Start the graph via HTTP (not WebSocket)
      // POST /start initiates graph execution and returns first effect or result
      const startResponse = await stub.fetch(
        new Request(`https://do/start`, {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({
            graphId: "habitica",
            input: { type: "text", text: rawInput }, // RawInput expects {type, text}
            telegramChatId: chatId,
          }),
        })
      );

      if (!startResponse.ok) {
        throw new Error(`StateMachineDO start failed: ${startResponse.status}`);
      }

      const startResult = await startResponse.json() as GraphStartResult;
      console.log(`[TelegramDO] Graph started, result type: ${startResult.type}`);

      // Handle the graph execution loop
      await this.handleGraphResult(chatId, sessionId, stub, startResult);
    } catch (err) {
      console.error(`[TelegramDO] Graph execution error:`, err);
      await sendMessage(
        this.env.TELEGRAM_TOKEN,
        chatId,
        `Sorry, something went wrong: ${err instanceof Error ? err.message : String(err)}`
      );

      // Clear session on error
      if (this.state) {
        this.state.wasmSessionId = null;
        await this.saveState();
      }
    }
  }

  /**
   * Convert incoming message to RawInput text for the graph.
   * Returns null if message type is not supported.
   */
  private messageToRawInput(message: TelegramIncomingMessage): string | null {
    switch (message.type) {
      case "text":
        return message.text;
      case "photo":
        // Could potentially use caption, but photos need special handling
        return message.caption ?? null;
      case "document":
        // Documents not supported for task extraction
        return null;
      case "button_click":
        // Button clicks are handled separately in the confirm flow
        return null;
      default: {
        const _exhaustive: never = message;
        console.error(`[TelegramDO] Unknown message type:`, _exhaustive);
        return null;
      }
    }
  }

  /**
   * Handle graph execution results, processing effects as needed.
   *
   * This is the main loop that:
   * 1. Receives effects from StateMachineDO
   * 2. Handles Telegram effects locally
   * 3. Sends results back to resume graph execution
   *
   * StateMachineDO handles non-Telegram effects (Log, LLM, Habitica) internally.
   * It only yields Telegram effects to us for handling.
   */
  private async handleGraphResult(
    chatId: number,
    sessionId: string,
    stub: DurableObjectStub,
    result: GraphStartResult
  ): Promise<void> {
    let currentResult = result;

    while (true) {
      switch (currentResult.type) {
        case "done": {
          // Graph completed successfully
          console.log(`[TelegramDO] Graph completed:`, currentResult.result);

          // Send success message to user
          const resultObj = currentResult.result as { message?: string; success?: boolean } | null;
          if (resultObj?.message) {
            await sendMessage(
              this.env.TELEGRAM_TOKEN,
              chatId,
              `✓ ${resultObj.message}`
            );
          }

          if (this.state) {
            this.state.wasmSessionId = null;
            await this.saveState();
          }
          return;
        }

        case "error": {
          // Graph failed
          console.error(`[TelegramDO] Graph error:`, currentResult.message);
          await sendMessage(
            this.env.TELEGRAM_TOKEN,
            chatId,
            `Error: ${currentResult.message}`
          );
          if (this.state) {
            this.state.wasmSessionId = null;
            await this.saveState();
          }
          return;
        }

        case "yield": {
          // Effect needs to be handled - dispatch based on effect type
          const effect = currentResult.effect;
          console.log(`[TelegramDO] Handling effect: ${effect.type}`);

          const handleResult = await this.handleYieldedEffect(effect, chatId);

          switch (handleResult.outcome) {
            case "blocking":
              // Receive effect is waiting for messages - exit loop
              console.log(`[TelegramDO] Receive blocking, waiting for message`);
              return;

            case "handled":
              // Effect handled, resume graph with result
              break;
          }

          // Resume graph with the result
          const resumeResponse = await stub.fetch(
            new Request(`https://do/resume`, {
              method: "POST",
              headers: { "Content-Type": "application/json" },
              body: JSON.stringify({ result: handleResult.result }),
            })
          );

          if (!resumeResponse.ok) {
            throw new Error(`StateMachineDO resume failed: ${resumeResponse.status}`);
          }

          currentResult = await resumeResponse.json() as GraphStartResult;
          break;
        }

        default: {
          const _exhaustive: never = currentResult;
          throw new Error(`Unknown result type: ${JSON.stringify(_exhaustive)}`);
        }
      }
    }
  }

  /**
   * Handle a yielded effect from StateMachineDO.
   *
   * Returns either:
   * - { outcome: "handled", result: EffectResult } - effect processed, resume with result
   * - { outcome: "blocking" } - Receive effect waiting for messages
   */
  private async handleYieldedEffect(
    effect: SerializableEffect,
    chatId: number
  ): Promise<
    | { outcome: "handled"; result: EffectResult }
    | { outcome: "blocking" }
  > {
    const state = await this.loadState();
    const ctx: TelegramHandlerContext = {
      chatId,
      pendingMessages: state.pendingMessages,
    };

    switch (effect.type) {
      // ─────────────────────────────────────────────────────────────────────
      // Telegram effects - handled locally by TelegramDO
      // ─────────────────────────────────────────────────────────────────────

      case "telegram_send": {
        const result = await handleTelegramSend(effect, this.env, ctx);
        return { outcome: "handled", result };
      }

      case "telegram_receive": {
        const receiveResult = handleTelegramReceive(effect, ctx);
        if (receiveResult.type === "yield") {
          // Block until messages arrive
          state.waitingForReceive = true;
          state.pendingEffect = effect;
          await this.saveState();
          return { outcome: "blocking" };
        }
        // Messages available
        state.pendingMessages = [];
        await this.saveState();
        return { outcome: "handled", result: receiveResult.result };
      }

      case "telegram_try_receive": {
        const result = handleTelegramTryReceive(effect, ctx);
        state.pendingMessages = [];
        await this.saveState();
        return { outcome: "handled", result };
      }

      case "TelegramConfirm": {
        // Check if buttons were already sent (resuming after blocking)
        const buttonsSent = state.pendingEffect?.type === "TelegramConfirm";

        const confirmResult = await handleTelegramConfirm(
          effect,
          this.env,
          ctx,
          buttonsSent
        );

        if (confirmResult.type === "yield") {
          // Block until button click arrives
          state.waitingForReceive = true; // Reuse this flag for confirm too
          state.pendingEffect = effect;
          await this.saveState();
          return { outcome: "blocking" };
        }

        // Button click received
        state.pendingMessages = [];
        state.pendingEffect = null;
        await this.saveState();
        return { outcome: "handled", result: confirmResult.result };
      }

      // ─────────────────────────────────────────────────────────────────────
      // Internal effects - StateMachineDO handles these, shouldn't reach here
      // Return error result to let the graph decide how to proceed
      // ─────────────────────────────────────────────────────────────────────

      case "LogInfo":
      case "LogError":
      case "LlmComplete":
      case "Habitica": {
        console.error(
          `[TelegramDO] Received internal effect "${effect.type}" that should be handled by StateMachineDO`
        );
        return {
          outcome: "handled",
          result: {
            type: "error",
            message: `Effect "${effect.type}" was unexpectedly yielded to TelegramDO`,
          },
        };
      }

      // ─────────────────────────────────────────────────────────────────────
      // Exhaustive check - TypeScript errors if we miss a case
      // ─────────────────────────────────────────────────────────────────────

      default: {
        const _exhaustive: never = effect;
        return {
          outcome: "handled",
          result: {
            type: "error",
            message: `Unknown effect type: ${JSON.stringify(_exhaustive)}`,
          },
        };
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
