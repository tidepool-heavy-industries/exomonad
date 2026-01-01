/**
 * Tidepool Cloudflare Worker - Durable Object Harness
 *
 * Hosts GHC WASM state machines with WebSocket communication.
 * See docs/PROTOCOL.md for the WebSocket protocol specification.
 */

import { DurableObject } from "cloudflare:workers";
import { loadMachine, type GraphMachine } from "./loader.js";
import type {
  EffectResult,
  ClientMessage,
  ServerMessage,
  SessionState,
  SerializableEffect,
} from "./protocol.js";
import { SESSION_TIMEOUT_MS } from "./protocol.js";
import { executeEffect, type Env as HandlersEnv } from "./handlers/index.js";
import { routeWebhook, type WebhookEnv } from "./telegram/webhook.js";

// Re-export TelegramDO for Cloudflare Workers
export { TelegramDO } from "./telegram/do.js";

// Import WASM module at build time
import wasmModule from "./tidepool.wasm";

// =============================================================================
// Environment Types
// =============================================================================

export interface Env extends HandlersEnv, WebhookEnv {
  STATE_MACHINE: DurableObjectNamespace<StateMachineDO>;
  // TELEGRAM_DO is inherited from WebhookEnv
}

// =============================================================================
// Durable Object: StateMachineDO
// =============================================================================

export class StateMachineDO extends DurableObject<Env> {
  private machine: GraphMachine | null = null;
  // The session ID is derived from the DO name (set via idFromName() routing)
  // For WebSocket: extracted from URL path /session/:sessionId
  // For HTTP: extracted from this.ctx.id.name (matches idFromName() parameter)
  // This ensures sessionId matches TelegramDO routing for consistency
  private sessionId: string | null = null;

  /**
   * Handle incoming requests - WebSocket upgrade or HTTP API
   */
  async fetch(request: Request): Promise<Response> {
    const url = new URL(request.url);

    // HTTP API for DO-to-DO communication (e.g., TelegramDO → StateMachineDO)
    if (request.method === "POST") {
      switch (url.pathname) {
        case "/start":
          return this.handleHttpStart(request);
        case "/resume":
          return this.handleHttpResume(request);
      }
    }

    // WebSocket upgrade for direct client connections
    const upgradeHeader = request.headers.get("Upgrade");
    if (upgradeHeader !== "websocket") {
      return new Response("Expected WebSocket upgrade or POST to /start|/resume", { status: 426 });
    }

    // Extract session ID from URL path (matches the idFromName routing)
    const match = url.pathname.match(/^\/session\/(.+)$/);
    if (match) {
      this.sessionId = match[1];
    }

    const pair = new WebSocketPair();
    const [client, server] = Object.values(pair);

    // Accept the WebSocket connection using Hibernation API
    this.ctx.acceptWebSocket(server);
    console.log(`[DO] WebSocket connection accepted, sessionId: ${this.sessionId}`);

    return new Response(null, { status: 101, webSocket: client });
  }

  /**
   * Handle HTTP /start - Start graph execution (for DO-to-DO calls).
   *
   * Runs the graph loop until it yields a Telegram effect or completes.
   * Non-Telegram effects (Log, LLM, Habitica) are handled internally.
   */
  private async handleHttpStart(request: Request): Promise<Response> {
    const body = await request.json() as {
      graphId: string;
      input: unknown;
      telegramChatId?: number;
    };

    const { graphId, input } = body;

    // Validate graphId
    if (!graphId || typeof graphId !== "string" || graphId.trim() === "") {
      return Response.json(
        { type: "error", message: "Invalid graphId: must be a non-empty string" },
        { status: 400 }
      );
    }

    // Use the DO name as session ID (set via idFromName() routing)
    // This ensures sessionId matches the TelegramDO routing for consistency
    if (!this.sessionId) {
      this.sessionId = this.ctx.id.name ?? crypto.randomUUID();
    }
    console.log(`[DO] HTTP start: sessionId=${this.sessionId}, graphId=${graphId}`);

    try {
      // Load WASM if not already loaded
      if (!this.machine) {
        this.machine = await loadMachine({
          wasmModule: wasmModule as unknown as WebAssembly.Module,
          debug: false,
        });
      }

      // Initialize the graph
      const output = await this.machine.initialize(input);

      // Run until Telegram effect or completion
      return this.runHttpGraphLoop(output, graphId);
    } catch (err) {
      console.error("[DO] HTTP start error:", err);
      return Response.json({
        type: "error",
        message: err instanceof Error ? err.message : String(err),
      });
    }
  }

  /**
   * Handle HTTP /resume - Continue graph after effect result.
   */
  private async handleHttpResume(request: Request): Promise<Response> {
    const body = await request.json() as { result: EffectResult };
    const { result } = body;

    if (!this.sessionId) {
      return Response.json(
        { type: "error", message: "No active session to resume" },
        { status: 400 }
      );
    }

    const sessionKey = `session:${this.sessionId}`;
    const session = await this.ctx.storage.get<SessionState>(sessionKey);

    if (!session) {
      return Response.json(
        { type: "error", message: "Session not found or expired" },
        { status: 400 }
      );
    }

    console.log(`[DO] HTTP resume: sessionId=${this.sessionId}`);

    try {
      // Update session
      session.lastActivity = Date.now();
      session.pendingEffect = null;
      await this.ctx.storage.put(sessionKey, session);

      // Ensure machine is loaded
      if (!this.machine) {
        this.machine = await loadMachine({
          wasmModule: wasmModule as unknown as WebAssembly.Module,
          debug: false,
        });
      }

      const output = await this.machine.step(result);
      return this.runHttpGraphLoop(output, session.graphId);
    } catch (err) {
      console.error("[DO] HTTP resume error:", err);
      return Response.json({
        type: "error",
        message: err instanceof Error ? err.message : String(err),
      });
    }
  }

  /**
   * Run graph loop for HTTP mode.
   *
   * Handles non-Telegram effects internally (Log, LLM, Habitica).
   * Yields to caller for Telegram effects.
   */
  private async runHttpGraphLoop(
    output: Awaited<ReturnType<GraphMachine["initialize"]>>,
    graphId: string
  ): Promise<Response> {
    const sessionId = this.sessionId!;
    const sessionKey = `session:${sessionId}`;

    while (!output.done && output.effect) {
      const effect = output.effect;

      // Check if this is a Telegram effect - yield to caller
      if (this.isTelegramEffect(effect)) {
        // Save session state for resume
        const session: SessionState = {
          graphId,
          // Persist the current WASM machine state so we can restore after hibernation
          machineState:
            this.machine && "serialize" in this.machine
              ? // `serialize` is provided by the GraphMachine implementation and
                // must return a JSON-serializable snapshot suitable for Durable Object storage.
                // We use a runtime check to avoid issues if some implementations lack it.
                (this.machine as any).serialize()
              : null,
          pendingEffect: effect,
          lastActivity: Date.now(),
        };
        await this.ctx.storage.put(sessionKey, session);
        await this.scheduleCleanupAlarm();

        return Response.json({
          type: "yield",
          effect,
          sessionId,
        });
      }

      // Handle non-Telegram effects internally
      const result = await executeEffect(effect, this.env);

      if (result.type === "error") {
        console.error(`[DO] Effect ${effect.type} failed:`, result.message);
        // Continue anyway - let the graph decide how to handle errors
      }

      // Step with the result
      output = await this.machine!.step(result);
    }

    // Graph completed - clean up
    await this.ctx.storage.delete(sessionKey);

    if (output.graphState.phase.type === "failed") {
      return Response.json({
        type: "error",
        message: output.graphState.phase.error,
      });
    }

    return Response.json({
      type: "done",
      result: output.stepResult,
    });
  }

  /**
   * Check if an effect is a Telegram effect.
   */
  private isTelegramEffect(effect: SerializableEffect): boolean {
    return (
      effect.type === "telegram_send" ||
      effect.type === "telegram_receive" ||
      effect.type === "telegram_try_receive"
    );
  }

  /**
   * Handle WebSocket messages
   */
  async webSocketMessage(ws: WebSocket, message: string | ArrayBuffer): Promise<void> {
    if (typeof message !== "string") {
      this.send(ws, { type: "error", message: "Binary messages not supported", recoverable: false });
      return;
    }

    let msg: ClientMessage;
    try {
      msg = JSON.parse(message) as ClientMessage;
    } catch {
      this.send(ws, { type: "error", message: "Invalid JSON", recoverable: false });
      return;
    }

    console.log(`[DO] Received message: ${msg.type}`);

    try {
      switch (msg.type) {
        case "init":
          await this.handleInit(ws, msg.graphId, msg.input);
          break;

        case "resume":
          await this.handleResume(ws, msg.result);
          break;

        case "reconnect":
          await this.handleReconnect(ws, msg.sessionId);
          break;

        case "ping":
          this.send(ws, { type: "pong" });
          break;

        default:
          this.send(ws, {
            type: "error",
            message: `Unknown message type: ${(msg as { type: string }).type}`,
            recoverable: false,
          });
      }
    } catch (err) {
      this.send(ws, {
        type: "error",
        message: err instanceof Error ? err.message : String(err),
        recoverable: this.sessionId !== null,
        sessionId: this.sessionId ?? undefined,
      });
    }
  }

  /**
   * Handle WebSocket close
   */
  async webSocketClose(ws: WebSocket, code: number, reason: string): Promise<void> {
    console.log(`[DO] WebSocket closed: ${code} ${reason}`);
    // Session state is preserved in storage for reconnection
  }

  /**
   * Handle WebSocket error
   */
  async webSocketError(ws: WebSocket, error: unknown): Promise<void> {
    console.error("[DO] WebSocket error:", error);
  }

  /**
   * Handle DO alarm for session cleanup
   */
  async alarm(): Promise<void> {
    console.log("[DO] Alarm triggered - checking for expired sessions");
    const now = Date.now();

    // List all session keys and clean up expired ones
    const keys = await this.ctx.storage.list({ prefix: "session:" });
    let hasActiveSessions = false;

    for (const [key, value] of keys) {
      const session = value as SessionState;
      if (now - session.lastActivity > SESSION_TIMEOUT_MS) {
        console.log(`[DO] Cleaning up expired session: ${key}`);
        await this.ctx.storage.delete(key);
      } else {
        hasActiveSessions = true;
      }
    }

    // Reschedule alarm if there are still active sessions
    if (hasActiveSessions) {
      await this.ctx.storage.setAlarm(now + SESSION_TIMEOUT_MS);
    }
  }

  /**
   * Handle init message - start new graph execution
   */
  private async handleInit(ws: WebSocket, graphId: string, input: unknown): Promise<void> {
    // Validate graphId
    if (!graphId || typeof graphId !== "string" || graphId.trim() === "") {
      this.send(ws, { type: "error", message: "Invalid graphId: must be a non-empty string", recoverable: false });
      return;
    }

    // Use the session ID from the URL path (set in fetch())
    // This ensures the sessionId matches the DO routing for reconnection
    const sessionId = this.sessionId;
    if (!sessionId) {
      this.send(ws, { type: "error", message: "Session ID not initialized", recoverable: false });
      return;
    }
    console.log(`[DO] Starting graph execution: sessionId=${sessionId}, graphId=${graphId}`);

    // Load WASM if not already loaded
    if (!this.machine) {
      this.machine = await loadMachine({
        wasmModule: wasmModule as unknown as WebAssembly.Module,
        debug: false,
      });
    }

    // Initialize the graph
    const output = await this.machine.initialize(input);

    // Run until completion or yield
    await this.runGraphLoop(ws, sessionId, graphId, output);
  }

  /**
   * Handle resume message - continue after client-handled effect
   */
  private async handleResume(ws: WebSocket, result: EffectResult): Promise<void> {
    if (!this.sessionId) {
      this.send(ws, { type: "error", message: "No active session to resume", recoverable: false });
      return;
    }

    const sessionKey = `session:${this.sessionId}`;
    const session = await this.ctx.storage.get<SessionState>(sessionKey);

    if (!session) {
      this.send(ws, { type: "error", message: "Session not found or expired", recoverable: false });
      return;
    }

    console.log(`[DO] Resuming session: ${this.sessionId}`);

    // Update last activity
    session.lastActivity = Date.now();
    session.pendingEffect = null;
    await this.ctx.storage.put(sessionKey, session);

    // Always create a fresh machine instance when resuming to avoid
    // stale in-memory state from a previous DO lifecycle
    this.machine = await loadMachine({
      wasmModule: wasmModule as unknown as WebAssembly.Module,
      debug: false,
    });

    const output = await this.machine.step(result);
    await this.runGraphLoop(ws, this.sessionId, session.graphId, output);
  }

  /**
   * Handle reconnect message - restore session from storage
   *
   * Note: The client must reconnect to the same URL path (/session/:sessionId)
   * for routing to reach this DO instance. The sessionId in the message is
   * validated to match the URL-based session.
   */
  private async handleReconnect(ws: WebSocket, sessionId: string): Promise<void> {
    // Validate that the requested sessionId matches this DO's session
    if (sessionId !== this.sessionId) {
      this.send(ws, {
        type: "error",
        message: "Session ID mismatch - reconnect to the correct session URL",
        recoverable: false,
      });
      return;
    }

    const sessionKey = `session:${sessionId}`;
    const session = await this.ctx.storage.get<SessionState>(sessionKey);

    if (!session) {
      this.send(ws, { type: "error", message: "Session not found or expired", recoverable: false });
      return;
    }

    console.log(`[DO] Reconnecting to session: ${sessionId}`);

    // Update last activity
    session.lastActivity = Date.now();
    await this.ctx.storage.put(sessionKey, session);

    // Re-send the pending effect if there is one
    if (session.pendingEffect) {
      this.send(ws, {
        type: "yield",
        effect: session.pendingEffect,
        sessionId,
      });
    } else {
      // No pending effect - session state is inconsistent/corrupted
      this.send(ws, {
        type: "error",
        message: "Session state is inconsistent - cannot resume",
        recoverable: false,
      });
      await this.ctx.storage.delete(sessionKey);
    }
  }

  /**
   * Run graph execution loop until completion or client yield
   */
  private async runGraphLoop(
    ws: WebSocket,
    sessionId: string,
    graphId: string,
    output: Awaited<ReturnType<GraphMachine["initialize"]>>
  ): Promise<void> {
    const sessionKey = `session:${sessionId}`;

    // Effect interpretation loop
    while (!output.done && output.effect) {
      const effect = output.effect;

      // Check if this effect should be yielded to client
      // For now, all effects are handled server-side
      // Future: add client-side effect types here

      // Send progress update
      this.send(ws, {
        type: "progress",
        effect,
        status: "executing",
      });

      // Execute the effect server-side using handler registry
      const result = await executeEffect(effect, this.env);

      // Check if effect failed - yield to client for retry/handling
      if (result.type === "error") {
        // Save session state for potential retry
        const session: SessionState = {
          graphId,
          machineState: null, // WASM state is in memory
          pendingEffect: effect,
          lastActivity: Date.now(),
        };
        await this.ctx.storage.put(sessionKey, session);
        await this.scheduleCleanupAlarm();

        this.send(ws, {
          type: "yield",
          effect,
          sessionId,
        });
        return; // Wait for client to send resume
      }

      // Step with the result
      output = await this.machine!.step(result);
    }

    // Graph completed - clean up session state (but keep sessionId for potential new graph)
    await this.ctx.storage.delete(sessionKey);

    // Check for completion or failure
    if (output.graphState.phase.type === "failed") {
      this.send(ws, {
        type: "error",
        message: output.graphState.phase.error,
        recoverable: false,
      });
    } else {
      this.send(ws, {
        type: "done",
        result: output.stepResult,
      });
    }
  }

  /**
   * Schedule cleanup alarm for session expiry
   */
  private async scheduleCleanupAlarm(): Promise<void> {
    const currentAlarm = await this.ctx.storage.getAlarm();
    if (!currentAlarm) {
      // Set alarm for session timeout
      await this.ctx.storage.setAlarm(Date.now() + SESSION_TIMEOUT_MS);
    }
  }

  /**
   * Helper to send typed messages
   */
  private send(ws: WebSocket, msg: ServerMessage): void {
    ws.send(JSON.stringify(msg));
  }
}

// =============================================================================
// Worker Entry Point
// =============================================================================

export default {
  async fetch(request: Request, env: Env): Promise<Response> {
    const url = new URL(request.url);

    // Health check
    if (url.pathname === "/" || url.pathname === "/health") {
      return new Response(JSON.stringify({ status: "ok", service: "tidepool" }), {
        headers: { "Content-Type": "application/json" },
      });
    }

    // Route to DO: /session/:sessionId
    const match = url.pathname.match(/^\/session\/(.+)$/);
    if (match) {
      const sessionId = match[1];
      const id = env.STATE_MACHINE.idFromName(sessionId);
      const stub = env.STATE_MACHINE.get(id);
      return stub.fetch(request);
    }

    // Create new session: /new
    if (url.pathname === "/new") {
      const sessionId = crypto.randomUUID();
      return new Response(JSON.stringify({ sessionId, wsUrl: `/session/${sessionId}` }), {
        headers: { "Content-Type": "application/json" },
      });
    }

    // Telegram webhook: /telegram
    if (url.pathname === "/telegram" && request.method === "POST") {
      console.log("[Worker] Routing to /telegram webhook");
      return routeWebhook(request, env);
    }

    // Telegram webhook status: /webhook-status
    if (url.pathname === "/webhook-status") {
      console.log("[Worker] Fetching webhook status");
      const info = await fetch(
        `https://api.telegram.org/bot${env.TELEGRAM_TOKEN}/getWebhookInfo`
      );
      return new Response(await info.text(), {
        headers: { "Content-Type": "application/json" },
      });
    }

    // Habitica auth test: /habitica-status
    if (url.pathname === "/habitica-status") {
      console.log("[Worker] Testing Habitica auth");

      if (!env.HABITICA_USER_ID || !env.HABITICA_API_TOKEN) {
        return new Response(JSON.stringify({
          error: "Habitica secrets not configured",
          hint: "Run: wrangler secret put HABITICA_USER_ID && wrangler secret put HABITICA_API_TOKEN"
        }), {
          status: 500,
          headers: { "Content-Type": "application/json" },
        });
      }

      try {
        const resp = await fetch("https://habitica.com/api/v3/user", {
          headers: {
            "x-api-user": env.HABITICA_USER_ID,
            "x-api-key": env.HABITICA_API_TOKEN,
            "x-client": `${env.HABITICA_USER_ID}-tidepool`,
          },
        });

        const data = await resp.json() as {
          success: boolean;
          data?: { stats: { hp: number; mp: number; exp: number; gp: number } };
          message?: string;
        };

        if (!data.success) {
          return new Response(JSON.stringify({
            error: "Habitica API error",
            message: data.message
          }), {
            status: 401,
            headers: { "Content-Type": "application/json" },
          });
        }

        return new Response(JSON.stringify({
          status: "ok",
          stats: {
            hp: data.data?.stats.hp,
            mp: data.data?.stats.mp,
            exp: data.data?.stats.exp,
            gp: data.data?.stats.gp,
          }
        }), {
          headers: { "Content-Type": "application/json" },
        });
      } catch (err) {
        return new Response(JSON.stringify({
          error: "Network error",
          message: err instanceof Error ? err.message : String(err)
        }), {
          status: 500,
          headers: { "Content-Type": "application/json" },
        });
      }
    }

    return new Response("Not found", { status: 404 });
  },
};
