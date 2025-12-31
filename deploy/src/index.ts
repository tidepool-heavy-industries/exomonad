/**
 * Tidepool Cloudflare Worker - Durable Object Harness
 *
 * Hosts GHC WASM state machines with WebSocket communication.
 * See docs/PROTOCOL.md for the WebSocket protocol specification.
 */

import { DurableObject } from "cloudflare:workers";
import { loadMachine, type GraphMachine } from "./loader.js";
import type {
  SerializableEffect,
  EffectResult,
  LlmCompleteEffect,
  HabiticaEffect,
  ClientMessage,
  ServerMessage,
  SessionState,
} from "./protocol.js";
import { successResult, errorResult, SESSION_TIMEOUT_MS } from "./protocol.js";
import { handleHabitica, type HabiticaConfig } from "./handlers/habitica.js";

// Import WASM module at build time
import wasmModule from "./tidepool.wasm";

// =============================================================================
// Environment Types
// =============================================================================

export interface Env {
  STATE_MACHINE: DurableObjectNamespace<StateMachineDO>;
  AI: Ai;
  // Habitica API credentials (set via wrangler secret)
  HABITICA_USER_ID: string;
  HABITICA_API_TOKEN: string;
}

// =============================================================================
// Durable Object: StateMachineDO
// =============================================================================

export class StateMachineDO extends DurableObject<Env> {
  private machine: GraphMachine | null = null;
  private currentSessionId: string | null = null;

  /**
   * Handle incoming requests - expect WebSocket upgrade
   */
  async fetch(request: Request): Promise<Response> {
    const upgradeHeader = request.headers.get("Upgrade");
    if (upgradeHeader !== "websocket") {
      return new Response("Expected WebSocket upgrade", { status: 426 });
    }

    const pair = new WebSocketPair();
    const [client, server] = Object.values(pair);

    // Accept the WebSocket connection using Hibernation API
    this.ctx.acceptWebSocket(server);
    console.log("[DO] WebSocket connection accepted");

    return new Response(null, { status: 101, webSocket: client });
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
      const sessionId = this.currentSessionId;
      this.send(ws, {
        type: "error",
        message: err instanceof Error ? err.message : String(err),
        recoverable: sessionId !== null,
        sessionId: sessionId ?? undefined,
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
    for (const [key, value] of keys) {
      const session = value as SessionState;
      if (now - session.lastActivity > SESSION_TIMEOUT_MS) {
        console.log(`[DO] Cleaning up expired session: ${key}`);
        await this.ctx.storage.delete(key);
      }
    }
  }

  /**
   * Handle init message - start new graph execution
   */
  private async handleInit(ws: WebSocket, graphId: string, input: unknown): Promise<void> {
    // Generate new session ID
    const sessionId = crypto.randomUUID();
    this.currentSessionId = sessionId;
    console.log(`[DO] Starting new session: ${sessionId}, graphId: ${graphId}`);

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
    if (!this.currentSessionId) {
      this.send(ws, { type: "error", message: "No active session to resume", recoverable: false });
      return;
    }

    const sessionKey = `session:${this.currentSessionId}`;
    const session = await this.ctx.storage.get<SessionState>(sessionKey);

    if (!session) {
      this.send(ws, { type: "error", message: "Session not found or expired", recoverable: false });
      return;
    }

    console.log(`[DO] Resuming session: ${this.currentSessionId}`);

    // Update last activity
    session.lastActivity = Date.now();
    session.pendingEffect = null;
    await this.ctx.storage.put(sessionKey, session);

    // Continue graph execution with the result
    if (!this.machine) {
      this.machine = await loadMachine({
        wasmModule: wasmModule as unknown as WebAssembly.Module,
        debug: false,
      });
    }

    const output = await this.machine.step(result);
    await this.runGraphLoop(ws, this.currentSessionId, session.graphId, output);
  }

  /**
   * Handle reconnect message - restore session from storage
   */
  private async handleReconnect(ws: WebSocket, sessionId: string): Promise<void> {
    const sessionKey = `session:${sessionId}`;
    const session = await this.ctx.storage.get<SessionState>(sessionKey);

    if (!session) {
      this.send(ws, { type: "error", message: "Session not found or expired", recoverable: false });
      return;
    }

    console.log(`[DO] Reconnecting to session: ${sessionId}`);
    this.currentSessionId = sessionId;

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
      // No pending effect - session might have been interrupted during server-side processing
      this.send(ws, {
        type: "error",
        message: "Session has no pending effect - cannot resume",
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

      // Execute the effect server-side
      const result = await this.executeEffect(effect);

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

    // Graph completed - clean up session
    await this.ctx.storage.delete(sessionKey);
    this.currentSessionId = null;

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
   * Execute an effect and return the result
   */
  private async executeEffect(effect: SerializableEffect): Promise<EffectResult> {
    try {
      switch (effect.type) {
        case "LlmComplete":
          return await this.callCfAi(effect);

        case "HttpFetch": {
          const resp = await fetch(effect.eff_url, { method: effect.eff_method });
          const contentType = resp.headers.get("content-type") ?? "";
          let body: unknown;

          if (contentType.includes("application/json")) {
            body = await resp.json();
          } else {
            body = await resp.text();
          }

          return successResult({ status: resp.status, body });
        }

        case "LogInfo":
          console.log(`[Graph Log] ${effect.eff_message}`);
          return successResult(null);

        case "LogError":
          console.error(`[Graph Error] ${effect.eff_message}`);
          return successResult(null);

        case "Habitica": {
          const config: HabiticaConfig = {
            userId: this.env.HABITICA_USER_ID,
            apiToken: this.env.HABITICA_API_TOKEN,
          };
          return await handleHabitica(effect as HabiticaEffect, config);
        }

        default:
          return errorResult(`Unknown effect type: ${(effect as { type: string }).type}`);
      }
    } catch (err) {
      return errorResult(err instanceof Error ? err.message : String(err));
    }
  }

  /**
   * Call Cloudflare AI for LLM completion
   */
  private async callCfAi(effect: LlmCompleteEffect): Promise<EffectResult> {
    const messages: Array<{ role: "system" | "user" | "assistant"; content: string }> = [
      { role: "system", content: effect.eff_system_prompt },
      { role: "user", content: effect.eff_user_content },
    ];

    // Build request options
    const options: Record<string, unknown> = {
      messages,
      max_tokens: 2048,
    };

    // Use JSON schema mode if schema provided
    if (effect.eff_schema) {
      options.response_format = {
        type: "json_schema",
        json_schema: {
          name: "effect_output",
          strict: true,
          schema: effect.eff_schema,
        },
      };
    }

    const response = await this.env.AI.run(
      "@cf/meta/llama-3.3-70b-instruct-fp8-fast",
      options
    ) as { response?: string };

    // Handle response
    let output: unknown = {};

    if (typeof response.response === "object" && response.response !== null) {
      // JSON schema mode returned parsed object directly
      output = response.response;
    } else if (typeof response.response === "string") {
      // Parse JSON from string response
      let jsonStr = response.response.trim();

      // Strip markdown code blocks if present
      if (jsonStr.startsWith("```json")) {
        jsonStr = jsonStr.slice(7);
      } else if (jsonStr.startsWith("```")) {
        jsonStr = jsonStr.slice(3);
      }
      if (jsonStr.endsWith("```")) {
        jsonStr = jsonStr.slice(0, -3);
      }
      jsonStr = jsonStr.trim();

      try {
        output = JSON.parse(jsonStr);
      } catch {
        // Return raw text if JSON parse fails
        output = { text: response.response };
      }
    }

    return successResult(output);
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

    return new Response("Not found", { status: 404 });
  },
};
