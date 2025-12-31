/**
 * Tidepool Cloudflare Worker - Durable Object Harness
 *
 * Hosts GHC WASM state machines with WebSocket communication.
 */

import { DurableObject } from "cloudflare:workers";
import { loadMachine, type GraphMachine } from "./loader.js";
import type {
  SerializableEffect,
  EffectResult,
  LlmCompleteEffect,
  HabiticaEffect,
} from "./protocol.js";
import { successResult, errorResult } from "./protocol.js";
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
// WebSocket Protocol Types
// =============================================================================

type ClientMessage =
  | { type: "init"; input: unknown }
  | { type: "resume"; result: EffectResult };

type ServerMessage =
  | { type: "progress"; node: string; effect: string }
  | { type: "suspend"; effect: SerializableEffect }
  | { type: "done"; result: unknown }
  | { type: "error"; message: string };

// =============================================================================
// Durable Object: StateMachineDO
// =============================================================================

export class StateMachineDO extends DurableObject<Env> {
  private machine: GraphMachine | null = null;

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

    return new Response(null, { status: 101, webSocket: client });
  }

  /**
   * Handle WebSocket messages
   */
  async webSocketMessage(ws: WebSocket, message: string | ArrayBuffer): Promise<void> {
    if (typeof message !== "string") {
      this.send(ws, { type: "error", message: "Binary messages not supported" });
      return;
    }

    try {
      const msg = JSON.parse(message) as ClientMessage;

      switch (msg.type) {
        case "init":
          await this.runGraph(ws, msg.input);
          break;

        case "resume":
          // Future: handle suspension resume
          this.send(ws, { type: "error", message: "Resume not yet implemented" });
          break;

        default:
          this.send(ws, { type: "error", message: `Unknown message type: ${(msg as { type: string }).type}` });
      }
    } catch (err) {
      this.send(ws, {
        type: "error",
        message: err instanceof Error ? err.message : String(err),
      });
    }
  }

  /**
   * Handle WebSocket close
   */
  async webSocketClose(ws: WebSocket, code: number, reason: string): Promise<void> {
    console.log(`[DO] WebSocket closed: ${code} ${reason}`);
  }

  /**
   * Handle WebSocket error
   */
  async webSocketError(ws: WebSocket, error: unknown): Promise<void> {
    console.error("[DO] WebSocket error:", error);
  }

  /**
   * Run graph to completion
   */
  private async runGraph(ws: WebSocket, input: unknown): Promise<void> {
    try {
      // Load WASM if not already loaded
      if (!this.machine) {
        this.machine = await loadMachine({
          wasmModule: wasmModule as unknown as WebAssembly.Module,
          debug: false,
        });
      }

      // Initialize the graph
      let output = await this.machine.initialize(input);

      // Effect interpretation loop
      while (!output.done && output.effect) {
        const effect = output.effect;
        const nodeName = output.graphState.phase.type === "in_node"
          ? output.graphState.phase.nodeName
          : "unknown";

        // Send progress update
        this.send(ws, {
          type: "progress",
          node: nodeName,
          effect: effect.type,
        });

        // Execute the effect
        const result = await this.executeEffect(effect);

        // Step with the result
        output = await this.machine.step(result);
      }

      // Check for completion or failure
      if (output.graphState.phase.type === "failed") {
        this.send(ws, {
          type: "error",
          message: output.graphState.phase.error,
        });
      } else {
        this.send(ws, {
          type: "done",
          result: output.stepResult,
        });
      }
    } catch (err) {
      this.send(ws, {
        type: "error",
        message: err instanceof Error ? err.message : String(err),
      });
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
