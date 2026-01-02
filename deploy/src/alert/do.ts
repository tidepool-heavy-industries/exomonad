/**
 * Alert Durable Object.
 *
 * Handles alert deduplication with 15-minute cooldown per fingerprint.
 *
 * Architecture:
 * - One AlertDO per alert fingerprint
 * - Manages cooldown to prevent alert fatigue
 * - Routes non-duplicate alerts to graph execution
 *
 * Design decisions:
 * - Each alert execution creates a new StateMachineDO session (timestamp in ID)
 *   because alert handling is stateless - we don't need to resume or query
 *   previous alert executions, and this avoids DO state conflicts.
 * - The "alert" graph ID is used here; the graph itself will be registered
 *   in tidepool-wasm/src/Tidepool/Wasm/Registry.hs in a follow-up PR.
 */

import { DurableObject } from "cloudflare:workers";
import { z } from "zod";
import type { GrafanaAlert } from "./types.js";
import { extractChatId, toAlertInput } from "./types.js";

/**
 * Cooldown period in milliseconds (15 minutes).
 */
const COOLDOWN_MS = 15 * 60 * 1000;

/**
 * Environment bindings for AlertDO.
 */
export interface AlertDOEnv {
  STATE_MACHINE: DurableObjectNamespace;
}

// =============================================================================
// Zod Schemas - Runtime validation at storage boundary
// =============================================================================

/**
 * Alert state schema with defaults for missing fields.
 *
 * This is the source of truth for state shape. When loaded from storage:
 * - Missing fields get defaults (schema evolution)
 * - Invalid data throws (corruption detection)
 * - Type safety is enforced at runtime, not just compile time
 */
const AlertStateSchema = z.object({
  fingerprint: z.string().default(""),
  lastFiredAt: z.number().default(0),
  lastStatus: z.enum(["firing", "resolved"]).default("resolved"),
});

type AlertState = z.infer<typeof AlertStateSchema>;

/**
 * Parse storage data into validated state.
 * Returns default state if nothing stored, validates and migrates if stored.
 */
function parseState(stored: unknown): AlertState {
  if (stored === undefined || stored === null) {
    return AlertStateSchema.parse({});
  }
  return AlertStateSchema.parse(stored);
}

/**
 * Durable Object for handling alert deduplication.
 *
 * One instance per alert fingerprint, identified by "alert:{fingerprint}" name.
 */
export class AlertDO extends DurableObject<AlertDOEnv> {
  private state: AlertState | null = null;

  /**
   * Load state from storage.
   *
   * Always returns a valid AlertState (with defaults if nothing stored).
   * Uses Zod schema for validation and schema evolution.
   */
  private async loadState(): Promise<AlertState> {
    if (this.state) return this.state;
    const stored = await this.ctx.storage.get("state");
    this.state = parseState(stored);
    return this.state;
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
   * Handle incoming requests from webhook router.
   */
  async fetch(request: Request): Promise<Response> {
    const url = new URL(request.url);
    console.log(`[AlertDO] fetch() called with path: ${url.pathname}`);

    switch (url.pathname) {
      case "/alert": {
        if (request.method !== "POST") {
          return new Response("Method not allowed", { status: 405 });
        }
        const alert = (await request.json()) as GrafanaAlert;
        return this.handleAlert(alert);
      }

      case "/status": {
        const state = await this.loadState();
        return Response.json({
          fingerprint: state.fingerprint,
          lastFiredAt: state.lastFiredAt,
          lastStatus: state.lastStatus,
          cooldownRemaining: Math.max(0, COOLDOWN_MS - (Date.now() - state.lastFiredAt)),
        });
      }

      default:
        return new Response("Not found", { status: 404 });
    }
  }

  /**
   * Process an incoming alert.
   *
   * - Resolved alerts always pass through (to notify resolution)
   * - Firing alerts are deduplicated with 15-minute cooldown
   * - Cooldown applies regardless of lastStatus to rate-limit alert-resolve-alert loops
   */
  private async handleAlert(alert: GrafanaAlert): Promise<Response> {
    const now = Date.now();
    const state = await this.loadState();

    console.log(
      `[AlertDO] Processing alert: fingerprint=${alert.fingerprint}, status=${alert.status}`
    );

    // Extract chat_id for routing
    const chatId = extractChatId(alert);
    if (!chatId) {
      console.warn(`[AlertDO] Alert missing chat_id, cannot route`);
      return new Response("Missing chat_id", { status: 400 });
    }

    // Resolved alerts always pass through (to notify resolution).
    // We preserve lastFiredAt to maintain cooldown for subsequent firing alerts.
    if (alert.status === "resolved") {
      console.log(`[AlertDO] Resolved alert, passing through`);
      try {
        await this.routeToGraph(alert, chatId);
        // Only update state after successful routing
        // Keep lastFiredAt to rate-limit alert-resolve-alert loops
        this.state = {
          fingerprint: alert.fingerprint,
          lastFiredAt: state.lastFiredAt,
          lastStatus: "resolved",
        };
        await this.saveState();
        return new Response("OK");
      } catch (err) {
        const message = err instanceof Error ? err.message : String(err);
        return new Response(`Graph execution failed: ${message}`, { status: 502 });
      }
    }

    // Check cooldown for firing alerts (regardless of lastStatus).
    // This rate-limits alert-resolve-alert loops where the same alert
    // fires, resolves, and fires again rapidly.
    const elapsed = now - state.lastFiredAt;
    if (state.lastFiredAt > 0 && elapsed < COOLDOWN_MS) {
      const remaining = Math.ceil((COOLDOWN_MS - elapsed) / 1000);
      console.log(
        `[AlertDO] Alert in cooldown, ${remaining}s remaining, skipping`
      );
      return Response.json({
        status: "deduplicated",
        cooldownRemaining: remaining,
      });
    }

    // Route alert, then update state only on success
    console.log(`[AlertDO] Alert passed cooldown check, routing to graph`);
    try {
      await this.routeToGraph(alert, chatId);
      // Only update state after successful routing
      this.state = {
        fingerprint: alert.fingerprint,
        lastFiredAt: now,
        lastStatus: "firing",
      };
      await this.saveState();
      return new Response("OK");
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      return new Response(`Graph execution failed: ${message}`, { status: 502 });
    }
  }

  /**
   * Route alert to StateMachineDO for graph execution.
   *
   * Throws on failure so callers can return appropriate error responses.
   * This ensures transient failures don't silently lose alerts.
   */
  private async routeToGraph(
    alert: GrafanaAlert,
    chatId: number
  ): Promise<void> {
    const alertInput = toAlertInput(alert, chatId);
    // Timestamp in session ID: each alert execution is independent (see design decisions)
    const sessionId = `alert-${alert.fingerprint}-${Date.now()}`;

    console.log(`[AlertDO] Starting graph session: ${sessionId}`);

    const doId = this.env.STATE_MACHINE.idFromName(sessionId);
    const stub = this.env.STATE_MACHINE.get(doId);

    const response = await stub.fetch(
      new Request("https://do/start", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          graphId: "alert",
          input: alertInput,
          telegramChatId: chatId,
        }),
      })
    );

    if (!response.ok) {
      const errorText = await response.text();
      console.error(`[AlertDO] Graph start failed: ${response.status} ${errorText}`);
      throw new Error(`Graph start failed: ${response.status} ${errorText}`);
    }

    const result = await response.json();
    console.log(`[AlertDO] Graph result:`, result);
  }
}
