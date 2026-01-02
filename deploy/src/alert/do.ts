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

/**
 * Alert state stored in DO storage.
 */
interface AlertState {
  fingerprint: string;
  lastFiredAt: number;
  lastStatus: "firing" | "resolved";
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
   */
  private async loadState(): Promise<AlertState | null> {
    if (this.state) return this.state;
    const stored = await this.ctx.storage.get<AlertState>("state");
    if (stored) {
      this.state = stored;
    }
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
          fingerprint: state?.fingerprint ?? null,
          lastFiredAt: state?.lastFiredAt ?? null,
          lastStatus: state?.lastStatus ?? null,
          cooldownRemaining: state
            ? Math.max(0, COOLDOWN_MS - (Date.now() - state.lastFiredAt))
            : 0,
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

    // Resolved alerts always pass through and reset the cooldown timer.
    // This ensures the next firing alert isn't blocked by a stale timestamp.
    if (alert.status === "resolved") {
      console.log(`[AlertDO] Resolved alert, passing through and resetting cooldown`);
      try {
        await this.routeToGraph(alert, chatId);
        // Only update state after successful routing
        this.state = {
          fingerprint: alert.fingerprint,
          lastFiredAt: 0, // Reset so next firing alert isn't incorrectly blocked
          lastStatus: "resolved",
        };
        await this.saveState();
        return new Response("OK");
      } catch (err) {
        const message = err instanceof Error ? err.message : String(err);
        return new Response(`Graph execution failed: ${message}`, { status: 502 });
      }
    }

    // Check cooldown for firing alerts
    if (state && state.lastStatus === "firing") {
      const elapsed = now - state.lastFiredAt;
      if (elapsed < COOLDOWN_MS) {
        const remaining = Math.ceil((COOLDOWN_MS - elapsed) / 1000);
        console.log(
          `[AlertDO] Alert in cooldown, ${remaining}s remaining, skipping`
        );
        return Response.json({
          status: "deduplicated",
          cooldownRemaining: remaining,
        });
      }
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
