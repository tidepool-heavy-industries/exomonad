/**
 * Alert webhook handler.
 *
 * Receives Grafana Alertmanager webhook payloads and routes to AlertDO.
 */

import type { GrafanaWebhookPayload } from "./types.js";
import { extractChatId } from "./types.js";
import type { AlertDO } from "./do.js";

/**
 * Environment with AlertDO binding.
 */
export interface AlertWebhookEnv {
  ALERT_DO: DurableObjectNamespace<AlertDO>;
  ALERT_WEBHOOK_SECRET?: string;
}

/**
 * Constant-time string comparison to prevent timing attacks.
 */
function timingSafeEqual(a: string, b: string): boolean {
  const len = Math.max(a.length, b.length);
  let result = a.length !== b.length ? 1 : 0;
  for (let i = 0; i < len; i++) {
    const charA = i < a.length ? a.charCodeAt(i) : 0;
    const charB = i < b.length ? b.charCodeAt(i) : 0;
    result |= charA ^ charB;
  }
  return result === 0;
}

/**
 * Verify optional webhook secret.
 * If no secret is configured, allow all requests.
 */
function verifyWebhookSecret(
  request: Request,
  expectedSecret: string | undefined
): boolean {
  if (!expectedSecret) return true;
  const header = request.headers.get("X-Webhook-Secret");
  if (!header) return false;
  return timingSafeEqual(header, expectedSecret);
}

/**
 * Route alert webhook to appropriate AlertDO instances.
 *
 * Each unique alert fingerprint gets its own DO.
 * Alerts without chat_id are logged but ignored.
 */
export async function routeAlertWebhook(
  request: Request,
  env: AlertWebhookEnv
): Promise<Response> {
  console.log("[AlertWebhook] Received request");

  // Optional secret verification
  if (!verifyWebhookSecret(request, env.ALERT_WEBHOOK_SECRET)) {
    console.error("[AlertWebhook] Invalid webhook secret");
    return new Response("Unauthorized", { status: 401 });
  }

  let payload: GrafanaWebhookPayload;
  try {
    payload = (await request.json()) as GrafanaWebhookPayload;
    console.log(
      `[AlertWebhook] Parsed payload: ${payload.alerts.length} alerts, status=${payload.status}`
    );
  } catch (error) {
    console.error("[AlertWebhook] Failed to parse JSON:", error);
    return new Response("Bad Request", { status: 400 });
  }

  // Process each alert in parallel
  const results = await Promise.allSettled(
    payload.alerts.map(async (alert) => {
      const chatId = extractChatId(alert);
      if (!chatId) {
        console.warn(
          `[AlertWebhook] Alert ${alert.fingerprint} missing chat_id, skipping`
        );
        return;
      }

      // Route to AlertDO keyed by fingerprint
      const doName = `alert:${alert.fingerprint}`;
      console.log(`[AlertWebhook] Routing to DO: ${doName}`);
      const doId = env.ALERT_DO.idFromName(doName);
      const stub = env.ALERT_DO.get(doId);

      await stub.fetch(
        new Request("https://internal/alert", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(alert),
        })
      );
    })
  );

  // Log any failures but don't fail the webhook
  const failures = results.filter((r) => r.status === "rejected");
  if (failures.length > 0) {
    console.error(`[AlertWebhook] ${failures.length} alerts failed to route`);
  }

  return new Response("OK");
}
