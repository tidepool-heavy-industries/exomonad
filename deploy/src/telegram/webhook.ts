/**
 * Telegram webhook handler.
 *
 * Verifies webhook secret and routes updates to TelegramDO.
 */

import type { TelegramUpdate, TelegramEnv } from "./types.js";
import { extractChatId } from "./types.js";
import type { TelegramDO } from "./do.js";

/**
 * Extended environment with TelegramDO binding.
 */
export interface WebhookEnv extends TelegramEnv {
  TELEGRAM_DO: DurableObjectNamespace<TelegramDO>;
}

/**
 * Constant-time string comparison to prevent timing attacks.
 * Returns true if strings are equal.
 */
function timingSafeEqual(a: string, b: string): boolean {
  // Use longer string length to prevent length-based timing leaks
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
 * Verify the webhook secret header.
 *
 * Telegram sends the secret in X-Telegram-Bot-Api-Secret-Token header.
 * Uses constant-time comparison to prevent timing attacks.
 * See: https://core.telegram.org/bots/api#setwebhook
 */
export function verifyWebhookSecret(
  request: Request,
  expectedSecret: string
): boolean {
  const header = request.headers.get("X-Telegram-Bot-Api-Secret-Token");
  if (!header) return false;
  return timingSafeEqual(header, expectedSecret);
}

/**
 * Route a webhook update to the appropriate TelegramDO.
 *
 * Each chat_id gets its own Durable Object instance.
 * Returns 200 OK to prevent Telegram retries on errors.
 */
export async function routeWebhook(
  request: Request,
  env: WebhookEnv
): Promise<Response> {
  console.log("[Webhook] Received request");

  // Verify webhook secret
  const secretValid = verifyWebhookSecret(request, env.TELEGRAM_WEBHOOK_SECRET);
  console.log(`[Webhook] Secret verification: ${secretValid ? "PASS" : "FAIL"}`);
  if (!secretValid) {
    console.error("[Webhook] Invalid webhook secret");
    return new Response("Unauthorized", { status: 401 });
  }

  let update: TelegramUpdate;
  try {
    update = (await request.json()) as TelegramUpdate;
    console.log(`[Webhook] Parsed update: ${JSON.stringify(update).slice(0, 500)}`);
  } catch (error) {
    // Bad JSON from Telegram (shouldn't happen, but handle it)
    console.error("[Webhook] Failed to parse JSON:", error);
    return new Response("Bad Request", { status: 400 });
  }

  try {
    // Extract chat_id for DO routing
    const chatId = extractChatId(update);
    console.log(`[Webhook] Extracted chat_id: ${chatId}`);
    if (!chatId) {
      console.log("[Webhook] No chat_id in update, ignoring");
      return new Response("OK");
    }

    // Route to TelegramDO keyed by chat_id
    const doName = `chat:${chatId}`;
    console.log(`[Webhook] Routing to DO: ${doName}`);
    const doId = env.TELEGRAM_DO.idFromName(doName);
    const agent = env.TELEGRAM_DO.get(doId);

    // Forward the update to the DO
    console.log("[Webhook] Forwarding to TelegramDO...");
    const doResponse = await agent.fetch(
      new Request("https://internal/update", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(update),
      })
    );

    console.log(`[Webhook] TelegramDO response: ${doResponse.status}`);
    if (!doResponse.ok) {
      console.error(`[Webhook] TelegramDO error: ${doResponse.status} ${doResponse.statusText}`);
    }

    return new Response("OK");
  } catch (error) {
    console.error("[Webhook] Error:", error);
    // Return 200 to prevent Telegram retries on internal errors
    return new Response("OK");
  }
}
