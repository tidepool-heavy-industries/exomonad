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
 * Verify the webhook secret header.
 *
 * Telegram sends the secret in X-Telegram-Bot-Api-Secret-Token header.
 * See: https://core.telegram.org/bots/api#setwebhook
 */
export function verifyWebhookSecret(
  request: Request,
  expectedSecret: string
): boolean {
  const header = request.headers.get("X-Telegram-Bot-Api-Secret-Token");
  return header === expectedSecret;
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
  // Verify webhook secret
  if (!verifyWebhookSecret(request, env.TELEGRAM_WEBHOOK_SECRET)) {
    console.error("Invalid webhook secret");
    return new Response("Unauthorized", { status: 401 });
  }

  try {
    const update = (await request.json()) as TelegramUpdate;

    // Extract chat_id for DO routing
    const chatId = extractChatId(update);
    if (!chatId) {
      console.log("No chat_id in update, ignoring");
      return new Response("OK");
    }

    // Route to TelegramDO keyed by chat_id
    const doId = env.TELEGRAM_DO.idFromName(`chat:${chatId}`);
    const agent = env.TELEGRAM_DO.get(doId);

    // Forward the update to the DO
    await agent.fetch(
      new Request("https://internal/update", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(update),
      })
    );

    return new Response("OK");
  } catch (error) {
    console.error("Webhook error:", error);
    // Return 200 to prevent Telegram retries
    return new Response("OK");
  }
}
