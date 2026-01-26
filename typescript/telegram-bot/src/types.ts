/**
 * Core types for the Telegram effect DSL.
 *
 * These types mirror the Haskell types in ExoMonad.Telegram.Types
 * and define the boundary between agent logic and the Telegram runtime.
 */

/**
 * Opaque handle for media files.
 *
 * The runtime interprets these handles - agent code just passes them around.
 * Handles are obtained from incoming media and can be used to send media.
 */
export type MediaHandle = string;

/**
 * Inline keyboard button with callback data.
 *
 * When pressed, the button's callback data is returned to the agent.
 * Only callback buttons are supported (no URL/login buttons).
 */
export interface InlineButton {
  /** Display text on the button */
  text: string;
  /** JSON payload returned on click */
  data: unknown;
}

/**
 * Outgoing messages that can be sent to the user.
 *
 * Mirrors Haskell: OutgoingMessage
 */
export type OutgoingMessage =
  | { type: 'text'; text: string }
  | { type: 'photo'; media: MediaHandle; caption?: string }
  | { type: 'document'; media: MediaHandle; filename: string }
  | { type: 'buttons'; text: string; buttons: InlineButton[][] };

/**
 * Incoming messages received from the user.
 *
 * Each variant is distinct to allow pattern matching on the type of response.
 *
 * Mirrors Haskell: IncomingMessage
 */
export type IncomingMessage =
  | { type: 'text'; text: string }
  | { type: 'photo'; media: MediaHandle; caption?: string }
  | { type: 'document'; media: MediaHandle; filename: string }
  | { type: 'button_click'; data: unknown };

// ══════════════════════════════════════════════════════════════
// TYPE GUARDS
// ══════════════════════════════════════════════════════════════

export function isTextReply(msg: IncomingMessage): msg is { type: 'text'; text: string } {
  return msg.type === 'text';
}

export function isPhotoReply(msg: IncomingMessage): msg is { type: 'photo'; media: MediaHandle; caption?: string } {
  return msg.type === 'photo';
}

export function isDocReply(msg: IncomingMessage): msg is { type: 'document'; media: MediaHandle; filename: string } {
  return msg.type === 'document';
}

export function isButtonClick(msg: IncomingMessage): msg is { type: 'button_click'; data: unknown } {
  return msg.type === 'button_click';
}

// ══════════════════════════════════════════════════════════════
// EXTRACTORS
// ══════════════════════════════════════════════════════════════

/** Extract the first text reply from messages. */
export function firstText(messages: IncomingMessage[]): string | undefined {
  const textMsg = messages.find(isTextReply);
  return textMsg?.text;
}

/** Extract the first button click from messages. */
export function firstClick(messages: IncomingMessage[]): unknown | undefined {
  const clickMsg = messages.find(isButtonClick);
  return clickMsg?.data;
}

/** Extract the first media handle from messages. */
export function firstMedia(messages: IncomingMessage[]): MediaHandle | undefined {
  for (const msg of messages) {
    if (msg.type === 'photo' || msg.type === 'document') {
      return msg.media;
    }
  }
  return undefined;
}

/** Extract all text replies from messages. */
export function allTexts(messages: IncomingMessage[]): string[] {
  return messages.filter(isTextReply).map(m => m.text);
}

/** Extract all button clicks from messages. */
export function allClicks(messages: IncomingMessage[]): unknown[] {
  return messages.filter(isButtonClick).map(m => m.data);
}

/** Extract all media handles from messages. */
export function allMedia(messages: IncomingMessage[]): MediaHandle[] {
  const result: MediaHandle[] = [];
  for (const msg of messages) {
    if (msg.type === 'photo' || msg.type === 'document') {
      result.push(msg.media);
    }
  }
  return result;
}
