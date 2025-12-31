/**
 * Telegram effect types for Tidepool agents.
 *
 * This package provides TypeScript types that mirror the Haskell
 * Telegram effect DSL, enabling type-safe communication between
 * agent logic (compiled to WASM) and the TypeScript runtime.
 *
 * @example
 * ```typescript
 * import {
 *   TelegramEffect,
 *   TelegramEffectResult,
 *   isSendEffect,
 *   isReceiveEffect,
 *   unitResult,
 *   messagesResult,
 *   firstText
 * } from 'tidepool-telegram';
 *
 * async function handleEffect(effect: TelegramEffect): Promise<TelegramEffectResult> {
 *   if (isSendEffect(effect)) {
 *     await sendToTelegram(effect.message);
 *     return unitResult();
 *   }
 *   if (isReceiveEffect(effect)) {
 *     const messages = await waitForMessages();
 *     return messagesResult(messages);
 *   }
 *   // ... etc
 * }
 * ```
 *
 * @packageDocumentation
 */

// Types
export type {
  MediaHandle,
  InlineButton,
  OutgoingMessage,
  IncomingMessage,
} from './types.js';

export {
  // Type guards
  isTextReply,
  isPhotoReply,
  isDocReply,
  isButtonClick,
  // Extractors
  firstText,
  firstClick,
  firstMedia,
  allTexts,
  allClicks,
  allMedia,
} from './types.js';

// Effects
export type {
  TelegramEffect,
  SendEffect,
  ReceiveEffect,
  TryReceiveEffect,
  TelegramEffectResult,
  UnitResult,
  MessagesResult,
} from './effects.js';

export {
  // Result constructors
  unitResult,
  messagesResult,
  // Type guards
  isSendEffect,
  isReceiveEffect,
  isTryReceiveEffect,
} from './effects.js';
