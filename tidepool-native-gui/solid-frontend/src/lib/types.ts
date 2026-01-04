/**
 * Wire format types for tidepool native GUI.
 * These definitions match the Haskell types in wire-types/src/Tidepool/Wire/Types.hs.
 */

// ============================================================================
// UIState (Server -> Client)
// ============================================================================

export interface UIState {
  messages: ChatMessage[];
  textInput: TextInputConfig | null;
  photoUpload: PhotoUploadConfig | null;
  buttons: ButtonConfig[] | null;
  graphNode: string;
  thinking: boolean;
}

export interface ChatMessage {
  role: MessageRole;
  content: string;
  timestamp: string;
}

export type MessageRole = "user" | "assistant" | "system";

export interface TextInputConfig {
  placeholder: string;
}

export interface PhotoUploadConfig {
  prompt: string;
}

export interface ButtonConfig {
  id: string;
  label: string;
}

// ============================================================================
// UserAction (Client -> Server)
// ============================================================================

export type UserAction = TextAction | ButtonAction | PhotoAction;

export interface TextAction {
  type: "text";
  content: string;
}

export interface ButtonAction {
  type: "button";
  id: string;
}

export interface PhotoAction {
  type: "photo";
  data: string;
  mimeType: string;
}

// ============================================================================
// Factory Functions
// ============================================================================

export function textAction(content: string): TextAction {
  return { type: "text", content };
}

export function buttonAction(id: string): ButtonAction {
  return { type: "button", id };
}

export function photoAction(data: string, mimeType: string): PhotoAction {
  return { type: "photo", data, mimeType };
}
