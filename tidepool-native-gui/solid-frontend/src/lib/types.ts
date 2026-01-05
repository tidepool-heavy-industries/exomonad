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
  choices: ChoiceConfig | null;
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

/** Choice option with rich metadata. */
export interface ChoiceOption {
  index: number;
  label: string;
  description?: string;
  costs: string[];
  disabled?: string; // reason if disabled
}

/** Choice configuration with multi-select support. */
export interface ChoiceConfig {
  prompt: string;
  options: ChoiceOption[];
  multiSelect: boolean;
}

/** @deprecated Use ChoiceOption instead */
export type ButtonConfig = ChoiceOption;

// ============================================================================
// UserAction (Client -> Server)
// ============================================================================

export type UserAction = TextAction | ChoiceAction | MultiChoiceAction | PhotoAction;

export interface TextAction {
  type: "text";
  content: string;
}

export interface ChoiceAction {
  type: "choice";
  index: number;
}

export interface MultiChoiceAction {
  type: "multiChoice";
  indices: number[];
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

export function choiceAction(index: number): ChoiceAction {
  return { type: "choice", index };
}

export function multiChoiceAction(indices: number[]): MultiChoiceAction {
  return { type: "multiChoice", indices };
}

export function photoAction(data: string, mimeType: string): PhotoAction {
  return { type: "photo", data, mimeType };
}

/** @deprecated Use choiceAction instead */
export function buttonAction(id: string): ChoiceAction {
  return { type: "choice", index: parseInt(id, 10) };
}
