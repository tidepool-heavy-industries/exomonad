/**
 * Wire format types for tidepool native GUI.
 *
 * These TypeScript definitions match the Haskell types in src/Tidepool/Wire/Types.hs.
 * See PROTOCOL.md for WebSocket lifecycle documentation.
 */

// ============================================================================
// UIState (Server → Client)
// ============================================================================

/**
 * UI state sent from server to client after each graph step.
 * The frontend renders this state directly - no business logic required.
 */
export interface UIState {
  /** Conversation history */
  messages: ChatMessage[];

  /** Text input configuration, null if text input disabled */
  textInput: TextInputConfig | null;

  /** Photo upload configuration, null if photo upload disabled */
  photoUpload: PhotoUploadConfig | null;

  /** Available buttons, null if no buttons shown */
  buttons: ButtonConfig[] | null;

  /** Current graph node (for debugging/observability) */
  graphNode: string;

  /** Whether the agent is currently processing */
  thinking: boolean;
}

/**
 * Chat message in conversation history.
 */
export interface ChatMessage {
  role: MessageRole;
  content: string;
  /** ISO 8601 timestamp */
  timestamp: string;
}

/**
 * Message role - matches LLM conversation roles.
 */
export type MessageRole = "user" | "assistant" | "system";

/**
 * Text input configuration.
 */
export interface TextInputConfig {
  placeholder: string;
}

/**
 * Photo upload configuration.
 */
export interface PhotoUploadConfig {
  prompt: string;
}

/**
 * Button configuration.
 */
export interface ButtonConfig {
  id: string;
  label: string;
}

// ============================================================================
// UserAction (Client → Server)
// ============================================================================

/**
 * User action sent from client to server.
 * Discriminated union on the `type` field.
 */
export type UserAction = TextAction | ButtonAction | PhotoAction;

/**
 * Text input action.
 */
export interface TextAction {
  type: "text";
  content: string;
}

/**
 * Button click action.
 */
export interface ButtonAction {
  type: "button";
  id: string;
}

/**
 * Photo upload action.
 */
export interface PhotoAction {
  type: "photo";
  /** Base64-encoded image data */
  data: string;
  /** MIME type (e.g., "image/jpeg", "image/png") */
  mimeType: string;
}

// ============================================================================
// Type Guards
// ============================================================================

export function isTextAction(action: UserAction): action is TextAction {
  return action.type === "text";
}

export function isButtonAction(action: UserAction): action is ButtonAction {
  return action.type === "button";
}

export function isPhotoAction(action: UserAction): action is PhotoAction {
  return action.type === "photo";
}

// ============================================================================
// Factory Functions
// ============================================================================

/**
 * Create a text action.
 */
export function textAction(content: string): TextAction {
  return { type: "text", content };
}

/**
 * Create a button action.
 */
export function buttonAction(id: string): ButtonAction {
  return { type: "button", id };
}

/**
 * Create a photo action.
 */
export function photoAction(data: string, mimeType: string): PhotoAction {
  return { type: "photo", data, mimeType };
}
