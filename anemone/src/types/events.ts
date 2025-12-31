/**
 * UI-specific event types for chat and history tracking.
 */

/** Chat message displayed in the chat pane */
export interface ChatMessage {
  id: string;
  role: "user" | "assistant" | "system";
  content: string;
  timestamp: Date;
  /** Which graph node generated this message (for assistant messages) */
  node?: string;
  /** Effect type if from an LLM effect */
  effectType?: string;
}

/** Event types for the timeline */
export type EventType =
  | "progress" // Node execution started
  | "suspend" // Awaiting effect execution
  | "done" // Graph completed
  | "error" // Execution failed
  | "log_info" // LogInfo effect
  | "log_error" // LogError effect
  | "llm_start" // LLM call started
  | "llm_complete" // LLM call finished
  | "http_start" // HTTP fetch started
  | "http_complete" // HTTP fetch finished
  | "transition" // Node transition
  | "connected" // WebSocket connected
  | "disconnected"; // WebSocket disconnected

/** Event in the history timeline */
export interface TimelineEvent {
  id: string;
  type: EventType;
  timestamp: Date;
  /** Which node this event relates to */
  node?: string;
  /** Additional data for the event */
  data?: unknown;
  /** Human-readable summary */
  summary: string;
}

/** Filter state for the event timeline */
export interface FilterState {
  /** Event types to show (empty = show all) */
  types: EventType[];
  /** Filter to specific node */
  nodeFilter: string | null;
  /** Text search query */
  searchQuery: string;
}

/** Connection status for the WebSocket */
export type ConnectionStatus =
  | "disconnected"
  | "connecting"
  | "connected"
  | "error";
