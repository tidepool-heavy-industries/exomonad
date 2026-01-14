/**
 * Protocol types for WASM ↔ TypeScript communication.
 *
 * Design principle: TypeScript is a graph-aware effect interpreter.
 *
 * The WASM module handles agent logic and state transitions, but when it needs
 * to perform a side effect (LLM call, logging, database access), it yields
 * control to TypeScript. TypeScript interprets the effect and returns the
 * result to WASM to continue the next step.
 * - Haskell owns: graph structure, DAG ordering, Needs resolution, Goto/exitWith
 * - TypeScript owns: domain-specific effects (LLM, Habitica), persistence, logging, observability
 * - No general-purpose primitives (HTTP fetch) - only domain-specific effects
 *
 * These types match the Haskell Serializable.hs and Info.hs modules.
 */

// ═══════════════════════════════════════════════════════════════════════════
// JSON SCHEMA (for LLM structured output)
// ═══════════════════════════════════════════════════════════════════════════

export interface JsonSchema {
  type?: string;
  properties?: Record<string, JsonSchema>;
  items?: JsonSchema;
  required?: string[];
  enum?: unknown[];
  description?: string;
}

// ═══════════════════════════════════════════════════════════════════════════
// TYPE INFO (runtime representation of Haskell types)
// ═══════════════════════════════════════════════════════════════════════════

export interface TypeInfo {
  /** Simple type name, e.g., "Intent" */
  typeName: string;
  /** Module path, e.g., "Echo" */
  typeModule: string;
}

// ═══════════════════════════════════════════════════════════════════════════
// GRAPH STRUCTURE (compile-time info, from Haskell Graph.Info)
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Detailed graph metadata - matches Haskell GraphInfo with full type information.
 * Edges are derived from Needs/Schema relationships (implicit data flow)
 * and GotoTargets (explicit control flow).
 *
 * Note: The simple GraphInfo type used by the loader is in graphs.ts.
 * This detailed version is for advanced introspection features.
 */
export interface DetailedGraphInfo {
  /** Graph name */
  name: string;
  /** Type accepted at Entry */
  entryType: TypeInfo;
  /** Type produced at Exit */
  exitType: TypeInfo;
  /** All nodes in the graph (excludes Entry/Exit) */
  nodes: NodeInfo[];
  /** Edges for visualization (from Needs/Schema and Goto) */
  edges: EdgeInfo[];
}

export type NodeKind = "LLM" | "Logic";

/**
 * Node metadata - matches Haskell NodeInfo.
 * In Servant-style design: field name becomes node name.
 */
export interface NodeInfo {
  /** Node name (record field name in Servant-style) */
  niName: string;
  /** LLM nodes yield effects; Logic nodes run Haskell code */
  niKind: NodeKind;
  /** Input type this node needs (single type) */
  niInput: TypeInfo | null;
  /** Output type (LLM nodes only - from Schema annotation) */
  niSchema: TypeInfo | null;
  /** Possible Goto targets (Logic nodes only) */
  niGotoTargets: GotoTarget[];
}

/**
 * Edge for graph visualization.
 */
export interface EdgeInfo {
  /** Source: "Entry" or node name */
  eiFrom: string;
  /** Target: "Exit" or node name */
  eiTo: string;
  /** Type carried on this edge */
  eiPayloadType: TypeInfo;
}

/**
 * Goto target - control flow destination.
 * In Servant-style: Goto is control-flow only, no payload.
 * Target node pulls data from accumulated context via Needs.
 */
export interface GotoTarget {
  /** Target node name, or "Exit" */
  gtTarget: string;
  /** Type passed (for Exit transitions) */
  gtPayloadType: TypeInfo;
}

// ═══════════════════════════════════════════════════════════════════════════
// RUNTIME STATE (matches Haskell Serializable.hs)
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Runtime graph state - matches Haskell GraphState.
 * Haskell maintains available values internally; TypeScript just observes.
 */
export interface GraphState {
  /** Current execution phase */
  phase: ExecutionPhase;
  /** Nodes that have completed */
  completedNodes: string[];
}

/**
 * Execution phase - matches Haskell ExecutionPhase.
 * Haskell uses ObjectWithSingleField encoding: {type: "in_node", nodeName: "classify"}
 */
export type ExecutionPhase =
  | { type: "idle" }
  | { type: "in_node"; nodeName: string }
  | { type: "transitioning"; fromNode: string; toNode: string }
  | { type: "completed"; result: unknown }
  | { type: "failed"; error: string };

// Helper to extract current node from phase (for observability)
export function getCurrentNode(phase: ExecutionPhase): string | null {
  if (phase.type === "in_node") return phase.nodeName;
  if (phase.type === "transitioning") return phase.fromNode;
  return null;
}

// ═══════════════════════════════════════════════════════════════════════════
// EFFECTS (what WASM yields to TypeScript - matches Haskell SerializableEffect)
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Effect types that Haskell yields for TypeScript to execute.
 * Haskell uses flat encoding: {type: "LlmComplete", eff_node: "...", ...}
 *
 * This union must match the SerializableEffect type in WireTypes.hs exactly.
 */
export type SerializableEffect =
  | LlmCompleteEffect
  | LlmCallEffect
  | LogInfoEffect
  | LogErrorEffect
  | HabiticaEffect
  | TelegramSendEffect
  | TelegramAskEffect
  | TelegramReceiveEffect
  | TelegramTryReceiveEffect
  | TelegramConfirmEffect
  | GetStateEffect
  | SetStateEffect
  | EmitEventEffect
  | RandomIntEffect
  | GetTimeEffect;

/**
 * Internal effects are executed by the StateMachineDO directly.
 * Yielded effects are sent to the caller for external handling.
 */
export type InternalEffect =
  | LlmCompleteEffect
  | LlmCallEffect
  | LogInfoEffect
  | LogErrorEffect
  | HabiticaEffect
  | GetStateEffect
  | SetStateEffect
  | RandomIntEffect
  | GetTimeEffect;

export type YieldedEffect =
  | TelegramSendEffect
  | TelegramAskEffect
  | TelegramReceiveEffect
  | TelegramTryReceiveEffect
  | TelegramConfirmEffect
  | EmitEventEffect;

/**
 * Type guard for internal effects (type narrowing).
 * Use this when you need TypeScript to narrow the effect type.
 * For runtime routing decisions, routing.ts also exports isInternalEffect.
 */
export function narrowToInternalEffect(effect: SerializableEffect): effect is InternalEffect {
  return ["LlmComplete", "LlmCall", "LogInfo", "LogError", "Habitica", "GetState", "SetState", "RandomInt", "GetTime"].includes(effect.type);
}

/**
 * Type guard for yielded effects (type narrowing).
 * Use this when you need TypeScript to narrow the effect type.
 * For runtime routing decisions, routing.ts also exports isYieldedEffect.
 */
export function narrowToYieldedEffect(effect: SerializableEffect): effect is YieldedEffect {
  return ["TelegramSend", "TelegramAsk", "telegram_send", "telegram_receive", "telegram_try_receive", "TelegramConfirm", "EmitEvent"].includes(effect.type);
}

/**
 * LLM completion request - matches Haskell EffLlmComplete.
 * TypeScript calls the LLM API and returns parsed output.
 */
export interface LlmCompleteEffect {
  type: "LlmComplete";
  /** Which node is making this call */
  eff_node: string;
  /** System prompt */
  eff_system_prompt: string;
  /** User content */
  eff_user_content: string;
  /** JSON schema for structured output */
  eff_schema: JsonSchema | null;
  /** Model to use (e.g., "@cf/meta/llama-3.3-70b-instruct-fp8-fast"). If null, use default. */
  eff_model?: string | null;
}

/**
 * Full LLM API call with conversation history - matches Haskell EffLlmCall.
 * Supports tool use and multi-turn conversations.
 */
export interface LlmCallEffect {
  type: "LlmCall";
  /** Which node is making this call */
  eff_node: string;
  /** Full conversation history */
  eff_messages: WireMessage[];
  /** JSON schema for structured output */
  eff_schema?: JsonSchema | null;
  /** Tool definitions (Anthropic format) */
  eff_tools: unknown[];
  /** Model to use. If null, use default. */
  eff_model?: string | null;
}

/**
 * Wire-format message for LLM conversation history.
 * Matches Haskell WireMessage.
 */
export interface WireMessage {
  /** Role: "user" | "assistant" | "system" */
  role: string;
  /** Content blocks */
  content: WireContentBlock[];
}

/**
 * Wire-format content block for LLM messages.
 * Matches Haskell WireContentBlock.
 */
export type WireContentBlock =
  | { type: "text"; text: string }
  | { type: "image"; source: ImageSource }
  | { type: "tool_use"; id: string; name: string; input: unknown }
  | { type: "tool_result"; tool_use_id: string; content: string; is_error: boolean };

/**
 * Image source for vision content.
 */
export interface ImageSource {
  type: "base64" | "url";
  media_type?: string;
  data?: string;
  url?: string;
}

/**
 * Info log - matches Haskell EffLogInfo.
 */
export interface LogInfoEffect {
  type: "LogInfo";
  eff_message: string;
  /** Optional structured fields for queryable log data */
  eff_fields?: Record<string, unknown>;
}

/**
 * Error log - matches Haskell EffLogError.
 */
export interface LogErrorEffect {
  type: "LogError";
  eff_message: string;
  /** Optional structured fields for queryable log data */
  eff_fields?: Record<string, unknown>;
}

/**
 * Habitica API effect - matches Haskell EffHabitica.
 * Used for task management and gamification features.
 */
export interface HabiticaEffect {
  type: "Habitica";
  /** Operation name: "GetUser", "ScoreTask", "GetTasks", etc. */
  eff_hab_op: string;
  /** Operation-specific payload (JSON) */
  eff_hab_payload: unknown;
}

// ═══════════════════════════════════════════════════════════════════════════
// STATE EFFECTS (for DM and other stateful graphs)
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Read state from Durable Object storage - matches Haskell EffGetState.
 */
export interface GetStateEffect {
  type: "GetState";
  /** State key (e.g., "worldState", "sessionState") */
  eff_state_key: string;
}

/**
 * Write state to Durable Object storage - matches Haskell EffSetState.
 */
export interface SetStateEffect {
  type: "SetState";
  /** State key */
  eff_state_key: string;
  /** New state value (full replacement) */
  eff_state_value: unknown;
}

// ═══════════════════════════════════════════════════════════════════════════
// EVENT EMISSION (for observability/GUI updates)
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Emit an event for external observers - matches Haskell EffEmitEvent.
 * Used for GUI updates, analytics, logging pipelines.
 */
export interface EmitEventEffect {
  type: "EmitEvent";
  /** Event name (e.g., "StressChanged", "ClockAdvanced") */
  eff_event_name: string;
  /** Event-specific payload */
  eff_event_payload: unknown;
}

// ═══════════════════════════════════════════════════════════════════════════
// RANDOM NUMBER GENERATION
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Generate a random integer in range - matches Haskell EffRandomInt.
 */
export interface RandomIntEffect {
  type: "RandomInt";
  /** Minimum value (inclusive) */
  eff_min: number;
  /** Maximum value (inclusive) */
  eff_max: number;
}

// ═══════════════════════════════════════════════════════════════════════════
// TIME
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Get current UTC time - matches Haskell EffGetTime.
 * Returns ISO8601 string.
 */
export interface GetTimeEffect {
  type: "GetTime";
}

// ═══════════════════════════════════════════════════════════════════════════
// TELEGRAM EFFECT TYPES (from tidepool-telegram-ts)
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Outgoing messages that can be sent to the user.
 * Mirrors Haskell: Tidepool.Telegram.Types.OutgoingMessage
 */
export type TelegramOutgoingMessage =
  | { type: 'text'; text: string }
  | { type: 'photo'; media: string; caption?: string }
  | { type: 'document'; media: string; filename: string }
  | { type: 'buttons'; text: string; buttons: TelegramInlineButton[][] };

/**
 * Inline keyboard button with callback data.
 */
export interface TelegramInlineButton {
  text: string;
  data: unknown;
}

/**
 * Incoming messages received from the user.
 * Mirrors Haskell: Tidepool.Telegram.Types.IncomingMessage
 */
export type TelegramIncomingMessage =
  | { type: 'text'; text: string }
  | { type: 'photo'; media: string; caption?: string }
  | { type: 'document'; media: string; filename: string }
  | { type: 'button_click'; data: unknown };

/**
 * Send a message (fire and forget).
 * Mirrors Haskell: Send :: OutgoingMessage -> Telegram m ()
 */
export interface TelegramSendEffect {
  type: "telegram_send";
  message: TelegramOutgoingMessage;
}

/**
 * Ask user with inline buttons - matches Haskell EffTelegramAsk.
 * Blocking effect - waits for button click or text response.
 */
export interface TelegramAskEffect {
  type: "TelegramAsk";
  /** Message text to display */
  eff_tg_text: string;
  /** Parse mode: "PlainText" | "Markdown" | "HTML" */
  eff_tg_parse_mode: string;
  /** Button options: [[label, callback], ...] */
  eff_buttons: [string, string][];
  /** Optional thread/topic ID for group forums */
  eff_tg_thread_id?: number | null;
}

/**
 * Result from TelegramAsk effect - matches Haskell TelegramAskResult.
 */
export type TelegramAskResult =
  | { type: "button"; response: string }
  | { type: "text"; text: string }
  | { type: "stale_button" };

/**
 * Block until at least one message arrives.
 * The runtime should block until there's at least one message,
 * then return all pending messages.
 * Mirrors Haskell: Receive :: Telegram m (NonEmpty IncomingMessage)
 */
export interface TelegramReceiveEffect {
  type: "telegram_receive";
}

/**
 * Non-blocking check for pending messages.
 * Returns immediately with any pending messages (may be empty).
 * Mirrors Haskell: TryReceive :: Telegram m [IncomingMessage]
 */
export interface TelegramTryReceiveEffect {
  type: "telegram_try_receive";
}

/**
 * Request user confirmation via Telegram inline buttons.
 * Blocking effect - waits for user to click a button.
 * Mirrors Haskell: EffTelegramConfirm
 *
 * Default buttons from Haskell:
 * - "✓ Yes" -> "approved"
 * - "✗ No" -> "denied"
 * - "Skip" -> "skipped"
 */
export interface TelegramConfirmEffect {
  type: "TelegramConfirm";
  /** Message to display above buttons */
  eff_message: string;
  /** Buttons as [label, value] pairs */
  eff_buttons: [string, string][];
}

/**
 * Response format for TelegramConfirmEffect.
 * Parsed by Haskell's parseConfirmation in HabiticaRoutingGraph.hs
 */
export interface TelegramConfirmResponse {
  response: "approved" | "denied" | "skipped";
  feedback?: string;
}

// ═══════════════════════════════════════════════════════════════════════════
// PLANNED EFFECTS (not yet in Haskell)
// ═══════════════════════════════════════════════════════════════════════════

// Future effect types that may be added:
// - DbQueryEffect (D1 database)
// - KvGetEffect / KvPutEffect (Workers KV)
// - ScheduleAlarmEffect (Durable Object alarms)
// - ClaudeCodeEffect (subprocess execution - native only)

// ═══════════════════════════════════════════════════════════════════════════
// EFFECT RESULTS (what TypeScript returns to WASM - matches Haskell EffectResult)
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Result of effect execution.
 * Aeson TaggedObject puts single-field records directly at top level (no contents wrapper).
 * fieldLabelModifier drops "res" prefix: resValue -> value, resMessage -> message
 */
export type EffectResult =
  | { type: "success"; value: unknown }
  | { type: "error"; message: string };

// Helper to create success result
export function successResult(value: unknown): EffectResult {
  return { type: "success", value };
}

// Helper to create error result
export function errorResult(message: string): EffectResult {
  return { type: "error", message };
}

/**
 * Result from LLM call (for LlmComplete effect).
 * This is what goes in the `value` field of a success result.
 */
export interface LlmResult {
  /** Parsed output (matches schema) */
  output: unknown;
  /** Token usage (optional) */
  usage?: {
    inputTokens: number;
    outputTokens: number;
  };
}

// ═══════════════════════════════════════════════════════════════════════════
// TELEGRAM EFFECT RESULTS
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Result for TelegramSendEffectV2: unit (void).
 * Mirrors Haskell: UnitResult
 */
export interface TelegramUnitResult {
  type: 'unit';
}

/**
 * Result for TelegramReceiveEffect / TelegramTryReceiveEffect.
 * For Receive: guaranteed to have at least one message.
 * For TryReceive: may be empty.
 * Mirrors Haskell: MessagesResult
 */
export interface TelegramMessagesResult {
  type: 'messages';
  messages: TelegramIncomingMessage[];
}

/** Create a unit result. */
export function telegramUnitResult(): TelegramUnitResult {
  return { type: 'unit' };
}

/** Create a messages result. */
export function telegramMessagesResult(messages: TelegramIncomingMessage[]): TelegramMessagesResult {
  return { type: 'messages', messages };
}

// ═══════════════════════════════════════════════════════════════════════════
// STEP OUTPUT (response from WASM per step - matches Haskell StepOutput)
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Output from each step of graph execution.
 * Matches Haskell StepOutput exactly.
 */
export interface StepOutput {
  /** Effect to execute, if any */
  effect: SerializableEffect | null;

  /** Is graph execution complete? */
  done: boolean;

  /** Final result (only when done=true, matches Exit type) */
  stepResult: unknown | null;

  /** Current graph execution state (for observability) */
  graphState: GraphState;
}

// ═══════════════════════════════════════════════════════════════════════════
// WASM MODULE INTERFACE (what the loaded module exposes)
// ═══════════════════════════════════════════════════════════════════════════

/**
 * WASM exports from the Haskell graph machine.
 * Matches the exports defined in Haskell Machine.hs.
 */
export interface GraphMachineExports {
  /** Start graph execution with JSON input */
  initialize(input: string): Promise<string>; // → JSON StepOutput

  /** Continue execution with JSON EffectResult */
  step(result: string): Promise<string>; // → JSON StepOutput

  /** Get compile-time graph structure (returns simple GraphInfo from graphs.ts) */
  getGraphInfo(): string; // → JSON { id, name, nodes, edges }

  /** Get current runtime state */
  getGraphState(): string; // → JSON GraphState

  // GHC RTS exports
  memory: WebAssembly.Memory;
  hs_init(argc: number, argv: number): void;
}

// ═══════════════════════════════════════════════════════════════════════════
// RUNNER TYPES (TypeScript-side orchestration)
// ═══════════════════════════════════════════════════════════════════════════

/** Configuration for graph execution */
export interface GraphRunnerConfig {
  /** Maximum steps before timeout (default: 1000) */
  maxSteps?: number;
  /** Step callback for progress monitoring */
  onStep?: (output: StepOutput, stepNum: number) => void;
}

/** Result of running a graph to completion */
export interface GraphRunResult {
  /** Final output value (matches graph's Exit type) */
  result: unknown;
  /** Was execution successful? */
  success: boolean;
  /** Error message if failed */
  error?: string;
  /** Final graph state */
  finalState: GraphState;
  /** Number of steps taken */
  totalSteps: number;
}

// ═══════════════════════════════════════════════════════════════════════════
// WEBSOCKET PROTOCOL (Client ↔ Server messages)
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Messages sent from client to server.
 */
export type ClientMessage =
  | { type: 'init'; graphId: string; input: unknown }
  | { type: 'resume'; result: EffectResult }
  | { type: 'reconnect'; sessionId: string }
  | { type: 'ping' };

/**
 * Messages sent from server to client.
 */
export type ServerMessage =
  | { type: 'yield'; effect: SerializableEffect; sessionId: string }
  | { type: 'progress'; effect: SerializableEffect; status: string }
  | { type: 'done'; result: unknown }
  | { type: 'error'; message: string; recoverable: boolean; sessionId?: string }
  | { type: 'pong' };

/**
 * Session state stored in Durable Object storage for reconnection.
 */
export interface SessionState {
  /** Graph being executed */
  graphId: string;
  /** Serialized WASM machine state (opaque to TypeScript) */
  machineState: unknown;
  /** Effect waiting for client response, if any */
  pendingEffect: SerializableEffect | null;
  /** Last activity timestamp (Unix ms) */
  lastActivity: number;
}

/** Session timeout in milliseconds (5 minutes) */
export const SESSION_TIMEOUT_MS = 5 * 60 * 1000;
