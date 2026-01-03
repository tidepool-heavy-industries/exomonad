/**
 * Protocol types for WASM ↔ TypeScript communication.
 *
 * Design principle: TypeScript is a graph-aware effect executor.
 * - Haskell owns: graph structure, DAG ordering, Needs resolution, Goto/exitWith
 * - TypeScript owns: domain-specific effects (LLM, Habitica), persistence, logging, observability
 * - No general-purpose primitives (HTTP fetch) - only domain-specific effects
 *
 * These types match the Haskell Serializable.hs and Info.hs modules.
 */
export interface JsonSchema {
    type?: string;
    properties?: Record<string, JsonSchema>;
    items?: JsonSchema;
    required?: string[];
    enum?: unknown[];
    description?: string;
}
export interface TypeInfo {
    /** Simple type name, e.g., "Intent" */
    typeName: string;
    /** Module path, e.g., "Echo" */
    typeModule: string;
}
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
    /** Types this node needs (from context) */
    niNeeds: TypeInfo[];
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
export type ExecutionPhase = {
    type: "idle";
} | {
    type: "in_node";
    nodeName: string;
} | {
    type: "transitioning";
    fromNode: string;
    toNode: string;
} | {
    type: "completed";
    result: unknown;
} | {
    type: "failed";
    error: string;
};
export declare function getCurrentNode(phase: ExecutionPhase): string | null;
/**
 * Effect types that Haskell yields for TypeScript to execute.
 * Haskell uses flat encoding: {type: "LlmComplete", eff_node: "...", ...}
 */
export type SerializableEffect = LlmCompleteEffect | LlmCallEffect | LogInfoEffect | LogErrorEffect | HabiticaEffect | TelegramSendEffect | TelegramReceiveEffect | TelegramTryReceiveEffect | TelegramAskEffect | GetStateEffect | SetStateEffect | EmitEventEffect | RandomIntEffect | GetTimeEffect;
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
    /** Model to use (e.g., "@cf/meta/llama-3.3-70b-instruct-fp8-fast"). If undefined, uses default. */
    eff_model?: string;
}
/**
 * Wire-format message for LLM conversation history.
 * Matches Haskell WireMessage.
 */
export interface WireMessage {
    /** Role: "user" | "assistant" | "system" */
    role: "user" | "assistant" | "system";
    /** Content blocks */
    content: WireContentBlock[];
}
/**
 * Wire-format content block for LLM messages.
 * Matches Haskell WireContentBlock.
 */
export type WireContentBlock = {
    type: "text";
    text: string;
} | {
    type: "tool_use";
    id: string;
    name: string;
    input: unknown;
} | {
    type: "tool_result";
    tool_use_id: string;
    content: string;
    is_error: boolean;
};
/**
 * Tool call request from LLM.
 * Matches Haskell WireToolCall.
 */
export interface WireToolCall {
    /** Unique ID for this tool call (used in tool_result) */
    id: string;
    /** Tool name (e.g., "ask_user") */
    name: string;
    /** Tool arguments (JSON) */
    input: unknown;
}
/**
 * LLM call with tool support - matches Haskell EffLlmCall.
 * TypeScript calls the LLM API and returns either "done" or "needs_tools".
 */
export interface LlmCallEffect {
    type: "LlmCall";
    /** Which node is making this call */
    eff_node: string;
    /** Full conversation history */
    eff_messages: WireMessage[];
    /** JSON schema for structured output */
    eff_schema: JsonSchema | null;
    /** Tool definitions (Anthropic format) */
    eff_tools: unknown[];
    /** Model to use (e.g., "@cf/meta/llama-3.3-70b-instruct-fp8-fast"). If undefined, uses default. */
    eff_model?: string;
}
/**
 * Result from LLM API call - either done or needs tools.
 * Matches Haskell LlmCallResult.
 */
export type LlmCallResult = {
    type: "done";
    content: WireContentBlock[];
} | {
    type: "needs_tools";
    tool_calls: WireToolCall[];
    content: WireContentBlock[];
};
/**
 * Info log - matches Haskell EffLogInfo.
 */
export interface LogInfoEffect {
    type: "LogInfo";
    eff_message: string;
}
/**
 * Error log - matches Haskell EffLogError.
 */
export interface LogErrorEffect {
    type: "LogError";
    eff_message: string;
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
/**
 * Get state by key - matches Haskell EffGetState.
 * TypeScript reads from Durable Object storage or in-memory store.
 */
export interface GetStateEffect {
    type: "GetState";
    /** State key (e.g., "worldState", "sessionState") */
    eff_state_key: string;
}
/**
 * Set state by key - matches Haskell EffSetState.
 * TypeScript writes to Durable Object storage or in-memory store.
 */
export interface SetStateEffect {
    type: "SetState";
    /** State key */
    eff_state_key: string;
    /** New state value (full replacement) */
    eff_state_value: unknown;
}
/**
 * Emit an event for observability/GUI updates - matches Haskell EffEmitEvent.
 * TypeScript forwards to connected WebSocket clients.
 */
export interface EmitEventEffect {
    type: "EmitEvent";
    /** Event name (e.g., "StressChanged", "ClockAdvanced") */
    eff_event_name: string;
    /** Event-specific payload */
    eff_event_payload: unknown;
}
/**
 * Get a random integer - matches Haskell EffRandomInt.
 * TypeScript uses crypto.getRandomValues or Math.random.
 */
export interface RandomIntEffect {
    type: "RandomInt";
    /** Minimum value (inclusive) */
    eff_min: number;
    /** Maximum value (inclusive) */
    eff_max: number;
}
/**
 * Get current UTC time - matches Haskell EffGetTime.
 * TypeScript returns ISO8601 string (e.g., "2024-01-15T10:30:00Z").
 */
export interface GetTimeEffect {
    type: "GetTime";
}
/**
 * Outgoing messages that can be sent to the user.
 * Mirrors Haskell: Tidepool.Telegram.Types.OutgoingMessage
 */
export type TelegramOutgoingMessage = {
    type: 'text';
    text: string;
} | {
    type: 'photo';
    media: string;
    caption?: string;
} | {
    type: 'document';
    media: string;
    filename: string;
} | {
    type: 'buttons';
    text: string;
    buttons: TelegramInlineButton[][];
};
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
export type TelegramIncomingMessage = {
    type: 'text';
    text: string;
} | {
    type: 'photo';
    media: string;
    caption?: string;
} | {
    type: 'document';
    media: string;
    filename: string;
} | {
    type: 'button_click';
    data: unknown;
};
/**
 * Send a message (fire and forget).
 * Mirrors Haskell: Send :: OutgoingMessage -> Telegram m ()
 */
export interface TelegramSendEffect {
    type: "TelegramSend";
    message: TelegramOutgoingMessage;
}
/**
 * Block until at least one message arrives.
 * The runtime should block until there's at least one message,
 * then return all pending messages.
 * Mirrors Haskell: Receive :: Telegram m (NonEmpty IncomingMessage)
 */
export interface TelegramReceiveEffect {
    type: "TelegramReceive";
}
/**
 * Non-blocking check for pending messages.
 * Returns immediately with any pending messages (may be empty).
 * Mirrors Haskell: TryReceive :: Telegram m [IncomingMessage]
 */
export interface TelegramTryReceiveEffect {
    type: "TelegramTryReceive";
}
/**
 * Request user input via Telegram inline buttons.
 * Blocking effect - waits for user to click a button OR send text.
 * Mirrors Haskell: EffTelegramAsk
 *
 * Default buttons from Haskell:
 * - "✓ Yes" -> "approved"
 * - "✗ No" -> "denied"
 * - "Skip" -> "skipped"
 */
export interface TelegramAskEffect {
    type: "TelegramAsk";
    /** Message to display above buttons */
    eff_tg_text: string;
    /** Parse mode: "PlainText" | "Markdown" | "HTML" */
    eff_tg_parse_mode: string;
    /** Buttons as [label, value] pairs */
    eff_buttons: [string, string][];
}
/**
 * Result from TelegramAskEffect.
 * Sum type: user can click button, send text, or click stale button.
 */
export type TelegramAskResult = {
    type: "button";
    response: string;
} | {
    type: "text";
    text: string;
} | {
    type: "stale_button";
};
/**
 * @deprecated Use TelegramAskResult instead
 */
export interface TelegramConfirmResponse {
    response: "approved" | "denied" | "skipped";
    feedback?: string;
}
/**
 * Result of effect execution.
 * Aeson TaggedObject puts single-field records directly at top level (no contents wrapper).
 * fieldLabelModifier drops "res" prefix: resValue -> value, resMessage -> message
 */
export type EffectResult = {
    type: "success";
    value: unknown;
} | {
    type: "error";
    message: string;
};
export declare function successResult(value: unknown): EffectResult;
export declare function errorResult(message: string): EffectResult;
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
export declare function telegramUnitResult(): TelegramUnitResult;
/** Create a messages result. */
export declare function telegramMessagesResult(messages: TelegramIncomingMessage[]): TelegramMessagesResult;
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
/**
 * WASM exports from the Haskell graph machine.
 * Matches the exports defined in Haskell Machine.hs.
 */
export interface GraphMachineExports {
    /** Start graph execution with JSON input */
    initialize(input: string): Promise<string>;
    /** Continue execution with JSON EffectResult */
    step(result: string): Promise<string>;
    /** Get compile-time graph structure (returns simple GraphInfo from graphs.ts) */
    getGraphInfo(): string;
    /** Get current runtime state */
    getGraphState(): string;
    memory: WebAssembly.Memory;
    hs_init(argc: number, argv: number): void;
}
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
/**
 * Messages sent from client to server.
 */
export type ClientMessage = {
    type: 'init';
    graphId: string;
    input: unknown;
} | {
    type: 'resume';
    result: EffectResult;
} | {
    type: 'reconnect';
    sessionId: string;
} | {
    type: 'ping';
};
/**
 * Messages sent from server to client.
 */
export type ServerMessage = {
    type: 'yield';
    effect: SerializableEffect;
    sessionId: string;
} | {
    type: 'progress';
    effect: SerializableEffect;
    status: string;
} | {
    type: 'done';
    result: unknown;
} | {
    type: 'error';
    message: string;
    recoverable: boolean;
    sessionId?: string;
} | {
    type: 'pong';
};
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
export declare const SESSION_TIMEOUT_MS: number;
//# sourceMappingURL=protocol.d.ts.map