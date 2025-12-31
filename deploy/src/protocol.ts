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
 * Static graph metadata - matches Haskell GraphInfo.
 * Edges are derived from Needs/Schema relationships (implicit data flow)
 * and GotoTargets (explicit control flow).
 */
export interface GraphInfo {
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
 */
export type SerializableEffect =
  | LlmCompleteEffect
  | LogInfoEffect
  | LogErrorEffect
  | HabiticaEffect;

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
}

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

// ═══════════════════════════════════════════════════════════════════════════
// FUTURE EFFECTS (not yet in Haskell, but planned)
// ═══════════════════════════════════════════════════════════════════════════

// These will be added as the Haskell side gains more effect types:
// - ToolCallEffect (LLM tool use)
// - DbQueryEffect (D1 database)
// - KvGetEffect / KvPutEffect (Workers KV)
// - ScheduleAlarmEffect (Durable Object alarms)

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

  /** Get compile-time graph structure */
  getGraphInfo(): string; // → JSON GraphInfo

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
