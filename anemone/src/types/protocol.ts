/**
 * Protocol types for WASM graph execution.
 * These match the types in deploy/src/protocol.ts
 */

// ═══════════════════════════════════════════════════════════════════════════
// JSON SCHEMA
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
// TYPE INFO
// ═══════════════════════════════════════════════════════════════════════════

export interface TypeInfo {
  typeName: string;
  typeModule: string;
}

// ═══════════════════════════════════════════════════════════════════════════
// GRAPH STRUCTURE
// ═══════════════════════════════════════════════════════════════════════════

export interface GraphInfo {
  name: string;
  entryType: TypeInfo;
  exitType: TypeInfo;
  nodes: NodeInfo[];
  edges: EdgeInfo[];
}

export type NodeKind = "LLM" | "Logic";

export interface NodeInfo {
  niName: string;
  niKind: NodeKind;
  niNeeds: TypeInfo[];
  niSchema: TypeInfo | null;
  niGotoTargets: GotoTarget[];
}

export interface EdgeInfo {
  eiFrom: string;
  eiTo: string;
  eiPayloadType: TypeInfo;
}

export interface GotoTarget {
  gtTarget: string;
  gtPayloadType: TypeInfo;
}

// ═══════════════════════════════════════════════════════════════════════════
// RUNTIME STATE
// ═══════════════════════════════════════════════════════════════════════════

export interface GraphState {
  phase: ExecutionPhase;
  completedNodes: string[];
}

export type ExecutionPhase =
  | { type: "idle" }
  | { type: "in_node"; nodeName: string }
  | { type: "transitioning"; fromNode: string; toNode: string }
  | { type: "completed"; result: unknown }
  | { type: "failed"; error: string };

export function getCurrentNode(phase: ExecutionPhase): string | null {
  if (phase.type === "in_node") return phase.nodeName;
  if (phase.type === "transitioning") return phase.fromNode;
  return null;
}

// ═══════════════════════════════════════════════════════════════════════════
// EFFECTS
// ═══════════════════════════════════════════════════════════════════════════

export type SerializableEffect =
  | LlmCompleteEffect
  | LogInfoEffect
  | LogErrorEffect;

export interface LlmCompleteEffect {
  type: "LlmComplete";
  eff_node: string;
  eff_system_prompt: string;
  eff_user_content: string;
  eff_schema: JsonSchema | null;
}

export interface LogInfoEffect {
  type: "LogInfo";
  eff_message: string;
}

export interface LogErrorEffect {
  type: "LogError";
  eff_message: string;
}

// ═══════════════════════════════════════════════════════════════════════════
// EFFECT RESULTS
// ═══════════════════════════════════════════════════════════════════════════

export type EffectResult =
  | { type: "success"; value: unknown }
  | { type: "error"; message: string };

export function successResult(value: unknown): EffectResult {
  return { type: "success", value };
}

export function errorResult(message: string): EffectResult {
  return { type: "error", message };
}

export interface LlmResult {
  output: unknown;
  usage?: {
    inputTokens: number;
    outputTokens: number;
  };
}

// ═══════════════════════════════════════════════════════════════════════════
// STEP OUTPUT
// ═══════════════════════════════════════════════════════════════════════════

export interface StepOutput {
  effect: SerializableEffect | null;
  done: boolean;
  stepResult: unknown | null;
  graphState: GraphState;
}

// ═══════════════════════════════════════════════════════════════════════════
// WEBSOCKET MESSAGES
// ═══════════════════════════════════════════════════════════════════════════

/** Messages from client to server */
export type ClientMessage =
  | { type: "init"; input: unknown }
  | { type: "resume"; result: EffectResult };

/** Messages from server to client */
export type ServerMessage =
  | { type: "progress"; node: string; effect: string }
  | { type: "suspend"; effect: SerializableEffect }
  | { type: "done"; result: unknown }
  | { type: "error"; message: string };
