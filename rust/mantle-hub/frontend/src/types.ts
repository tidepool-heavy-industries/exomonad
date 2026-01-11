// API types for mantle-hub frontend
// Mirrors rust/mantle-shared/src/hub/types.rs

// ============================================================================
// State Enums
// ============================================================================

export type SessionState = "running" | "completed" | "failed";
export type NodeState = "pending" | "running" | "completed" | "failed" | "cancelled";

// ============================================================================
// Session Types
// ============================================================================

export interface SessionInfo {
  id: string;
  name: string;
  state: SessionState;
  created_at: string;
  updated_at: string;
  node_count: number;
}

export interface SessionWithNodes {
  session: SessionInfo;
  nodes: NodeInfo[];
}

// ============================================================================
// Node Types
// ============================================================================

export interface NodeInfo {
  id: string;
  session_id: string;
  parent_node_id: string | null;
  branch: string;
  worktree: string;
  prompt: string;
  model: string;
  state: NodeState;
  created_at: string;
  updated_at: string;
  result?: NodeResult;
}

export interface NodeResult {
  node_id: string;
  exit_code: number;
  is_error: boolean;
  result_text?: string;
  structured_output?: unknown;
  total_cost_usd: number;
  num_turns: number;
  cc_session_id: string;
  duration_secs: number;
  model_usage: Record<string, ModelUsage>;
}

export interface ModelUsage {
  input_tokens: number;
  output_tokens: number;
  cache_read_input_tokens: number;
  cache_creation_input_tokens: number;
  cost_usd: number;
}

// ============================================================================
// Event Types
// ============================================================================

export interface NodeEvent {
  id: number;
  node_id: string;
  event_type: string;
  event: StreamEvent;
  timestamp: string;
}

// StreamEvent from Claude Code (simplified for frontend)
export type StreamEvent =
  | { AssistantMessage: { content: string } }
  | { ToolUse: { name: string; input: unknown } }
  | { ToolResult: { output: string } }
  | { SystemMessage: { content: string } }
  | { UserMessage: { content: string } }
  | { Error: { message: string } }
  | Record<string, unknown>;

// ============================================================================
// WebSocket Events
// ============================================================================

export type HubEvent =
  | { SessionCreated: { session: SessionInfo } }
  | { SessionUpdated: { session: SessionInfo } }
  | { NodeCreated: { node: NodeInfo } }
  | { NodeUpdated: { node: NodeInfo } }
  | { NodeCompleted: { node_id: string; result: NodeResult } }
  | { NodeFailed: { node_id: string; error: string } }
  | { NodeEvent: { session_id: string; node_id: string; event: StreamEvent; timestamp: string } };

export interface InitMessage {
  type: "init";
  sessions: SessionInfo[];
}

export type WebSocketMessage = InitMessage | HubEvent;

// ============================================================================
// Graph Types
// ============================================================================

export interface GraphData {
  session: SessionInfo;
  nodes: GraphNode[];
  edges: GraphEdge[];
}

export interface GraphNode {
  id: string;
  branch: string;
  state: NodeState;
  prompt: string;
  parent_id?: string;
  result_text?: string;
  structured_output?: unknown;
  total_cost_usd?: number;
  duration_secs?: number;
}

export interface GraphEdge {
  source: string;
  target: string;
}

// ============================================================================
// API Response Types
// ============================================================================

export interface SessionCreateResponse {
  session: SessionInfo;
  root_node: NodeInfo;
}

export interface NodeCreateResponse {
  node: NodeInfo;
}
