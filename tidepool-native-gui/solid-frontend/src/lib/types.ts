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

// ============================================================================
// GraphExport Types (from /graph/info endpoint)
// ============================================================================

/** Complete graph export for visualization. */
export interface GraphExport {
  entryType: string | null;
  exitType: string | null;
  nodes: Record<string, NodeExport>;
  edges: EdgeExport[];
}

/** Node export with full metadata. */
export interface NodeExport {
  kind: "LLM" | "Logic";
  needs: string[];
  schema: SchemaExport | null;
  template: TemplateExport | null;
  system: TemplateExport | null;
  tools: ToolExport[];
  memory: MemoryExport | null;
  transitions: string[];
  canExit: boolean;
  hasVision: boolean;
}

/** Edge between nodes. */
export interface EdgeExport {
  from: string;
  to: string;
  payload: string | null;
  kind: "implicit" | "explicit";
}

/** Schema info for structured output. */
export interface SchemaExport {
  typeName: string;
  fields: Array<{ name: string; type: string; required: boolean }>;
  jsonSchema: unknown;
}

/** Template info for prompt rendering. */
export interface TemplateExport {
  typeName: string;
  path: string;
  deps: string[];
  accessedFields: string[];
  contextType: string;
}

/** Memory info for persistent state. */
export interface MemoryExport {
  typeName: string;
}

/** Tool info for LLM-invocable actions. */
export interface ToolExport {
  name: string;
  description: string;
  inputSchema: unknown;
}
