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
  // DM-specific fields (optional)
  dmStats: DMStats | null;
  dmClocks: Clock[];
  dmDicePool: DicePool | null;
  dmMood: DMMood | null;
  dmCharCreation: CharacterCreation | null;
  dmHistory: HistoryEntry[];
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
// DM Game Types (mirrors wire-types/src/Tidepool/Wire/Types.hs)
// ============================================================================

/** Precarity level - drives narrative voice intensity. */
export type Precarity =
  | "operatingFromStrength"
  | "roomToManeuver"
  | "wallsClosingIn"
  | "hangingByThread";

/** Character stats for sidebar display. */
export interface DMStats {
  stress: number;        // 0-9, trauma at 9
  heat: number;          // 0-9
  coin: number;
  wantedLevel: number;   // 0-4
  trauma: string[];      // Freeform trauma names
  precarity: Precarity;
}

/** Clock color/type. */
export type ClockColor = "threat" | "opportunity" | "neutral";

/** Progress clock for tracking threats and opportunities. */
export interface Clock {
  id: string;
  name: string;
  segments: number;      // 4-8
  filled: number;
  visible: boolean;      // False = hidden GM clock
  color: ClockColor;
}

/** Action position (risk level). */
export type Position = "controlled" | "risky" | "desperate";

/** Action effect level. */
export type Effect = "limited" | "standard" | "great";

/** Outcome tier - calculated from die value and position. */
export type OutcomeTier = "critical" | "success" | "partial" | "bad" | "disaster";

/** Single die option with LLM-generated preview. */
export interface DieOption {
  value: number;         // 1-6
  tier: OutcomeTier;
  hint: string;          // LLM-generated preview of this outcome
}

/** Dice pool state. */
export interface DicePool {
  dice: DieOption[];
  position: Position;
  effect: Effect;
  context: string;       // What they're attempting
  pushAvailable: boolean;
  devilBargain: string | null;
}

/** Scene urgency level. */
export type Urgency = "low" | "medium" | "high" | "critical";

/** Scene variant - what kind of scene this is. */
export type SceneVariant =
  | { variant: "encounter"; urgency: Urgency }
  | { variant: "opportunity"; catch: string }
  | { variant: "discovery"; implications: string };

/** Action domain overlay. */
export type ActionDomain = "infiltration" | "social" | "violence" | "pursuit" | "arcane";

/** Action variant - risk level with context. */
export type ActionVariant =
  | { variant: "controlled"; threat: string; opportunity: string }
  | { variant: "risky"; threat: string; opportunity: string }
  | { variant: "desperate"; threat: string; opportunity: string };

/** Aftermath variant - how the action resolved. */
export type AftermathVariant =
  | { variant: "clean" }
  | { variant: "costly"; cost: string }
  | { variant: "setback"; escape: string }
  | { variant: "disaster" };

/** Downtime variant - what kind of downtime activity. */
export type DowntimeVariant =
  | { variant: "recovery"; activities: string[] }
  | { variant: "project"; name: string; progress: number }
  | { variant: "entanglement"; description: string };

/** Bargain cost types. */
export type BargainCost =
  | { type: "stress"; amount: number }
  | { type: "heat"; amount: number }
  | { type: "wanted" }
  | { type: "clockTick"; clock: string; segments: number }
  | { type: "factionDebt"; faction: string }
  | { type: "trauma" }
  | { type: "item"; item: string };

/** Bargain option - LLM-generated contextual deal. */
export interface BargainOption {
  label: string;
  cost: BargainCost;
  description: string;
}

/** Current game phase/mood with rich variants. */
export type DMMood =
  | { mood: "scene"; scene: SceneVariant }
  | { mood: "action"; action: ActionVariant; domain: ActionDomain | null }
  | { mood: "aftermath"; aftermath: AftermathVariant }
  | { mood: "downtime"; downtime: DowntimeVariant }
  | { mood: "trauma" }
  | { mood: "bargain"; options: BargainOption[] };

/** Character archetype. */
export type Archetype = "cutter" | "hound" | "leech" | "lurk" | "slide" | "spider" | "whisper";

/** Character pronouns. */
export type Pronouns =
  | { type: "heHim" }
  | { type: "sheHer" }
  | { type: "theyThem" }
  | { type: "custom"; pronouns: string };

/** Tarot card for character creation spread. */
export interface TarotCard {
  name: string;
  meaning: string;
}

/** Three-card tarot spread for character creation. */
export interface TarotSpread {
  past: TarotCard;
  present: TarotCard;
  future: TarotCard;
}

/** Character creation step. */
export type CharCreationStep =
  | { step: "enterName" }
  | { step: "choosePronouns" }
  | { step: "chooseArchetype" }
  | { step: "enterBackground" }
  | { step: "drawTarot" }
  | { step: "confirm" };

/** Character creation state. */
export interface CharacterCreation {
  step: CharCreationStep;
  name: string | null;
  pronouns: Pronouns | null;
  archetype: Archetype | null;
  background: string | null;
  tarotSpread: TarotSpread | null;
}

/** History entry for session log. */
export interface HistoryEntry {
  timestamp: string;
  type: string;          // "narration", "action", "roll", "clock"
  summary: string;
  details: string | null;
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
