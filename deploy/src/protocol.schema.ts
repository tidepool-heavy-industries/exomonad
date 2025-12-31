/**
 * Zod schemas for protocol types.
 *
 * These schemas validate JSON from Haskell WireTypes at runtime.
 * If Haskell changes a field name or type, these will fail to parse.
 */

import { z } from "zod";

// JSON Schema (recursive, so we use passthrough for flexibility)
export const JsonSchemaZ: z.ZodType<unknown> = z
  .object({
    type: z.string().optional(),
    properties: z.record(z.lazy(() => JsonSchemaZ)).optional(),
    items: z.lazy(() => JsonSchemaZ).optional(),
    required: z.array(z.string()).optional(),
    enum: z.array(z.unknown()).optional(),
    description: z.string().optional(),
  })
  .passthrough();

// SerializableEffect variants
export const LlmCompleteEffectZ = z.object({
  type: z.literal("LlmComplete"),
  eff_node: z.string(),
  eff_system_prompt: z.string(),
  eff_user_content: z.string(),
  eff_schema: JsonSchemaZ.nullable(),
});

export const HttpFetchEffectZ = z.object({
  type: z.literal("HttpFetch"),
  eff_url: z.string(),
  eff_method: z.string(),
});

export const LogInfoEffectZ = z.object({
  type: z.literal("LogInfo"),
  eff_message: z.string(),
});

export const LogErrorEffectZ = z.object({
  type: z.literal("LogError"),
  eff_message: z.string(),
});

export const SerializableEffectZ = z.discriminatedUnion("type", [
  LlmCompleteEffectZ,
  HttpFetchEffectZ,
  LogInfoEffectZ,
  LogErrorEffectZ,
]);

// EffectResult
export const EffectResultZ = z.discriminatedUnion("type", [
  z.object({ type: z.literal("success"), value: z.unknown() }),
  z.object({ type: z.literal("error"), message: z.string() }),
]);

// ExecutionPhase
export const ExecutionPhaseZ = z.discriminatedUnion("type", [
  z.object({ type: z.literal("idle") }),
  z.object({ type: z.literal("in_node"), nodeName: z.string() }),
  z.object({
    type: z.literal("transitioning"),
    fromNode: z.string(),
    toNode: z.string(),
  }),
  z.object({ type: z.literal("completed"), result: z.unknown() }),
  z.object({ type: z.literal("failed"), error: z.string() }),
]);

// GraphState
export const GraphStateZ = z.object({
  phase: ExecutionPhaseZ,
  completedNodes: z.array(z.string()),
});

// StepOutput
export const StepOutputZ = z.object({
  effect: SerializableEffectZ.nullable(),
  done: z.boolean(),
  stepResult: z.unknown().nullable(),
  graphState: GraphStateZ,
});

// Type exports (inferred from schemas)
export type SerializableEffect = z.infer<typeof SerializableEffectZ>;
export type EffectResult = z.infer<typeof EffectResultZ>;
export type ExecutionPhase = z.infer<typeof ExecutionPhaseZ>;
export type GraphState = z.infer<typeof GraphStateZ>;
export type StepOutput = z.infer<typeof StepOutputZ>;
