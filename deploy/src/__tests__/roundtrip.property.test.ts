/**
 * Property-based cross-boundary serialization tests.
 *
 * Uses fast-check to generate test cases in TypeScript, sends them through
 * the WASM roundtrip functions (Haskell deserialize → reserialize), and
 * verifies the output matches the input.
 *
 * This catches any serialization mismatches between TypeScript and Haskell.
 */

import { describe, it, expect, beforeAll } from "vitest";
import * as fc from "fast-check";
import * as fs from "fs";
import * as path from "path";
import { fileURLToPath } from "url";
import {
  loadRoundtripModule,
  type RoundtripExports,
  type RoundtripResult,
} from "../roundtrip-loader.js";
import type {
  SerializableEffect,
  EffectResult,
  ExecutionPhase,
  GraphState,
  StepOutput,
  TypeInfo,
  GotoTarget,
  NodeInfo,
  EdgeInfo,
  DetailedGraphInfo,
} from "tidepool-generated-ts";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

// =============================================================================
// Arbitraries matching Haskell's Arbitrary instances
// =============================================================================

// Basic text - matches ProtocolPropertySpec.hs:45-55
const arbText = fc.oneof(
  fc.string({ minLength: 0, maxLength: 200 }),
  fc.constant(""),
  fc.constant(" "),
  fc.constant("\t\n\r"),
  fc.constant("emoji: \u{1F600}\u{1F4A9}\u{2764}"),
  fc.constant("null"),
  fc.constant('{"nested": "json"}')
);

// JSON values with bounded depth - matches aeson's Arbitrary Value
// Note: We filter out -0 because JSON doesn't distinguish between -0 and +0,
// so {value: [-0]} would roundtrip as {value: [0]} and fail deep equality.
const arbJsonValue = fc.jsonValue({ maxDepth: 3 }).filter((v) => {
  // Recursively check for -0 in the value
  const hasNegativeZero = (val: unknown): boolean => {
    if (typeof val === "number") {
      return Object.is(val, -0);
    }
    if (Array.isArray(val)) {
      return val.some(hasNegativeZero);
    }
    if (val !== null && typeof val === "object") {
      return Object.values(val).some(hasNegativeZero);
    }
    return false;
  };
  return !hasNegativeZero(v);
});

// JSON Schema - more constrained than arbitrary JSON
// Note: We use undefined (field omitted) instead of null because Aeson's .:?
// treats null same as missing, so {eff_schema: null} doesn't roundtrip.
const arbJsonSchema: fc.Arbitrary<Record<string, unknown> | undefined> = fc.oneof(
  fc.constant(undefined),
  fc.record({
    type: fc.constantFrom("object", "string", "number", "boolean", "array"),
    properties: fc.option(
      fc.dictionary(fc.string({ minLength: 1, maxLength: 20 }), fc.constant({ type: "string" })),
      { nil: undefined }
    ),
  }).map(schema => {
    const result: Record<string, unknown> = { type: schema.type };
    if (schema.properties !== undefined) {
      result.properties = schema.properties;
    }
    return result;
  })
);

// SerializableEffect - matches ProtocolPropertySpec.hs:66-81
const arbLlmCompleteEffect = fc.record({
  type: fc.constant("LlmComplete" as const),
  eff_node: arbText,
  eff_system_prompt: arbText,
  eff_user_content: arbText,
  eff_schema: arbJsonSchema,
});

const arbLogInfoEffect = fc.oneof(
  // Without fields
  fc.record({
    type: fc.constant("LogInfo" as const),
    eff_message: arbText,
  }),
  // With fields
  fc.record({
    type: fc.constant("LogInfo" as const),
    eff_message: arbText,
    eff_fields: fc.dictionary(arbText, arbJsonValue),
  })
);

const arbLogErrorEffect = fc.oneof(
  // Without fields
  fc.record({
    type: fc.constant("LogError" as const),
    eff_message: arbText,
  }),
  // With fields
  fc.record({
    type: fc.constant("LogError" as const),
    eff_message: arbText,
    eff_fields: fc.dictionary(arbText, arbJsonValue),
  })
);

const arbHabiticaEffect = fc.record({
  type: fc.constant("Habitica" as const),
  eff_hab_op: fc.constantFrom(
    "GetUser",
    "ScoreTask",
    "GetTasks",
    "FetchTodos",
    "CreateTodo",
    "AddChecklistItem"
  ),
  eff_hab_payload: arbJsonValue,
});

// Note: TelegramConfirm exists in Haskell WireTypes but is not in
// TypeScript SerializableEffect union. We test only the shared types.
// TelegramSend/Receive/TryReceive are in TS but have different structure.

const arbSerializableEffect: fc.Arbitrary<SerializableEffect> = fc.oneof(
  arbLlmCompleteEffect,
  arbLogInfoEffect,
  arbLogErrorEffect,
  arbHabiticaEffect
) as fc.Arbitrary<SerializableEffect>;

// EffectResult - matches ProtocolPropertySpec.hs:103-115
// Note: We avoid generating {value: null} because Aeson's .:? treats null same
// as missing. Generate either undefined (field omitted) or a non-null value.
const arbNonNullJsonValue = arbJsonValue.filter((v) => v !== null);
const arbEffectResult: fc.Arbitrary<EffectResult> = fc.oneof(
  fc.record({
    type: fc.constant("success" as const),
    value: fc.option(arbNonNullJsonValue, { nil: undefined }),
  }),
  fc.record({ type: fc.constant("error" as const), message: arbText })
);

// ExecutionPhase - matches ProtocolPropertySpec.hs:118-126
const arbExecutionPhase: fc.Arbitrary<ExecutionPhase> = fc.oneof(
  fc.constant({ type: "idle" as const }),
  fc.record({ type: fc.constant("in_node" as const), nodeName: arbText }),
  fc.record({
    type: fc.constant("transitioning" as const),
    fromNode: arbText,
    toNode: arbText,
  }),
  fc.record({
    type: fc.constant("completed" as const),
    result: arbJsonValue,
  }),
  fc.record({ type: fc.constant("failed" as const), error: arbText })
);

// GraphState - matches ProtocolPropertySpec.hs:144-149
const arbGraphState: fc.Arbitrary<GraphState> = fc.record({
  phase: arbExecutionPhase,
  completedNodes: fc.array(arbText, { maxLength: 10 }),
});

// StepOutput - matches ProtocolPropertySpec.hs:155-161
const arbStepYield: fc.Arbitrary<StepOutput> = fc
  .tuple(arbSerializableEffect, arbGraphState)
  .map(([effect, graphState]) => ({
    effect,
    done: false as const,
    stepResult: null,
    graphState,
  }));

const arbStepDone: fc.Arbitrary<StepOutput> = fc
  .tuple(arbJsonValue, arbGraphState)
  .map(([result, graphState]) => ({
    effect: null,
    done: true as const,
    stepResult: result,
    graphState,
  }));

const arbStepFailed: fc.Arbitrary<StepOutput> = fc
  .tuple(arbText, arbGraphState)
  .map(([error, graphState]) => ({
    effect: null,
    done: true as const,
    stepResult: null,
    error,
    graphState,
  }));

const arbStepOutput: fc.Arbitrary<StepOutput> = fc.oneof(
  arbStepYield,
  arbStepDone,
  arbStepFailed
);

// =============================================================================
// Graph Info Arbitraries - matches WireTypes.hs
// =============================================================================

// TypeInfo - matches TypeInfoWire
const arbTypeInfo: fc.Arbitrary<TypeInfo> = fc.record({
  typeName: arbText,
  typeModule: arbText,
});

// GotoTarget - matches GotoTargetWire
const arbGotoTarget: fc.Arbitrary<GotoTarget> = fc.record({
  gtTarget: arbText,
  gtPayloadType: arbTypeInfo,
});

// NodeInfo - matches NodeInfoWire
const arbNodeInfo: fc.Arbitrary<NodeInfo> = fc.record({
  niName: arbText,
  niKind: fc.constantFrom("LLM" as const, "Logic" as const),
  niInput: fc.option(arbTypeInfo, { nil: null }),
  niSchema: fc.option(arbTypeInfo, { nil: null }),
  niGotoTargets: fc.array(arbGotoTarget, { maxLength: 5 }),
});

// EdgeInfo - matches EdgeInfoWire
const arbEdgeInfo: fc.Arbitrary<EdgeInfo> = fc.record({
  eiFrom: arbText,
  eiTo: arbText,
  eiPayloadType: arbTypeInfo,
});

// DetailedGraphInfo - matches GraphInfoWire
const arbDetailedGraphInfo: fc.Arbitrary<DetailedGraphInfo> = fc.record({
  name: arbText,
  entryType: arbTypeInfo,
  exitType: arbTypeInfo,
  nodes: fc.array(arbNodeInfo, { maxLength: 5 }),
  edges: fc.array(arbEdgeInfo, { maxLength: 10 }),
});

// =============================================================================
// Test Setup
// =============================================================================

let roundtrip: RoundtripExports | null = null;
const WASM_PATH = path.join(__dirname, "../../tidepool-roundtrip.wasm");

beforeAll(async () => {
  if (!fs.existsSync(WASM_PATH)) {
    console.warn(`WASM not found at: ${WASM_PATH}`);
    console.warn("To run these tests:");
    console.warn("  1. Enter nix develop .#wasm");
    console.warn("  2. Run: just build-roundtrip-wasm");
    return;
  }

  const wasmBytes = fs.readFileSync(WASM_PATH);
  // Use globalThis.WebAssembly to get the Node.js native implementation
  // (Cloudflare Workers types don't include compile())
  const NodeWebAssembly = globalThis.WebAssembly as typeof WebAssembly & {
    compile(bytes: BufferSource): Promise<WebAssembly.Module>;
  };
  const wasmModule = await NodeWebAssembly.compile(wasmBytes);
  roundtrip = await loadRoundtripModule({ wasmModule, debug: false });
});

// =============================================================================
// Property Tests
// =============================================================================

describe("Cross-boundary serialization roundtrip", () => {
  const skipIfNoWasm = () => {
    if (!roundtrip) {
      console.warn("Skipping: WASM module not loaded");
      return true;
    }
    return false;
  };

  describe("SerializableEffect", () => {
    it("roundtrips through WASM", async () => {
      if (skipIfNoWasm()) return;

      await fc.assert(
        fc.asyncProperty(arbSerializableEffect, async (effect) => {
          const inputJson = JSON.stringify(effect);
          const resultStr = await roundtrip!.roundtripSerializableEffect(
            inputJson
          );
          const result: RoundtripResult = JSON.parse(resultStr);

          expect(result.ok).toBe(true);
          expect(result.value).toEqual(effect);
        }),
        { numRuns: 100 }
      );
    });
  });

  describe("EffectResult", () => {
    it("roundtrips through WASM", async () => {
      if (skipIfNoWasm()) return;

      await fc.assert(
        fc.asyncProperty(arbEffectResult, async (effectResult) => {
          const inputJson = JSON.stringify(effectResult);
          const resultStr = await roundtrip!.roundtripEffectResult(inputJson);
          const result: RoundtripResult = JSON.parse(resultStr);

          expect(result.ok).toBe(true);
          expect(result.value).toEqual(effectResult);
        }),
        { numRuns: 100 }
      );
    });
  });

  describe("ExecutionPhase", () => {
    it("roundtrips through WASM", async () => {
      if (skipIfNoWasm()) return;

      await fc.assert(
        fc.asyncProperty(arbExecutionPhase, async (phase) => {
          const inputJson = JSON.stringify(phase);
          const resultStr = await roundtrip!.roundtripExecutionPhase(inputJson);
          const result: RoundtripResult = JSON.parse(resultStr);

          expect(result.ok).toBe(true);
          expect(result.value).toEqual(phase);
        }),
        { numRuns: 100 }
      );
    });
  });

  describe("GraphState", () => {
    it("roundtrips through WASM", async () => {
      if (skipIfNoWasm()) return;

      await fc.assert(
        fc.asyncProperty(arbGraphState, async (state) => {
          const inputJson = JSON.stringify(state);
          const resultStr = await roundtrip!.roundtripGraphState(inputJson);
          const result: RoundtripResult = JSON.parse(resultStr);

          expect(result.ok).toBe(true);
          expect(result.value).toEqual(state);
        }),
        { numRuns: 100 }
      );
    });
  });

  describe("StepOutput", () => {
    it("roundtrips through WASM", async () => {
      if (skipIfNoWasm()) return;

      await fc.assert(
        fc.asyncProperty(arbStepOutput, async (output) => {
          const inputJson = JSON.stringify(output);
          const resultStr = await roundtrip!.roundtripStepOutput(inputJson);
          const result: RoundtripResult = JSON.parse(resultStr);

          expect(result.ok).toBe(true);
          expect(result.value).toEqual(output);
        }),
        { numRuns: 100 }
      );
    });
  });

  describe("TypeInfo (wire type)", () => {
    it("roundtrips through WASM", async () => {
      if (skipIfNoWasm()) return;

      await fc.assert(
        fc.asyncProperty(arbTypeInfo, async (typeInfo) => {
          const inputJson = JSON.stringify(typeInfo);
          const resultStr = await roundtrip!.roundtripTypeInfoWire(inputJson);
          const result: RoundtripResult = JSON.parse(resultStr);

          expect(result.ok).toBe(true);
          expect(result.value).toEqual(typeInfo);
        }),
        { numRuns: 100 }
      );
    });
  });

  describe("GotoTarget (wire type)", () => {
    it("roundtrips through WASM", async () => {
      if (skipIfNoWasm()) return;

      await fc.assert(
        fc.asyncProperty(arbGotoTarget, async (gotoTarget) => {
          const inputJson = JSON.stringify(gotoTarget);
          const resultStr = await roundtrip!.roundtripGotoTargetWire(inputJson);
          const result: RoundtripResult = JSON.parse(resultStr);

          expect(result.ok).toBe(true);
          expect(result.value).toEqual(gotoTarget);
        }),
        { numRuns: 100 }
      );
    });
  });

  describe("NodeInfo (wire type)", () => {
    it("roundtrips through WASM", async () => {
      if (skipIfNoWasm()) return;

      await fc.assert(
        fc.asyncProperty(arbNodeInfo, async (nodeInfo) => {
          const inputJson = JSON.stringify(nodeInfo);
          const resultStr = await roundtrip!.roundtripNodeInfoWire(inputJson);
          const result: RoundtripResult = JSON.parse(resultStr);

          expect(result.ok).toBe(true);
          expect(result.value).toEqual(nodeInfo);
        }),
        { numRuns: 100 }
      );
    });
  });

  describe("EdgeInfo (wire type)", () => {
    it("roundtrips through WASM", async () => {
      if (skipIfNoWasm()) return;

      await fc.assert(
        fc.asyncProperty(arbEdgeInfo, async (edgeInfo) => {
          const inputJson = JSON.stringify(edgeInfo);
          const resultStr = await roundtrip!.roundtripEdgeInfoWire(inputJson);
          const result: RoundtripResult = JSON.parse(resultStr);

          expect(result.ok).toBe(true);
          expect(result.value).toEqual(edgeInfo);
        }),
        { numRuns: 100 }
      );
    });
  });

  describe("DetailedGraphInfo (wire type)", () => {
    it("roundtrips through WASM", async () => {
      if (skipIfNoWasm()) return;

      await fc.assert(
        fc.asyncProperty(arbDetailedGraphInfo, async (graphInfo) => {
          const inputJson = JSON.stringify(graphInfo);
          const resultStr = await roundtrip!.roundtripGraphInfoWire(inputJson);
          const result: RoundtripResult = JSON.parse(resultStr);

          expect(result.ok).toBe(true);
          expect(result.value).toEqual(graphInfo);
        }),
        { numRuns: 100 }
      );
    });
  });
});
