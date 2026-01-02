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
} from "../protocol.js";

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
const arbJsonValue = fc.jsonValue({ maxDepth: 3 });

// JSON Schema - more constrained than arbitrary JSON
const arbJsonSchema: fc.Arbitrary<Record<string, unknown> | null> = fc.oneof(
  fc.constant(null),
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

const arbLogInfoEffect = fc.record({
  type: fc.constant("LogInfo" as const),
  eff_message: arbText,
});

const arbLogErrorEffect = fc.record({
  type: fc.constant("LogError" as const),
  eff_message: arbText,
});

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
const arbEffectResult: fc.Arbitrary<EffectResult> = fc.oneof(
  fc.record({
    type: fc.constant("success" as const),
    value: fc.option(arbJsonValue, { nil: undefined }).map((v) => v ?? null),
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
});
