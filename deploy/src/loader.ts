/**
 * Tidepool WASM Loader for Cloudflare Workers
 *
 * Self-contained loader for the DO harness.
 */

import { WASI, File, OpenFile, ConsoleStdout } from "@bjorn3/browser_wasi_shim";
import { createJsFFI, type WasmExports } from "./jsffi.js";
import type {
  StepOutput,
  EffectResult,
  GraphInfo,
  GraphState,
} from "./protocol.js";

// =============================================================================
// GraphMachine Interface (inline to avoid circular deps)
// =============================================================================

export interface GraphMachine {
  initialize(input: unknown): Promise<StepOutput>;
  step(result: EffectResult): Promise<StepOutput>;
  getGraphInfo(): Promise<GraphInfo>;
  getGraphState(): Promise<GraphState>;
}

// =============================================================================
// Cloudflare Workers Polyfills
// =============================================================================

// MessageChannel is not available in Workers - use setTimeout
if (typeof (globalThis as Record<string, unknown>).MessageChannel === "undefined") {
  (globalThis as Record<string, unknown>).MessageChannel = class {
    port1 = { postMessage: () => {} };
    port2 = { onmessage: null };
  };
}

// setImmediate polyfill for GHC WASM scheduler
if (typeof (globalThis as Record<string, unknown>).setImmediate === "undefined") {
  (globalThis as Record<string, unknown>).setImmediate = (fn: () => void) => setTimeout(fn, 0);
}

// FinalizationRegistry - no-op for short-lived Workers
if (typeof globalThis.FinalizationRegistry === "undefined") {
  // @ts-expect-error - Minimal polyfill, Workers requests are short-lived
  globalThis.FinalizationRegistry = class {
    register() {}
    unregister() {}
  };
}

// =============================================================================
// Loader Options
// =============================================================================

export interface LoaderOptions {
  /** The compiled WASM module */
  wasmModule: WebAssembly.Module;
  /** Enable debug logging */
  debug?: boolean;
}

// =============================================================================
// Loader Implementation
// =============================================================================

export async function loadMachine(options: LoaderOptions): Promise<GraphMachine> {
  const { wasmModule, debug = false } = options;

  // Set up WASI with minimal filesystem (stdout/stderr only)
  const fds = [
    new OpenFile(new File([])), // stdin (empty)
    ConsoleStdout.lineBuffered((msg) => console.log(`[WASM stdout] ${msg}`)),
    ConsoleStdout.lineBuffered((msg) => console.error(`[WASM stderr] ${msg}`)),
  ];

  const wasi = new WASI([], [], fds, { debug });

  // Create exports object for knot-tying pattern
  const __exports: Partial<WasmExports> = {};

  // Create JSFFI imports
  const { imports: jsFFIImports } = createJsFFI(__exports);

  // Instantiate WASM module
  const instance = await WebAssembly.instantiate(wasmModule, {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: jsFFIImports as WebAssembly.ModuleImports,
  });

  // Fill in exports (completes the knot)
  Object.assign(__exports, instance.exports);
  const exports = instance.exports as unknown as WasmExports;

  // Initialize WASI for reactor module
  wasi.initialize({
    exports: {
      memory: exports.memory,
      _initialize: exports._initialize,
    },
  });

  // Initialize GHC runtime
  exports.hs_init(0, 0);

  if (debug) {
    console.log("[Tidepool] WASM module loaded");
  }

  return {
    async initialize(input: unknown): Promise<StepOutput> {
      const inputJson = JSON.stringify(input);
      if (debug) console.log("[Tidepool] initialize:", inputJson);

      const resultStr = await exports.initialize(inputJson);

      const result: StepOutput = typeof resultStr === "string"
        ? JSON.parse(resultStr)
        : { effect: null, done: true, stepResult: null, graphState: { phase: { type: "idle" } as const, completedNodes: [] } };

      if (debug) console.log("[Tidepool] initialize result:", JSON.stringify(result, null, 2));
      return result;
    },

    async step(result: EffectResult): Promise<StepOutput> {
      const resultJson = JSON.stringify(result);
      if (debug) console.log("[Tidepool] step:", resultJson);

      const outputStr = await exports.step(resultJson);

      const output: StepOutput = typeof outputStr === "string"
        ? JSON.parse(outputStr)
        : { effect: null, done: true, stepResult: null, graphState: { phase: { type: "idle" } as const, completedNodes: [] } };

      if (debug) console.log("[Tidepool] step result:", JSON.stringify(output, null, 2));
      return output;
    },

    async getGraphInfo(): Promise<GraphInfo> {
      const fn = (exports as unknown as { getGraphInfo?: () => string | Promise<string> }).getGraphInfo;
      if (!fn) {
        return { name: "Unknown", entryType: { typeName: "", typeModule: "" }, exitType: { typeName: "", typeModule: "" }, nodes: [], edges: [] };
      }
      const str = fn();
      const resolved = typeof str === "string" ? str : await str;
      return JSON.parse(resolved);
    },

    async getGraphState(): Promise<GraphState> {
      const fn = (exports as unknown as { getGraphState?: () => string | Promise<string> }).getGraphState;
      if (!fn) {
        return { phase: { type: "idle" } as const, completedNodes: [] };
      }
      const str = fn();
      const resolved = typeof str === "string" ? str : await str;
      return JSON.parse(resolved);
    },
  };
}
