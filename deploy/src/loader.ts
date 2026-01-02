/**
 * Tidepool WASM Loader for Cloudflare Workers
 *
 * Uses generated dispatcher from tidepool-generated package.
 */

import { WASI, File, OpenFile, ConsoleStdout } from "@bjorn3/browser_wasi_shim";
import { createJsFFI, type WasmExports } from "tidepool-generated-ts";
import { setupPolyfills } from "./polyfills";
import type {
  StepOutput,
  EffectResult,
  DetailedGraphInfo as GraphInfo,
  GraphState,
  GraphId,
  GraphWasmExports,
} from "tidepool-generated-ts";
import { getGraphFns } from "tidepool-generated-ts";

// =============================================================================
// GraphMachine Interface
// =============================================================================

// Re-export GraphId for consumers
export type { GraphId };

export interface GraphMachine {
  initialize(graphId: GraphId, input: unknown): Promise<StepOutput>;
  step(graphId: GraphId, result: EffectResult): Promise<StepOutput>;
  getGraphInfo(graphId: GraphId): Promise<GraphInfo>;
  getGraphState(graphId: GraphId): Promise<GraphState>;
}

// =============================================================================
// GHC WASM Runtime Polyfills
// =============================================================================

// Setup required polyfills for GHC WASM runtime
setupPolyfills();

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
  exports.initRegistry();  // Populate graph registry

  if (debug) {
    console.log("[Tidepool] WASM module loaded");
  }

  // Cast WASM exports to generated interface
  // Note: WASM functions may return Promise<string> due to GHC RTS async behavior
  const graphExports = exports as unknown as GraphWasmExports;

  const defaultStepOutput: StepOutput = {
    effect: null,
    done: true,
    stepResult: null,
    graphState: { phase: { type: "idle" } as const, completedNodes: [] }
  };

  return {
    async initialize(graphId: GraphId, input: unknown): Promise<StepOutput> {
      const fns = getGraphFns(graphId, graphExports);
      const inputJson = JSON.stringify(input);
      if (debug) console.log(`[Tidepool] initialize ${graphId}:`, inputJson);

      // Handle both sync and async returns from WASM
      const resultStr = await Promise.resolve(fns.initialize(inputJson));

      const result: StepOutput = typeof resultStr === "string"
        ? JSON.parse(resultStr)
        : defaultStepOutput;

      if (debug) console.log(`[Tidepool] initialize ${graphId} result:`, JSON.stringify(result, null, 2));
      return result;
    },

    async step(graphId: GraphId, result: EffectResult): Promise<StepOutput> {
      const fns = getGraphFns(graphId, graphExports);
      const resultJson = JSON.stringify(result);
      if (debug) console.log(`[Tidepool] step ${graphId}:`, resultJson);

      // Handle both sync and async returns from WASM
      const outputStr = await Promise.resolve(fns.step(resultJson));

      const output: StepOutput = typeof outputStr === "string"
        ? JSON.parse(outputStr)
        : defaultStepOutput;

      if (debug) console.log(`[Tidepool] step ${graphId} result:`, JSON.stringify(output, null, 2));
      return output;
    },

    async getGraphInfo(graphId: GraphId): Promise<GraphInfo> {
      const fns = getGraphFns(graphId, graphExports);
      const str = await Promise.resolve(fns.getGraphInfo());
      return JSON.parse(str);
    },

    async getGraphState(graphId: GraphId): Promise<GraphState> {
      const fns = getGraphFns(graphId, graphExports);
      const str = await Promise.resolve(fns.getGraphState());
      return JSON.parse(str);
    },
  };
}
