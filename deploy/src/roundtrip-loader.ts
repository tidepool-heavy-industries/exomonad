/**
 * Minimal WASM loader for roundtrip serialization testing.
 *
 * Loads the roundtrip functions exported by tidepool-wasm for property testing.
 */

import { WASI, File, OpenFile, ConsoleStdout } from "@bjorn3/browser_wasi_shim";
import { createJsFFI, type WasmExports } from "tidepool-generated";

// =============================================================================
// Polyfills (same as loader.ts)
// =============================================================================

// MessageChannel is not available in Workers/Node test env
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

// FinalizationRegistry - no-op for short-lived tests
if (typeof globalThis.FinalizationRegistry === "undefined") {
  // @ts-expect-error - Minimal polyfill
  globalThis.FinalizationRegistry = class {
    register() {}
    unregister() {}
  };
}

// =============================================================================
// Types
// =============================================================================

/** Result from roundtrip functions */
export interface RoundtripResult {
  ok: boolean;
  value?: unknown;
  error?: string;
}

/** Roundtrip function exports from WASM */
export interface RoundtripExports {
  roundtripSerializableEffect(json: string): Promise<string>;
  roundtripEffectResult(json: string): Promise<string>;
  roundtripExecutionPhase(json: string): Promise<string>;
  roundtripGraphState(json: string): Promise<string>;
  roundtripStepOutput(json: string): Promise<string>;
}

// =============================================================================
// Loader
// =============================================================================

export interface RoundtripLoaderOptions {
  /** The compiled WASM module */
  wasmModule: WebAssembly.Module;
  /** Enable debug logging */
  debug?: boolean;
}

/**
 * Load the roundtrip WASM module and return the exported functions.
 */
export async function loadRoundtripModule(
  options: RoundtripLoaderOptions
): Promise<RoundtripExports> {
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

  type RawExports = WasmExports & {
    roundtripSerializableEffect: (json: string) => string | Promise<string>;
    roundtripEffectResult: (json: string) => string | Promise<string>;
    roundtripExecutionPhase: (json: string) => string | Promise<string>;
    roundtripGraphState: (json: string) => string | Promise<string>;
    roundtripStepOutput: (json: string) => string | Promise<string>;
  };

  const exports = instance.exports as unknown as RawExports;

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
    console.log("[Roundtrip] WASM module loaded");
  }

  // Helper to wrap a roundtrip function
  const wrapRoundtrip = (
    fn: (json: string) => string | Promise<string>,
    name: string
  ) => {
    return async (json: string): Promise<string> => {
      if (debug) console.log(`[Roundtrip] ${name}:`, json.slice(0, 100));
      const result = fn(json);
      const resolved = typeof result === "string" ? result : await result;
      if (debug) console.log(`[Roundtrip] ${name} result:`, resolved.slice(0, 100));
      return resolved;
    };
  };

  return {
    roundtripSerializableEffect: wrapRoundtrip(
      exports.roundtripSerializableEffect,
      "roundtripSerializableEffect"
    ),
    roundtripEffectResult: wrapRoundtrip(
      exports.roundtripEffectResult,
      "roundtripEffectResult"
    ),
    roundtripExecutionPhase: wrapRoundtrip(
      exports.roundtripExecutionPhase,
      "roundtripExecutionPhase"
    ),
    roundtripGraphState: wrapRoundtrip(
      exports.roundtripGraphState,
      "roundtripGraphState"
    ),
    roundtripStepOutput: wrapRoundtrip(
      exports.roundtripStepOutput,
      "roundtripStepOutput"
    ),
  };
}
