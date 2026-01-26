/**
 * Minimal WASM loader for roundtrip serialization testing.
 *
 * Loads the roundtrip functions exported by exomonad-wasm for property testing.
 */

import { WASI, File, OpenFile, ConsoleStdout } from "@bjorn3/browser_wasi_shim";
import { createJsFFI, type WasmExports } from "exomonad-generated-ts";
import { setupPolyfills } from "./polyfills";

// =============================================================================
// GHC WASM Runtime Polyfills
// =============================================================================

// Setup required polyfills for GHC WASM runtime
setupPolyfills();

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
  // Graph info types
  roundtripTypeInfoWire(json: string): Promise<string>;
  roundtripGotoTargetWire(json: string): Promise<string>;
  roundtripNodeInfoWire(json: string): Promise<string>;
  roundtripEdgeInfoWire(json: string): Promise<string>;
  roundtripGraphInfoWire(json: string): Promise<string>;
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
    // Graph info types
    roundtripTypeInfoWire: (json: string) => string | Promise<string>;
    roundtripGotoTargetWire: (json: string) => string | Promise<string>;
    roundtripNodeInfoWire: (json: string) => string | Promise<string>;
    roundtripEdgeInfoWire: (json: string) => string | Promise<string>;
    roundtripGraphInfoWire: (json: string) => string | Promise<string>;
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
    // Graph info types
    roundtripTypeInfoWire: wrapRoundtrip(
      exports.roundtripTypeInfoWire,
      "roundtripTypeInfoWire"
    ),
    roundtripGotoTargetWire: wrapRoundtrip(
      exports.roundtripGotoTargetWire,
      "roundtripGotoTargetWire"
    ),
    roundtripNodeInfoWire: wrapRoundtrip(
      exports.roundtripNodeInfoWire,
      "roundtripNodeInfoWire"
    ),
    roundtripEdgeInfoWire: wrapRoundtrip(
      exports.roundtripEdgeInfoWire,
      "roundtripEdgeInfoWire"
    ),
    roundtripGraphInfoWire: wrapRoundtrip(
      exports.roundtripGraphInfoWire,
      "roundtripGraphInfoWire"
    ),
  };
}
