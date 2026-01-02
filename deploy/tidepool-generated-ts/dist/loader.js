/**
 * Tidepool WASM Loader for Cloudflare Workers
 *
 * Self-contained loader for the DO harness.
 *
 * The WASM module exports unified FFI functions:
 * - initialize(graphId, json) - Start a graph session
 * - step(graphId, json) - Continue with effect result
 * - getGraphInfo(graphId) - Get static graph metadata
 * - getGraphState(graphId) - Get current execution state
 */
import { WASI, File, OpenFile, ConsoleStdout } from "@bjorn3/browser_wasi_shim";
import { createJsFFI } from "./jsffi.js";
// =============================================================================
// Cloudflare Workers Polyfills
// =============================================================================
// MessageChannel is not available in Workers - use setTimeout
if (typeof globalThis.MessageChannel === "undefined") {
    globalThis.MessageChannel = class {
        port1 = { postMessage: () => { } };
        port2 = { onmessage: null };
    };
}
// setImmediate polyfill for GHC WASM scheduler
if (typeof globalThis.setImmediate === "undefined") {
    globalThis.setImmediate = (fn) => setTimeout(fn, 0);
}
// FinalizationRegistry - no-op for short-lived Workers
if (typeof globalThis.FinalizationRegistry === "undefined") {
    // @ts-expect-error - Minimal polyfill, Workers requests are short-lived
    globalThis.FinalizationRegistry = class {
        register() { }
        unregister() { }
    };
}
// =============================================================================
// Loader Implementation
// =============================================================================
export async function loadMachine(options) {
    const { wasmModule, graphId, debug = false } = options;
    // Set up WASI with minimal filesystem (stdout/stderr only)
    const fds = [
        new OpenFile(new File([])), // stdin (empty)
        ConsoleStdout.lineBuffered((msg) => console.log(`[WASM stdout] ${msg}`)),
        ConsoleStdout.lineBuffered((msg) => console.error(`[WASM stderr] ${msg}`)),
    ];
    const wasi = new WASI([], [], fds, { debug });
    // Create exports object for knot-tying pattern
    const __exports = {};
    // Create JSFFI imports
    const { imports: jsFFIImports } = createJsFFI(__exports);
    // Instantiate WASM module
    const instance = await WebAssembly.instantiate(wasmModule, {
        wasi_snapshot_preview1: wasi.wasiImport,
        ghc_wasm_jsffi: jsFFIImports,
    });
    // Fill in exports (completes the knot)
    Object.assign(__exports, instance.exports);
    const exports = instance.exports;
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
        console.log(`[Tidepool] WASM module loaded for graph: ${graphId}`);
    }
    // Verify unified FFI exports exist
    if (!exports.initialize) {
        throw new Error(`WASM module missing unified export: initialize. Is the WASM module built with unified FFI?`);
    }
    return {
        async initialize(input) {
            const inputJson = JSON.stringify(input);
            if (debug)
                console.log(`[Tidepool:${graphId}] initialize:`, inputJson);
            const resultStr = await exports.initialize(graphId, inputJson);
            const result = typeof resultStr === "string"
                ? JSON.parse(resultStr)
                : { effect: null, done: true, stepResult: null, graphState: { phase: { type: "idle" }, completedNodes: [] } };
            if (debug)
                console.log(`[Tidepool:${graphId}] initialize result:`, JSON.stringify(result, null, 2));
            return result;
        },
        async step(result) {
            const resultJson = JSON.stringify(result);
            if (debug)
                console.log(`[Tidepool:${graphId}] step:`, resultJson);
            const outputStr = await exports.step(graphId, resultJson);
            const output = typeof outputStr === "string"
                ? JSON.parse(outputStr)
                : { effect: null, done: true, stepResult: null, graphState: { phase: { type: "idle" }, completedNodes: [] } };
            if (debug)
                console.log(`[Tidepool:${graphId}] step result:`, JSON.stringify(output, null, 2));
            return output;
        },
        async getGraphInfo() {
            const str = await exports.getGraphInfo(graphId);
            return typeof str === "string"
                ? JSON.parse(str)
                : { id: graphId, name: "Unknown", nodes: [], edges: [] };
        },
        async getGraphState() {
            const str = await exports.getGraphState(graphId);
            return typeof str === "string"
                ? JSON.parse(str)
                : { phase: { type: "idle" }, completedNodes: [] };
        },
    };
}
//# sourceMappingURL=loader.js.map