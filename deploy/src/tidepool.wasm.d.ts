/**
 * Stub type declaration for WASM module import.
 *
 * This file exists solely for Wrangler build-time type checking.
 * It provides minimal typing for `import wasmModule from "./tidepool.wasm"`.
 *
 * **Important**: The actual WASM exports (initialize, step, getGraphInfo, etc.)
 * are typed via `tidepool-generated-ts` package, which provides runtime types
 * generated from the Haskell codebase. See loader.ts for usage.
 *
 * At build time, Wrangler processes the .wasm import and produces a WebAssembly.Module.
 * At runtime, we instantiate this module with WASI and JSFFI bindings to get the typed exports.
 */
declare const wasmModule: WebAssembly.Module;
export default wasmModule;
