/**
 * GHC WASM JSFFI Implementation
 *
 * Implements the ghc_wasm_jsffi imports required by GHC's WASM backend.
 * The implementations are derived from the ghc_wasm_jsffi custom section
 * embedded in the WASM binary.
 */
export interface WasmExports {
    memory: WebAssembly.Memory;
    _initialize: () => void;
    hs_init: (argc: number, argv: number) => void;
    initRegistry: () => void;
    initialize: (input: string) => Promise<string>;
    step: (result: string) => Promise<string>;
    rts_schedulerLoop: () => void;
    rts_freeStablePtr: (sp: number) => void;
    rts_promiseResolveUnit: (p: number) => void;
    rts_promiseResolveJSVal: (p: number, val: number) => void;
    rts_promiseReject: (p: number, err: unknown) => void;
    rts_promiseThrowTo: (p: number, err: unknown) => void;
}
/**
 * Create the ghc_wasm_jsffi import object for WebAssembly.instantiate.
 * Uses a "knot-tying" pattern - pass an empty exports object that gets
 * filled in after instantiation.
 */
type JsFFIFunction = (...args: any[]) => any;
/** Access to the JSVal table for the loader */
export interface JsValTable {
    newJSVal(val: unknown): number;
    freeJSVal(id: number): void;
    getJSVal(id: number): unknown;
}
export interface JsFFIResult {
    imports: Record<string, JsFFIFunction>;
    jsvalTable: JsValTable;
}
export declare function createJsFFI(__exports: Partial<WasmExports>): JsFFIResult;
export {};
//# sourceMappingURL=jsffi.d.ts.map