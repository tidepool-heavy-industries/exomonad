/**
 * GHC WASM JSFFI Implementation
 *
 * Implements the ghc_wasm_jsffi imports required by GHC's WASM backend.
 * The implementations are derived from the ghc_wasm_jsffi custom section
 * embedded in the WASM binary.
 */
const DEBUG_JSFFI = false;
function logCall(name, args) {
    if (DEBUG_JSFFI) {
        console.log(`[JSFFI] ${name}(${args.map(a => typeof a === 'object' ? '[obj]' : a).join(', ')})`);
    }
}
export function createJsFFI(__exports) {
    // Finalization registry for automatic cleanup of Haskell stable pointers
    const __ghc_wasm_jsffi_finalization_registry = new FinalizationRegistry((sp) => {
        __exports.rts_freeStablePtr?.(sp);
    });
    // JSVal table - maps integer handles to JavaScript values
    const jsvalTable = new Map();
    let nextJsvalId = 1;
    // JSVal manipulation functions (used by both JSFFI imports and loader)
    const jsvalFunctions = {
        newJSVal: (val) => {
            const id = nextJsvalId++;
            jsvalTable.set(id, val);
            return id;
        },
        freeJSVal: (id) => {
            jsvalTable.delete(id);
        },
        getJSVal: (id) => {
            return jsvalTable.get(id);
        },
    };
    // Wrap each function with logging and error catching
    function wrap(name, fn) {
        return ((...args) => {
            logCall(name, args);
            try {
                const result = fn(...args);
                if (DEBUG_JSFFI && result !== undefined) {
                    console.log(`[JSFFI] ${name} => ${typeof result === 'object' ? '[obj]' : result}`);
                }
                return result;
            }
            catch (err) {
                console.error(`[JSFFI] ${name} ERROR:`, err);
                throw err;
            }
        });
    }
    const ffi = {
        // =========================================================================
        // Core JSVal Management
        // =========================================================================
        newJSVal: jsvalFunctions.newJSVal,
        freeJSVal: jsvalFunctions.freeJSVal,
        getJSVal: jsvalFunctions.getJSVal,
        // =========================================================================
        // Scheduler
        // =========================================================================
        scheduleWork: () => {
            queueMicrotask(() => {
                __exports.rts_schedulerLoop?.();
            });
        },
        // =========================================================================
        // Types - String encoding/decoding
        // =========================================================================
        // ZC0: Error stringification
        "ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC": (err) => {
            return err.stack ? err.stack : String(err);
        },
        // ZC1: Decode UTF-8 string from WASM memory
        "ZC1ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC": (ptr, len) => {
            const memory = __exports.memory;
            const bytes = new Uint8Array(memory.buffer, ptr, len);
            return new TextDecoder("utf-8", { fatal: true }).decode(bytes);
        },
        // ZC2: Encode string into WASM memory, return bytes written
        "ZC2ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC": (str, ptr, len) => {
            const memory = __exports.memory;
            const bytes = new Uint8Array(memory.buffer, ptr, len);
            return new TextEncoder().encodeInto(str, bytes).written ?? 0;
        },
        // ZC3: Get string length
        "ZC3ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC": (str) => str.length,
        // ZC4: Unregister from finalization registry
        "ZC4ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC": (val) => {
            try {
                __ghc_wasm_jsffi_finalization_registry.unregister(val);
            }
            catch { /* ignore */ }
        },
        // =========================================================================
        // Exports - Promise operations
        // =========================================================================
        // ZC0: Reject promise with RuntimeError
        "ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC": (p, msg) => {
            p.reject(new WebAssembly.RuntimeError(msg));
        },
        // ZC1-18: Resolve promise with typed value (all same implementation)
        "ZC18ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC": (p, val) => {
            p.resolve(val);
        },
        // ZC20: Set empty throwTo handler
        "ZC20ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC": (p) => {
            p.throwTo = () => { };
        },
        // ZC21: Set throwTo handler that calls rts_promiseThrowTo
        "ZC21ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC": (p, sp) => {
            p.throwTo = (err) => {
                __exports.rts_promiseThrowTo?.(sp, err);
            };
        },
        // ZC22: Create promise with resolve/reject attached
        "ZC22ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC": () => {
            let resolve;
            let reject;
            const p = new Promise((res, rej) => {
                resolve = res;
                reject = rej;
            });
            p.resolve = resolve;
            p.reject = reject;
            p.throwTo = () => { };
            return p;
        },
        // =========================================================================
        // Imports - Promise await handlers
        // =========================================================================
        // ZC18: Await promise that resolves to unit
        "ZC18ZCghczminternalZCGHCziInternalziWasmziPrimziImportsZC": (promise, sp) => {
            promise.then(() => __exports.rts_promiseResolveUnit?.(sp), (err) => __exports.rts_promiseReject?.(sp, err));
        },
        // =========================================================================
        // Concurrency - Delay/sleep
        // =========================================================================
        // ZC0: Async delay (microseconds)
        "ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziConcziInternalZC": async (delayMicros) => {
            await new Promise(resolve => setTimeout(resolve, delayMicros / 1000));
        },
    };
    // Wrap all functions with logging
    const wrapped = {};
    for (const [name, fn] of Object.entries(ffi)) {
        wrapped[name] = wrap(name, fn);
    }
    return { imports: wrapped, jsvalTable: jsvalFunctions };
}
//# sourceMappingURL=jsffi.js.map