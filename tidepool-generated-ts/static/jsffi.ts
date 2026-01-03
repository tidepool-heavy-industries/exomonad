/**
 * GHC WASM JSFFI Implementation
 *
 * Implements the ghc_wasm_jsffi imports required by GHC's WASM backend.
 * The implementations are derived from the ghc_wasm_jsffi custom section
 * embedded in the WASM binary.
 */

export interface WasmExports {
  memory: WebAssembly.Memory;
  // Initialization exports
  _initialize: () => void;  // WASI/libc initialization (reactor mode)
  hs_init: (argc: number, argv: number) => void;  // GHC RTS initialization
  initRegistry: () => void;  // Populate graph registry
  // GHC WASM exports use externref - JS values passed directly
  initialize: (input: string) => Promise<string>;
  step: (result: string) => Promise<string>;
  // RTS exports for JSFFI
  rts_schedulerLoop: () => void;
  rts_freeStablePtr: (sp: number) => void;
  rts_promiseResolveUnit: (p: number) => void;
  rts_promiseResolveJSVal: (p: number, val: number) => void;
  rts_promiseReject: (p: number, err: unknown) => void;
  rts_promiseThrowTo: (p: number, err: unknown) => void;
}

interface PromiseWithResolvers<T> extends Promise<T> {
  resolve: (value: T) => void;
  reject: (reason: unknown) => void;
  throwTo: (err: unknown) => void;
}

/**
 * Create the ghc_wasm_jsffi import object for WebAssembly.instantiate.
 * Uses a "knot-tying" pattern - pass an empty exports object that gets
 * filled in after instantiation.
 */
// eslint-disable-next-line @typescript-eslint/no-explicit-any
type JsFFIFunction = (...args: any[]) => any;

const DEBUG_JSFFI = false;

function logCall(name: string, args: unknown[]): void {
  if (DEBUG_JSFFI) {
    console.log(`[JSFFI] ${name}(${args.map(a => typeof a === 'object' ? '[obj]' : a).join(', ')})`);
  }
}

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

export function createJsFFI(
  __exports: Partial<WasmExports>
): JsFFIResult {
  // Finalization registry for automatic cleanup of Haskell stable pointers
  const __ghc_wasm_jsffi_finalization_registry = new FinalizationRegistry(
    (sp: number) => {
      __exports.rts_freeStablePtr?.(sp);
    }
  );

  // JSVal table - maps integer handles to JavaScript values
  const jsvalTable = new Map<number, unknown>();
  let nextJsvalId = 1;

  // JSVal manipulation functions (used by both JSFFI imports and loader)
  const jsvalFunctions: JsValTable = {
    newJSVal: (val: unknown): number => {
      const id = nextJsvalId++;
      jsvalTable.set(id, val);
      return id;
    },
    freeJSVal: (id: number): void => {
      jsvalTable.delete(id);
    },
    getJSVal: (id: number): unknown => {
      return jsvalTable.get(id);
    },
  };

  // Wrap each function with logging and error catching
  function wrap<T extends JsFFIFunction>(name: string, fn: T): T {
    return ((...args: unknown[]) => {
      logCall(name, args);
      try {
        const result = fn(...args);
        if (DEBUG_JSFFI && result !== undefined) {
          console.log(`[JSFFI] ${name} => ${typeof result === 'object' ? '[obj]' : result}`);
        }
        return result;
      } catch (err) {
        console.error(`[JSFFI] ${name} ERROR:`, err);
        throw err;
      }
    }) as T;
  }

  const ffi: Record<string, JsFFIFunction> = {
    // =========================================================================
    // Core JSVal Management
    // =========================================================================

    newJSVal: jsvalFunctions.newJSVal,
    freeJSVal: jsvalFunctions.freeJSVal,
    getJSVal: jsvalFunctions.getJSVal,

    // =========================================================================
    // Scheduler
    // =========================================================================

    scheduleWork: (): void => {
      queueMicrotask(() => {
        __exports.rts_schedulerLoop?.();
      });
    },

    // =========================================================================
    // Types - String encoding/decoding
    // =========================================================================

    // ZC0: Error stringification
    "ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC":
      (err: Error): string => {
        return err.stack ? err.stack : String(err);
      },

    // ZC1: Decode UTF-8 string from WASM memory
    "ZC1ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC":
      (ptr: number, len: number): string => {
        const memory = __exports.memory!;
        const bytes = new Uint8Array(memory.buffer, ptr, len);
        return new TextDecoder("utf-8", { fatal: true } as TextDecoderConstructorOptions).decode(bytes);
      },

    // ZC2: Encode string into WASM memory, return bytes written
    "ZC2ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC":
      (str: string, ptr: number, len: number): number => {
        const memory = __exports.memory!;
        const bytes = new Uint8Array(memory.buffer, ptr, len);
        return new TextEncoder().encodeInto(str, bytes).written ?? 0;
      },

    // ZC3: Get string length
    "ZC3ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC":
      (str: string): number => str.length,

    // ZC4: Unregister from finalization registry
    "ZC4ZCghczminternalZCGHCziInternalziWasmziPrimziTypesZC":
      (val: object): void => {
        try {
          __ghc_wasm_jsffi_finalization_registry.unregister(val);
        } catch { /* ignore */ }
      },

    // =========================================================================
    // Exports - Promise operations
    // =========================================================================

    // ZC0: Reject promise with RuntimeError
    "ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC":
      (p: PromiseWithResolvers<unknown>, msg: string): void => {
        p.reject(new WebAssembly.RuntimeError(msg));
      },

    // ZC1-19: Resolve promise with typed value
    // GHC generates numbered variants for different Haskell return types.
    // All have the same JS implementation - just resolve the promise with the value.
    ...Object.fromEntries(
      Array.from({ length: 19 }, (_, i) => [
        `ZC${i + 1}ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC`,
        (p: PromiseWithResolvers<unknown>, val: unknown): void => {
          p.resolve(val);
        },
      ])
    ),

    // ZC20: Set empty throwTo handler
    "ZC20ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC":
      (p: PromiseWithResolvers<unknown>): void => {
        p.throwTo = () => {};
      },

    // ZC21: Set throwTo handler that calls rts_promiseThrowTo
    "ZC21ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC":
      (p: PromiseWithResolvers<unknown>, sp: number): void => {
        p.throwTo = (err: unknown) => {
          __exports.rts_promiseThrowTo?.(sp, err);
        };
      },

    // ZC22: Create promise with resolve/reject attached
    "ZC22ZCghczminternalZCGHCziInternalziWasmziPrimziExportsZC":
      (): PromiseWithResolvers<unknown> => {
        let resolve: (v: unknown) => void;
        let reject: (e: unknown) => void;
        const p = new Promise((res, rej) => {
          resolve = res;
          reject = rej;
        }) as PromiseWithResolvers<unknown>;
        p.resolve = resolve!;
        p.reject = reject!;
        p.throwTo = () => {};
        return p;
      },

    // =========================================================================
    // Imports - Promise await handlers
    // =========================================================================

    // ZC18: Await promise that resolves to unit
    "ZC18ZCghczminternalZCGHCziInternalziWasmziPrimziImportsZC":
      (promise: Promise<void>, sp: number): void => {
        promise.then(
          () => __exports.rts_promiseResolveUnit?.(sp),
          (err) => __exports.rts_promiseReject?.(sp, err)
        );
      },

    // =========================================================================
    // Concurrency - Delay/sleep
    // =========================================================================

    // ZC0: Async delay (microseconds)
    "ZC0ZCghczminternalZCGHCziInternalziWasmziPrimziConcziInternalZC":
      async (delayMicros: number): Promise<void> => {
        await new Promise(resolve => setTimeout(resolve, delayMicros / 1000));
      },
  };

  // Wrap all functions with logging
  const wrapped: Record<string, JsFFIFunction> = {};
  for (const [name, fn] of Object.entries(ffi)) {
    wrapped[name] = wrap(name, fn);
  }
  return { imports: wrapped, jsvalTable: jsvalFunctions };
}
