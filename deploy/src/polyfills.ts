/**
 * GHC WASM Runtime Polyfills
 *
 * Provides minimal polyfills for GHC WASM runtime in Cloudflare Workers.
 * These polyfills are needed because:
 * - MessageChannel is not available in Workers or Node test environments
 * - setImmediate is required by the GHC WASM scheduler
 * - FinalizationRegistry is not needed in short-lived Workers requests
 */

/**
 * Setup all required polyfills for GHC WASM runtime.
 *
 * Call this before loading any WASM module.
 */
export function setupPolyfills(): void {
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
}
