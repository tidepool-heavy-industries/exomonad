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
import type { StepOutput, EffectResult, GraphState } from "./protocol.js";
import type { GraphInfo } from "./graphs.js";
export interface GraphMachine {
    initialize(input: unknown): Promise<StepOutput>;
    step(result: EffectResult): Promise<StepOutput>;
    getGraphInfo(): Promise<GraphInfo>;
    getGraphState(): Promise<GraphState>;
}
export interface LoaderOptions {
    /** The compiled WASM module */
    wasmModule: WebAssembly.Module;
    /** Graph ID to load (e.g., "test", "example", "habitica") */
    graphId: string;
    /** Enable debug logging */
    debug?: boolean;
}
export declare function loadMachine(options: LoaderOptions): Promise<GraphMachine>;
//# sourceMappingURL=loader.d.ts.map