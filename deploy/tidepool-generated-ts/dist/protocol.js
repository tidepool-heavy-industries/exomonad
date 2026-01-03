/**
 * Protocol types for WASM ↔ TypeScript communication.
 *
 * Design principle: TypeScript is a graph-aware effect executor.
 * - Haskell owns: graph structure, DAG ordering, Needs resolution, Goto/exitWith
 * - TypeScript owns: domain-specific effects (LLM, Habitica), persistence, logging, observability
 * - No general-purpose primitives (HTTP fetch) - only domain-specific effects
 *
 * These types match the Haskell Serializable.hs and Info.hs modules.
 */
// Helper to extract current node from phase (for observability)
export function getCurrentNode(phase) {
    if (phase.type === "in_node")
        return phase.nodeName;
    if (phase.type === "transitioning")
        return phase.fromNode;
    return null;
}
// Helper to create success result
export function successResult(value) {
    return { type: "success", value };
}
// Helper to create error result (generic, for backwards compatibility)
export function errorResult(message) {
    return { type: "error", message };
}
// Helper to create typed LLM error result
export function llmErrorResult(code, message) {
    return { type: "error", message, error_code: code };
}
/** Create a unit result. */
export function telegramUnitResult() {
    return { type: 'unit' };
}
/** Create a messages result. */
export function telegramMessagesResult(messages) {
    return { type: 'messages', messages };
}
/** Session timeout in milliseconds (5 minutes) */
export const SESSION_TIMEOUT_MS = 5 * 60 * 1000;
//# sourceMappingURL=protocol.js.map