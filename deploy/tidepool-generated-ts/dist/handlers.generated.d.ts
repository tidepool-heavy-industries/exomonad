import type { SerializableEffect, EffectResult } from './protocol.js';
export type InternalEffectType = "LogInfo" | "LogError" | "LlmComplete" | "LlmCall" | "Habitica" | "GetState" | "SetState" | "RandomInt" | "GetTime";
/**
 * Handler interface for internal effects.
 *
 * TEnv is the environment type (e.g., Cloudflare Worker Env).
 * Each handler receives the specific effect type and environment.
 */
export interface InternalEffectHandlers<TEnv> {
    LogInfo: (effect: Extract<SerializableEffect, {
        type: "LogInfo";
    }>, env: TEnv) => Promise<EffectResult>;
    LogError: (effect: Extract<SerializableEffect, {
        type: "LogError";
    }>, env: TEnv) => Promise<EffectResult>;
    LlmComplete: (effect: Extract<SerializableEffect, {
        type: "LlmComplete";
    }>, env: TEnv) => Promise<EffectResult>;
    LlmCall: (effect: Extract<SerializableEffect, {
        type: "LlmCall";
    }>, env: TEnv) => Promise<EffectResult>;
    Habitica: (effect: Extract<SerializableEffect, {
        type: "Habitica";
    }>, env: TEnv) => Promise<EffectResult>;
    GetState: (effect: Extract<SerializableEffect, {
        type: "GetState";
    }>, env: TEnv) => Promise<EffectResult>;
    SetState: (effect: Extract<SerializableEffect, {
        type: "SetState";
    }>, env: TEnv) => Promise<EffectResult>;
    RandomInt: (effect: Extract<SerializableEffect, {
        type: "RandomInt";
    }>, env: TEnv) => Promise<EffectResult>;
    GetTime: (effect: Extract<SerializableEffect, {
        type: "GetTime";
    }>, env: TEnv) => Promise<EffectResult>;
}
/**
 * Dispatch an internal effect to the appropriate handler.
 *
 * @param effect - The effect to execute
 * @param handlers - Registry of handler functions
 * @param env - Environment with bindings
 * @returns Promise resolving to success/error result
 */
export declare function dispatchInternalEffect<TEnv>(effect: SerializableEffect, handlers: InternalEffectHandlers<TEnv>, env: TEnv): Promise<EffectResult>;
//# sourceMappingURL=handlers.generated.d.ts.map