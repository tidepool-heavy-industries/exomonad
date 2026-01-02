import type { SerializableEffect } from './protocol.js';
export type EffectCategory = 'internal' | 'yielded';
export type EffectSemantics = 'fire_and_forget' | 'blocking';
export declare function getEffectCategory(effect: SerializableEffect): EffectCategory;
export declare function getEffectSemantics(effect: SerializableEffect): EffectSemantics;
export declare function isYieldedEffect(effect: SerializableEffect): boolean;
export declare function isInternalEffect(effect: SerializableEffect): boolean;
export declare function isBlockingEffect(effect: SerializableEffect): boolean;
//# sourceMappingURL=routing.d.ts.map