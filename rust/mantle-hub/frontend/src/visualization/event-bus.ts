/**
 * Typed Event Bus
 *
 * Central pub/sub system for decoupling visualization components.
 * Type-safe: events and their payloads are fully typed.
 */

import type { VisualizationEvents, EventHandler } from "./types";

type EventMap = {
  [K in keyof VisualizationEvents]: Set<EventHandler<K>>;
};

/**
 * Typed event bus for visualization layer communication.
 *
 * Usage:
 * ```typescript
 * const bus = new EventBus();
 *
 * // Subscribe
 * const unsub = bus.on("node:event", (data) => {
 *   console.log(data.nodeId, data.event);
 * });
 *
 * // Emit
 * bus.emit("node:event", { nodeId: "abc", event: {...}, timestamp: "..." });
 *
 * // Unsubscribe
 * unsub();
 * ```
 */
export class EventBus {
  private listeners: Partial<EventMap> = {};

  /**
   * Subscribe to an event type.
   * @returns Unsubscribe function
   */
  on<K extends keyof VisualizationEvents>(
    event: K,
    handler: EventHandler<K>
  ): () => void {
    if (!this.listeners[event]) {
      this.listeners[event] = new Set() as EventMap[K];
    }

    const handlers = this.listeners[event] as Set<EventHandler<K>>;
    handlers.add(handler);

    // Return unsubscribe function
    return () => {
      handlers.delete(handler);
      if (handlers.size === 0) {
        delete this.listeners[event];
      }
    };
  }

  /**
   * Subscribe to an event type for one emission only.
   */
  once<K extends keyof VisualizationEvents>(
    event: K,
    handler: EventHandler<K>
  ): () => void {
    const wrapper: EventHandler<K> = (data) => {
      unsub();
      handler(data);
    };
    const unsub = this.on(event, wrapper);
    return unsub;
  }

  /**
   * Emit an event to all subscribers.
   */
  emit<K extends keyof VisualizationEvents>(
    event: K,
    data: VisualizationEvents[K]
  ): void {
    const handlers = this.listeners[event] as Set<EventHandler<K>> | undefined;
    if (!handlers) return;

    // Clone to allow handlers to unsubscribe during iteration
    for (const handler of [...handlers]) {
      try {
        handler(data);
      } catch (err) {
        console.error(`EventBus: Error in handler for "${event}":`, err);
      }
    }
  }

  /**
   * Remove all listeners for an event type.
   */
  off<K extends keyof VisualizationEvents>(event: K): void {
    delete this.listeners[event];
  }

  /**
   * Remove all listeners.
   */
  clear(): void {
    this.listeners = {};
  }

  /**
   * Get count of listeners for an event type.
   */
  listenerCount<K extends keyof VisualizationEvents>(event: K): number {
    return this.listeners[event]?.size ?? 0;
  }
}

/** Singleton instance for global use */
export const visualizationBus = new EventBus();
