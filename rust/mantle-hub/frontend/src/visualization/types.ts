/**
 * Visualization Layer Types
 *
 * Core interfaces for the extensible node visualization system.
 * Designed for future expansion: sprites, themes, personality.
 */

import type { NodeState, StreamEvent } from "../types";

// =============================================================================
// Position Types
// =============================================================================

/** Position data synced from D3 simulation */
export interface NodePosition {
  nodeId: string;
  x: number;
  y: number;
  state: NodeState;
}

/** Anchor point for decorator positioning relative to node */
export type DecoratorAnchor = "top" | "bottom" | "left" | "right" | "center";

/** Offset from anchor point */
export interface AnchorOffset {
  anchor: DecoratorAnchor;
  offsetX: number;
  offsetY: number;
}

// =============================================================================
// Decorator Types
// =============================================================================

/** Visual state of a decorator */
export type DecoratorVisualState =
  | "expanded" // Full content visible
  | "minimized" // Icon only
  | "hidden" // Not rendered
  | "attention"; // Needs user attention (new content)

/** Base interface for all node decorators */
export interface NodeDecorator {
  /** Unique decorator instance ID */
  readonly id: string;
  /** ID of the node this decorator is attached to */
  readonly nodeId: string;
  /** Render priority - higher renders on top */
  readonly priority: number;
  /** Whether user has pinned this decorator (won't auto-minimize) */
  readonly isPinned: boolean;

  // Lifecycle
  attach(container: HTMLElement): void;
  detach(): void;
  dispose(): void;

  // Position sync
  updatePosition(pos: NodePosition): void;

  // State management
  getVisualState(): DecoratorVisualState;
  setVisualState(state: DecoratorVisualState): void;

  // Pinning (user intent to keep expanded)
  pin(): void;
  unpin(): void;

  // Bounds for collision detection
  getBounds(): DOMRect | null;
}

/** Factory for creating decorators - enables plugin pattern */
export interface DecoratorFactory<T extends NodeDecorator = NodeDecorator> {
  /** Unique type identifier */
  readonly type: string;

  /** Create a new decorator instance for a node */
  create(nodeId: string, config?: DecoratorConfig): T;

  /** Determine if this factory should create a decorator for the given node */
  shouldAttach(node: NodeSnapshot): boolean;
}

/** Configuration passed to decorator factories */
export interface DecoratorConfig {
  anchor?: AnchorOffset;
  priority?: number;
  [key: string]: unknown;
}

/** Snapshot of node data for decorator decisions */
export interface NodeSnapshot {
  id: string;
  state: NodeState;
  branch: string;
  sessionId?: string;
}

// =============================================================================
// Event Bus Types
// =============================================================================

/** Events emitted by the visualization layer */
export interface VisualizationEvents {
  /** Node positions updated from D3 simulation tick */
  "position:update": NodePosition[];

  /** New StreamEvent received for a node */
  "node:event": {
    nodeId: string;
    event: StreamEvent;
    timestamp: string;
  };

  /** Node state changed (running → completed, etc.) */
  "node:state-change": {
    nodeId: string;
    oldState: NodeState;
    newState: NodeState;
  };

  /** Node created */
  "node:created": NodeSnapshot;

  /** Node removed */
  "node:removed": { nodeId: string };

  /** Decorator attached to a node */
  "decorator:attached": {
    nodeId: string;
    decorator: NodeDecorator;
  };

  /** Decorator detached from a node */
  "decorator:detached": {
    nodeId: string;
    decoratorId: string;
  };

  /** Decorator state changed */
  "decorator:state-change": {
    decoratorId: string;
    oldState: DecoratorVisualState;
    newState: DecoratorVisualState;
  };

  /** Layout detected crowded area - nodes that should minimize */
  "layout:crowded": { nodeIds: string[] };
}

/** Event handler function type */
export type EventHandler<K extends keyof VisualizationEvents> = (
  data: VisualizationEvents[K]
) => void;

// =============================================================================
// Layout Types
// =============================================================================

/** Constraints for layout management */
export interface LayoutConstraints {
  /** Viewport bounds for culling */
  viewportBounds: DOMRect;
  /** Maximum number of expanded decorators */
  maxVisibleDecorators: number;
  /** Distance threshold for auto-minimize */
  minimizeThreshold: number;
  /** Padding between decorators for collision */
  collisionPadding: number;
}

// =============================================================================
// Theme Types (Future Extension Point)
// =============================================================================

/** Theme definition for visualization layer */
export interface VisualizationTheme {
  id: string;
  name: string;

  /** CSS custom property overrides */
  cssVariables: Record<string, string>;

  /** Decorator styling */
  decorators: {
    borderRadius: string;
    shadow: string;
    backdropFilter?: string;
  };

  /** Future: sprite configuration */
  sprites?: {
    enabled: boolean;
    spriteSheet: string;
    frameWidth: number;
    frameHeight: number;
  };

  /** Future: personality layer */
  personality?: {
    idleAnimations: boolean;
    reactions: boolean;
  };
}
