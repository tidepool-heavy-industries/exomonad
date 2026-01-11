/**
 * Layout Manager
 *
 * Manages decorator positions, collision detection, and auto-minimize logic.
 * Ensures the visualization doesn't become cluttered with too many expanded windows.
 */

import type {
  NodeDecorator,
  NodePosition,
  LayoutConstraints,
} from "../types";
import { visualizationBus } from "../event-bus";

/** Default layout constraints (consumers typically override in their options) */
const DEFAULT_CONSTRAINTS: LayoutConstraints = {
  viewportBounds: new DOMRect(0, 0, window.innerWidth, window.innerHeight),
  maxVisibleDecorators: 5,
  minimizeThreshold: 100,
  collisionPadding: 10,
};

/**
 * Manages layout of decorators to prevent visual clutter.
 *
 * Responsibilities:
 * - Track all active decorators
 * - Detect collisions between expanded windows
 * - Auto-minimize lower priority decorators when crowded
 * - Cull off-screen decorators
 */
export class LayoutManager {
  private decorators: Map<string, NodeDecorator> = new Map();
  private nodeToDecorators: Map<string, Set<string>> = new Map();
  private constraints: LayoutConstraints;
  private layoutScheduled = false;

  /** Bound resize handler for cleanup */
  private handleResize = (): void => {
    this.constraints.viewportBounds = new DOMRect(
      0,
      0,
      window.innerWidth,
      window.innerHeight
    );
    this.scheduleLayout();
  };

  constructor(constraints: Partial<LayoutConstraints> = {}) {
    this.constraints = { ...DEFAULT_CONSTRAINTS, ...constraints };
    window.addEventListener("resize", this.handleResize);
  }

  /**
   * Register a decorator for layout management.
   */
  register(decorator: NodeDecorator): void {
    this.decorators.set(decorator.id, decorator);

    // Track by node ID for quick lookup
    if (!this.nodeToDecorators.has(decorator.nodeId)) {
      this.nodeToDecorators.set(decorator.nodeId, new Set());
    }
    this.nodeToDecorators.get(decorator.nodeId)!.add(decorator.id);

    this.scheduleLayout();
  }

  /**
   * Unregister a decorator from layout management.
   */
  unregister(decoratorId: string): void {
    const decorator = this.decorators.get(decoratorId);
    if (!decorator) return;

    this.decorators.delete(decoratorId);

    const nodeDecorators = this.nodeToDecorators.get(decorator.nodeId);
    if (nodeDecorators) {
      nodeDecorators.delete(decoratorId);
      if (nodeDecorators.size === 0) {
        this.nodeToDecorators.delete(decorator.nodeId);
      }
    }

    this.scheduleLayout();
  }

  /**
   * Get all decorators for a specific node.
   */
  getDecoratorsForNode(nodeId: string): NodeDecorator[] {
    const ids = this.nodeToDecorators.get(nodeId);
    if (!ids) return [];

    return Array.from(ids)
      .map((id) => this.decorators.get(id))
      .filter((d): d is NodeDecorator => d !== undefined);
  }

  /**
   * Update positions for all decorators of a node.
   */
  updateNodePosition(pos: NodePosition): void {
    const decorators = this.getDecoratorsForNode(pos.nodeId);
    for (const decorator of decorators) {
      decorator.updatePosition(pos);
    }
    this.scheduleLayout();
  }

  /**
   * Batch update positions for multiple nodes.
   */
  updatePositions(positions: NodePosition[]): void {
    for (const pos of positions) {
      const decorators = this.getDecoratorsForNode(pos.nodeId);
      for (const decorator of decorators) {
        decorator.updatePosition(pos);
      }
    }
    this.scheduleLayout();
  }

  /**
   * Schedule a layout pass (debounced).
   */
  private scheduleLayout(): void {
    if (this.layoutScheduled) return;
    this.layoutScheduled = true;

    requestAnimationFrame(() => {
      this.layoutScheduled = false;
      this.performLayout();
    });
  }

  /**
   * Perform the layout pass.
   */
  private performLayout(): void {
    const expanded = this.getExpandedDecorators();

    // Check viewport culling
    this.cullOffscreen(expanded);

    // Check max visible limit
    this.enforceMaxVisible(expanded);

    // Check collisions
    this.resolveCollisions(expanded);
  }

  /**
   * Get all expanded decorators, sorted by priority (highest first).
   */
  private getExpandedDecorators(): NodeDecorator[] {
    return Array.from(this.decorators.values())
      .filter((d) => d.getVisualState() === "expanded")
      .sort((a, b) => b.priority - a.priority);
  }

  /**
   * Hide decorators that are fully outside the viewport.
   * Skips pinned decorators.
   */
  private cullOffscreen(expanded: NodeDecorator[]): void {
    const vp = this.constraints.viewportBounds;

    for (const decorator of expanded) {
      // Don't auto-minimize pinned decorators
      if (decorator.isPinned) continue;

      const bounds = decorator.getBounds();
      if (!bounds) continue;

      // Check if completely outside viewport
      const isOffscreen =
        bounds.right < vp.left ||
        bounds.left > vp.right ||
        bounds.bottom < vp.top ||
        bounds.top > vp.bottom;

      if (isOffscreen) {
        // Minimize instead of hiding completely - user can still see icon
        decorator.setVisualState("minimized");
      }
    }
  }

  /**
   * Enforce maximum number of visible expanded decorators.
   * Skips pinned decorators.
   */
  private enforceMaxVisible(expanded: NodeDecorator[]): void {
    // Filter out pinned decorators - they get to stay expanded
    const autoExpanded = expanded.filter((d) => !d.isPinned);

    // If we have more auto-expanded than allowed, minimize the lowest priority ones
    if (autoExpanded.length > this.constraints.maxVisibleDecorators) {
      const toMinimize = autoExpanded.slice(this.constraints.maxVisibleDecorators);

      for (const decorator of toMinimize) {
        decorator.setVisualState("minimized");
      }

      // Emit crowded event for potential UI feedback
      visualizationBus.emit("layout:crowded", {
        nodeIds: toMinimize.map((d) => d.nodeId),
      });
    }
  }

  /**
   * Detect and resolve collisions between expanded decorators.
   * Only auto-minimizes non-pinned decorators.
   */
  private resolveCollisions(expanded: NodeDecorator[]): void {
    const padding = this.constraints.collisionPadding;
    const colliding = new Set<string>();

    // O(n^2) collision detection - fine for small numbers of decorators
    for (let i = 0; i < expanded.length; i++) {
      const a = expanded[i];
      const boundsA = a.getBounds();
      if (!boundsA) continue;

      for (let j = i + 1; j < expanded.length; j++) {
        const b = expanded[j];
        const boundsB = b.getBounds();
        if (!boundsB) continue;

        if (this.rectsOverlap(boundsA, boundsB, padding)) {
          // Skip if both are pinned - user manages their own layout
          if (a.isPinned && b.isPinned) {
            continue; // Both pinned, don't auto-minimize either
          }

          // Prefer minimizing non-pinned decorators
          if (a.isPinned) {
            colliding.add(b.id);
          } else if (b.isPinned) {
            colliding.add(a.id);
          } else {
            // Neither pinned: lower priority gets minimized
            if (a.priority < b.priority) {
              colliding.add(a.id);
            } else {
              colliding.add(b.id);
            }
          }
        }
      }
    }

    // Minimize colliding decorators (only non-pinned ones)
    for (const id of colliding) {
      const decorator = this.decorators.get(id);
      if (decorator && !decorator.isPinned) {
        decorator.setVisualState("minimized");
      }
    }
  }

  /**
   * Check if two rectangles overlap with padding.
   */
  private rectsOverlap(a: DOMRect, b: DOMRect, padding: number): boolean {
    return !(
      a.right + padding < b.left ||
      b.right + padding < a.left ||
      a.bottom + padding < b.top ||
      b.bottom + padding < a.top
    );
  }

  /**
   * Force expand a specific decorator (user action).
   * Pins it so it won't be auto-minimized.
   */
  expandDecorator(decoratorId: string): void {
    const decorator = this.decorators.get(decoratorId);
    if (!decorator) return;

    // Pin so it won't be auto-minimized
    decorator.pin();

    // Expand the requested decorator
    decorator.setVisualState("expanded");
  }

  /**
   * Update layout constraints.
   */
  setConstraints(constraints: Partial<LayoutConstraints>): void {
    this.constraints = { ...this.constraints, ...constraints };
    this.scheduleLayout();
  }

  /**
   * Get current number of expanded decorators.
   */
  getExpandedCount(): number {
    return Array.from(this.decorators.values()).filter(
      (d) => d.getVisualState() === "expanded"
    ).length;
  }

  /**
   * Get all registered decorators.
   */
  getAllDecorators(): NodeDecorator[] {
    return Array.from(this.decorators.values());
  }

  /**
   * Clean up all decorators and listeners.
   */
  dispose(): void {
    window.removeEventListener("resize", this.handleResize);

    for (const decorator of this.decorators.values()) {
      decorator.dispose();
    }
    this.decorators.clear();
    this.nodeToDecorators.clear();
  }
}
