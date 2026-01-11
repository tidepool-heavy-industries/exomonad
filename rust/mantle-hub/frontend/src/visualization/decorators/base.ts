/**
 * Base Decorator
 *
 * Abstract base class for all node decorators.
 * Handles positioning, state management, and lifecycle.
 */

import type {
  NodeDecorator,
  DecoratorVisualState,
  NodePosition,
  AnchorOffset,
} from "../types";

/** Default anchor configuration */
const DEFAULT_ANCHOR: AnchorOffset = {
  anchor: "top",
  offsetX: 0,
  offsetY: -20,
};

/** Node radius for anchor calculations */
const NODE_RADIUS = 20;

/**
 * Abstract base class for node decorators.
 *
 * Subclasses must implement:
 * - `createElement()`: Create the decorator's DOM element
 * - `onStateChange()`: Handle visual state transitions
 */
export abstract class BaseDecorator implements NodeDecorator {
  readonly id: string;
  readonly nodeId: string;
  readonly priority: number;

  protected element: HTMLElement | null = null;
  protected container: HTMLElement | null = null;
  protected anchor: AnchorOffset;
  protected visualState: DecoratorVisualState = "expanded";
  protected currentPosition: NodePosition | null = null;

  constructor(nodeId: string, priority = 0, anchor?: Partial<AnchorOffset>) {
    this.id = `decorator-${nodeId}-${Date.now()}-${Math.random().toString(36).slice(2, 6)}`;
    this.nodeId = nodeId;
    this.priority = priority;
    this.anchor = { ...DEFAULT_ANCHOR, ...anchor };
  }

  /**
   * Attach the decorator to a container element.
   */
  attach(container: HTMLElement): void {
    if (this.element) {
      console.warn(`Decorator ${this.id} already attached`);
      return;
    }

    this.container = container;
    this.element = this.createElement();
    this.element.id = this.id;
    this.element.dataset.nodeId = this.nodeId;
    this.element.classList.add(
      "node-decorator",
      `decorator-${this.visualState}`
    );
    this.element.style.position = "absolute";
    this.element.style.zIndex = String(this.priority);

    container.appendChild(this.element);

    // Apply initial position if we have one
    if (this.currentPosition) {
      this.updatePosition(this.currentPosition);
    }
  }

  /**
   * Detach the decorator from its container.
   */
  detach(): void {
    if (this.element) {
      this.element.remove();
      this.element = null;
    }
    this.container = null;
  }

  /**
   * Update the decorator's position based on node position.
   */
  updatePosition(pos: NodePosition): void {
    this.currentPosition = pos;
    if (!this.element) return;

    const { x, y } = this.computeAnchoredPosition(pos);

    // Use transform for smooth positioning
    this.element.style.transform = `translate(${x}px, ${y}px)`;
  }

  /**
   * Compute the decorated position based on anchor point.
   */
  protected computeAnchoredPosition(pos: NodePosition): { x: number; y: number } {
    const { anchor, offsetX, offsetY } = this.anchor;

    // Get element dimensions for centering (estimate if not rendered yet)
    const width = this.element?.offsetWidth ?? 300;
    const height = this.element?.offsetHeight ?? 200;

    let x = pos.x;
    let y = pos.y;

    switch (anchor) {
      case "top":
        x = pos.x - width / 2;
        y = pos.y - NODE_RADIUS - height;
        break;
      case "bottom":
        x = pos.x - width / 2;
        y = pos.y + NODE_RADIUS;
        break;
      case "left":
        x = pos.x - NODE_RADIUS - width;
        y = pos.y - height / 2;
        break;
      case "right":
        x = pos.x + NODE_RADIUS;
        y = pos.y - height / 2;
        break;
      case "center":
        x = pos.x - width / 2;
        y = pos.y - height / 2;
        break;
    }

    return {
      x: x + offsetX,
      y: y + offsetY,
    };
  }

  /**
   * Get the current visual state.
   */
  getVisualState(): DecoratorVisualState {
    return this.visualState;
  }

  /**
   * Set the visual state and trigger transition.
   */
  setVisualState(state: DecoratorVisualState): void {
    if (state === this.visualState) return;

    const oldState = this.visualState;
    this.visualState = state;

    if (this.element) {
      // Update CSS class
      this.element.classList.remove(`decorator-${oldState}`);
      this.element.classList.add(`decorator-${state}`);
    }

    this.onStateChange(state, oldState);
  }

  /**
   * Get the bounding rect for collision detection.
   */
  getBounds(): DOMRect | null {
    return this.element?.getBoundingClientRect() ?? null;
  }

  /**
   * Clean up resources.
   */
  dispose(): void {
    this.detach();
  }

  // =========================================================================
  // Abstract methods for subclasses
  // =========================================================================

  /**
   * Create the decorator's DOM element.
   * Called once during `attach()`.
   */
  protected abstract createElement(): HTMLElement;

  /**
   * Handle visual state transitions.
   * Called after state and CSS class are updated.
   */
  protected abstract onStateChange(
    newState: DecoratorVisualState,
    oldState: DecoratorVisualState
  ): void;
}
