/**
 * Visualization Controller
 *
 * Orchestrates decorators and layout management.
 * Bridges the gap between D3 simulation and visualization layer.
 */

import type {
  NodeDecorator,
  DecoratorFactory,
  DecoratorConfig,
  NodePosition,
  NodeSnapshot,
  LayoutConstraints,
} from "./types";
import type { NodeState, StreamEvent } from "../types";
import { visualizationBus } from "./event-bus";
import { LayoutManager } from "./layout/manager";
import { chatWindowFactory } from "./decorators/chat-window";

/**
 * Controller options.
 */
interface ControllerOptions {
  /** Container for decorator DOM elements */
  container: HTMLElement;
  /** Layout constraints */
  layoutConstraints?: Partial<LayoutConstraints>;
  /** Whether to auto-attach decorators to running nodes */
  autoAttach?: boolean;
  /** Session ID for API calls */
  sessionId?: string;
}

/**
 * Orchestrates the visualization layer.
 *
 * Responsibilities:
 * - Register decorator factories
 * - Auto-attach decorators to eligible nodes
 * - Forward events to appropriate decorators
 * - Coordinate with LayoutManager
 */
export class VisualizationController {
  private container: HTMLElement;
  private layoutManager: LayoutManager;
  private factories: Map<string, DecoratorFactory> = new Map();
  private nodeDecorators: Map<string, Set<string>> = new Map();
  private autoAttach: boolean;
  private sessionId: string | null = null;

  constructor(options: ControllerOptions) {
    this.container = options.container;
    this.layoutManager = new LayoutManager(options.layoutConstraints);
    this.autoAttach = options.autoAttach ?? true;
    this.sessionId = options.sessionId ?? null;

    // Register default factories
    this.registerFactory(chatWindowFactory);

    // Subscribe to events
    this.setupEventListeners();
  }

  /**
   * Set session ID (can be set after construction if not known at init time).
   */
  setSessionId(sessionId: string): void {
    this.sessionId = sessionId;
  }

  /**
   * Register a decorator factory.
   */
  registerFactory(factory: DecoratorFactory): void {
    this.factories.set(factory.type, factory);
  }

  /**
   * Set up event bus subscriptions.
   */
  private setupEventListeners(): void {
    // Handle position updates
    visualizationBus.on("position:update", (positions) => {
      this.layoutManager.updatePositions(positions);
    });

    // Handle node events - forward to decorators
    visualizationBus.on("node:event", (_data) => {
      // Events are handled by decorators themselves via their own subscriptions
      // This is here for potential future logging/analytics
    });

    // Handle node state changes
    visualizationBus.on("node:state-change", (data) => {
      this.handleNodeStateChange(data.nodeId, data.oldState, data.newState);
    });

    // Handle node creation
    visualizationBus.on("node:created", (node) => {
      console.log("[Viz] node:created event received:", node);
      if (this.autoAttach) {
        this.tryAttachDecorators(node);
      }
    });

    // Handle node removal
    visualizationBus.on("node:removed", (data) => {
      this.detachAllDecorators(data.nodeId);
    });

    // Handle user manually expanding a decorator - mark it so it won't be auto-minimized
    visualizationBus.on("decorator:user-expanded", (data) => {
      this.layoutManager.markUserExpanded(data.decoratorId);
    });
  }

  /**
   * Handle node state changes - may trigger attach/detach.
   */
  private handleNodeStateChange(
    nodeId: string,
    _oldState: NodeState,
    newState: NodeState
  ): void {
    if (!this.autoAttach) return;

    const snapshot: NodeSnapshot = {
      id: nodeId,
      state: newState,
      branch: "", // We don't have branch info in state change, but factories typically only check state
      sessionId: this.sessionId ?? undefined,
    };

    // Check if any factory now wants to attach
    for (const factory of this.factories.values()) {
      const shouldAttach = factory.shouldAttach(snapshot);
      const hasDecorator = this.hasDecoratorOfType(nodeId, factory.type);

      if (shouldAttach && !hasDecorator) {
        // Attach new decorator with config
        const config: DecoratorConfig = {
          sessionId: this.sessionId ?? undefined,
          initialState: newState,
        };
        this.attachDecorator(nodeId, factory, config);
      } else if (!shouldAttach && hasDecorator) {
        // Detach decorator - node no longer eligible
        this.detachDecoratorOfType(nodeId);
      }
    }

    // Check if node completed/failed - minimize the chat window
    if (newState === "completed" || newState === "failed") {
      // Delay to show the completion event briefly
      setTimeout(() => {
        const decorators = this.layoutManager.getDecoratorsForNode(nodeId);
        for (const decorator of decorators) {
          decorator.setVisualState("minimized");
        }
      }, 2000);
    }
  }

  /**
   * Try to attach decorators for a node.
   */
  private tryAttachDecorators(node: NodeSnapshot): void {
    console.log("[Viz] tryAttachDecorators for node:", node.id, "state:", node.state);
    for (const factory of this.factories.values()) {
      const shouldAttach = factory.shouldAttach(node);
      const hasDecorator = this.hasDecoratorOfType(node.id, factory.type);
      console.log("[Viz] Factory", factory.type, "shouldAttach:", shouldAttach, "hasDecorator:", hasDecorator);
      if (shouldAttach && !hasDecorator) {
        console.log("[Viz] Attaching decorator for node:", node.id);
        // Build config with sessionId and initialState
        const config: DecoratorConfig = {
          sessionId: node.sessionId ?? this.sessionId ?? undefined,
          initialState: node.state,
        };
        this.attachDecorator(node.id, factory, config);
      }
    }
  }

  /**
   * Attach a decorator to a node.
   */
  private attachDecorator(nodeId: string, factory: DecoratorFactory, config?: DecoratorConfig): void {
    const decorator = factory.create(nodeId, config);

    // Attach to DOM
    decorator.attach(this.container);

    // Register with layout manager
    this.layoutManager.register(decorator);

    // Track decorator
    if (!this.nodeDecorators.has(nodeId)) {
      this.nodeDecorators.set(nodeId, new Set());
    }
    this.nodeDecorators.get(nodeId)!.add(decorator.id);

    // Emit event
    visualizationBus.emit("decorator:attached", {
      nodeId,
      decorator,
    });
  }

  /**
   * Check if node has a decorator of a specific type.
   */
  private hasDecoratorOfType(nodeId: string, _factoryType: string): boolean {
    const decorators = this.layoutManager.getDecoratorsForNode(nodeId);
    // Factory type is embedded in decorator ID: "decorator-nodeId-..."
    // This is a simplification - in production we'd track types properly
    return decorators.length > 0;
  }

  /**
   * Detach a decorator of a specific type from a node.
   */
  private detachDecoratorOfType(nodeId: string): void {
    const decoratorIds = this.nodeDecorators.get(nodeId);
    if (!decoratorIds) return;

    for (const decoratorId of decoratorIds) {
      const decorators = this.layoutManager.getAllDecorators();
      const decorator = decorators.find((d) => d.id === decoratorId);
      if (decorator) {
        this.detachDecorator(decorator);
      }
    }
  }

  /**
   * Detach all decorators from a node.
   */
  private detachAllDecorators(nodeId: string): void {
    const decoratorIds = this.nodeDecorators.get(nodeId);
    if (!decoratorIds) return;

    const decorators = this.layoutManager.getDecoratorsForNode(nodeId);
    for (const decorator of decorators) {
      this.detachDecorator(decorator);
    }

    this.nodeDecorators.delete(nodeId);
  }

  /**
   * Detach a single decorator.
   */
  private detachDecorator(decorator: NodeDecorator): void {
    // Emit event first
    visualizationBus.emit("decorator:detached", {
      nodeId: decorator.nodeId,
      decoratorId: decorator.id,
    });

    // Remove from layout manager
    this.layoutManager.unregister(decorator.id);

    // Remove from tracking
    const nodeDecorators = this.nodeDecorators.get(decorator.nodeId);
    if (nodeDecorators) {
      nodeDecorators.delete(decorator.id);
    }

    // Detach from DOM
    decorator.detach();
    decorator.dispose();
  }

  /**
   * Manually attach a decorator to a node.
   */
  attach(nodeId: string, factoryType = "chat-window"): NodeDecorator | null {
    const factory = this.factories.get(factoryType);
    if (!factory) {
      console.warn(`Unknown decorator factory: ${factoryType}`);
      return null;
    }

    const decorator = factory.create(nodeId);
    decorator.attach(this.container);
    this.layoutManager.register(decorator);

    if (!this.nodeDecorators.has(nodeId)) {
      this.nodeDecorators.set(nodeId, new Set());
    }
    this.nodeDecorators.get(nodeId)!.add(decorator.id);

    visualizationBus.emit("decorator:attached", { nodeId, decorator });

    return decorator;
  }

  /**
   * Get the layout manager for external position updates.
   */
  getLayoutManager(): LayoutManager {
    return this.layoutManager;
  }

  /**
   * Update positions for multiple nodes.
   * Called from D3 simulation tick.
   */
  updatePositions(positions: NodePosition[]): void {
    visualizationBus.emit("position:update", positions);
  }

  /**
   * Emit a node event.
   * Called when WebSocket receives a stream event.
   */
  emitNodeEvent(nodeId: string, event: StreamEvent, timestamp: string): void {
    console.log("[Viz] emitNodeEvent:", nodeId, event);
    visualizationBus.emit("node:event", { nodeId, event, timestamp });
  }

  /**
   * Emit a node state change.
   */
  emitNodeStateChange(nodeId: string, oldState: NodeState, newState: NodeState): void {
    visualizationBus.emit("node:state-change", { nodeId, oldState, newState });
  }

  /**
   * Emit a node created event.
   */
  emitNodeCreated(node: NodeSnapshot): void {
    visualizationBus.emit("node:created", node);
  }

  /**
   * Attach decorators to existing nodes on initial page load.
   * Call this after fetching graph data.
   */
  attachToExistingNodes(nodes: NodeSnapshot[]): void {
    console.log("[Viz] attachToExistingNodes:", nodes.length, "nodes");
    for (const node of nodes) {
      // Ensure sessionId is set
      if (!node.sessionId && this.sessionId) {
        node.sessionId = this.sessionId;
      }
      this.tryAttachDecorators(node);
    }
  }

  /**
   * Clean up all resources.
   */
  dispose(): void {
    this.layoutManager.dispose();
    this.factories.clear();
    this.nodeDecorators.clear();
  }
}
