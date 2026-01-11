/**
 * Visualization Layer
 *
 * Extensible system for node visualizations: chat windows, sprites, etc.
 */

// Types
export type {
  NodePosition,
  DecoratorAnchor,
  AnchorOffset,
  DecoratorVisualState,
  NodeDecorator,
  DecoratorFactory,
  DecoratorConfig,
  NodeSnapshot,
  VisualizationEvents,
  EventHandler,
  LayoutConstraints,
  VisualizationTheme,
} from "./types";

// Event Bus
export { EventBus, visualizationBus } from "./event-bus";

// Decorators (added as implemented)
export { BaseDecorator } from "./decorators/base";
export { ChatWindowDecorator, chatWindowFactory } from "./decorators/chat-window";

// Layout (added as implemented)
export { LayoutManager } from "./layout/manager";

// Controller (added as implemented)
export { VisualizationController } from "./controller";
