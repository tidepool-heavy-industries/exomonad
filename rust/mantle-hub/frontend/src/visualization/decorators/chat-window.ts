/**
 * Chat Window Decorator
 *
 * Live scrolling chat window that hovers over running nodes.
 * Shows streaming events, minimizes to icon when crowded.
 */

import { BaseDecorator } from "./base";
import type {
  DecoratorVisualState,
  DecoratorFactory,
  DecoratorConfig,
  NodeSnapshot,
} from "../types";
import type { StreamEvent, NodeState, NodeEvent } from "../../types";
import { visualizationBus } from "../event-bus";

/** Message parsed from StreamEvent for display */
interface ChatMessage {
  id: string;
  timestamp: string;
  type: "assistant" | "tool_use" | "tool_result" | "user" | "system";
  content: string;
  toolName?: string;
  isStreaming?: boolean;
}

/** Configuration for ChatWindowDecorator */
interface ChatWindowConfig extends DecoratorConfig {
  maxMessages?: number;
  autoScrollThreshold?: number;
  sessionId?: string;
  initialState?: NodeState;
}

/**
 * Chat window that displays live events for a node.
 */
export class ChatWindowDecorator extends BaseDecorator {
  private messages: ChatMessage[] = [];
  private unreadCount = 0;
  private maxMessages: number;
  private autoScrollThreshold: number;
  private userHasScrolled = false;
  private messagesContainer: HTMLElement | null = null;
  private unsubscribe: (() => void) | null = null;
  private headerElement: HTMLElement | null = null;
  private unreadBadge: HTMLElement | null = null;
  private isDragging = false;
  private dragOffset = { x: 0, y: 0 };
  private sessionId: string | null = null;
  private nodeState: NodeState = "pending";

  constructor(nodeId: string, config: ChatWindowConfig = {}) {
    super(nodeId, config.priority ?? 10, config.anchor);
    this.maxMessages = config.maxMessages ?? 100;
    this.autoScrollThreshold = config.autoScrollThreshold ?? 50;
    this.sessionId = config.sessionId ?? null;
    this.nodeState = config.initialState ?? "pending";
  }

  /**
   * Create the chat window DOM structure.
   */
  protected createElement(): HTMLElement {
    const wrapper = document.createElement("div");
    wrapper.className = "chat-window";
    wrapper.dataset.nodeState = this.nodeState;

    // Header with controls
    this.headerElement = document.createElement("div");
    this.headerElement.className = "chat-header";
    this.headerElement.innerHTML = `
      <span class="chat-title">${this.nodeId.slice(0, 8)}...</span>
      <div class="chat-controls">
        <button class="chat-btn minimize-btn" title="Minimize">−</button>
        <button class="chat-btn close-btn" title="Close">×</button>
      </div>
    `;

    // Wire up controls
    const minimizeBtn = this.headerElement.querySelector(".minimize-btn");
    const closeBtn = this.headerElement.querySelector(".close-btn");

    minimizeBtn?.addEventListener("click", (e) => {
      e.stopPropagation();
      this.setVisualState("minimized");
    });

    closeBtn?.addEventListener("click", (e) => {
      e.stopPropagation();
      this.setVisualState("hidden");
    });

    // Drag functionality
    this.headerElement.addEventListener("mousedown", (e) => {
      if ((e.target as HTMLElement).closest(".chat-btn")) return;
      this.startDrag(e);
    });

    // Click on minimized state to expand
    wrapper.addEventListener("click", () => {
      if (this.visualState === "minimized") {
        this.setVisualState("expanded");
        // Emit user-expanded event so layout manager won't auto-minimize
        visualizationBus.emit("decorator:user-expanded", { decoratorId: this.id });
      }
    });

    // Messages container
    this.messagesContainer = document.createElement("div");
    this.messagesContainer.className = "chat-messages";

    // Track user scroll
    this.messagesContainer.addEventListener("scroll", () => {
      this.checkUserScroll();
    });

    // Unread badge (for minimized state)
    this.unreadBadge = document.createElement("div");
    this.unreadBadge.className = "unread-badge hidden";
    this.unreadBadge.textContent = "0";

    // Minimized icon
    const minimizedIcon = document.createElement("div");
    minimizedIcon.className = "minimized-icon";
    minimizedIcon.innerHTML = "💬";

    wrapper.appendChild(this.headerElement);
    wrapper.appendChild(this.messagesContainer);
    wrapper.appendChild(minimizedIcon);
    wrapper.appendChild(this.unreadBadge);

    // Subscribe to events for this node
    this.subscribeToEvents();

    // Load historical events if we have a sessionId
    if (this.sessionId) {
      this.loadHistoricalEvents();
    }

    // Set initial visual state based on node state
    // Running nodes start expanded, others start minimized
    if (this.nodeState !== "running") {
      // Defer to allow DOM attachment first
      requestAnimationFrame(() => {
        this.setVisualState("minimized");
      });
    }

    return wrapper;
  }

  /**
   * Subscribe to node events via the event bus.
   */
  private subscribeToEvents(): void {
    console.log("[ChatWindow] Subscribing to events for node:", this.nodeId);
    this.unsubscribe = visualizationBus.on("node:event", (data) => {
      console.log("[ChatWindow] Received node:event for:", data.nodeId, "my node:", this.nodeId);
      if (data.nodeId === this.nodeId) {
        this.handleEvent(data.event, data.timestamp);
      }
    });

    // Also subscribe to state changes
    visualizationBus.on("node:state-change", (data) => {
      if (data.nodeId === this.nodeId) {
        this.updateNodeState(data.newState);
      }
    });
  }

  /**
   * Load historical events from the API.
   */
  private async loadHistoricalEvents(): Promise<void> {
    if (!this.sessionId) return;

    try {
      const resp = await fetch(
        `/api/sessions/${this.sessionId}/nodes/${this.nodeId}/events`
      );
      if (!resp.ok) {
        console.warn(`[ChatWindow] Failed to load events: ${resp.status}`);
        return;
      }

      const events: NodeEvent[] = await resp.json();
      console.log(`[ChatWindow] Loaded ${events.length} historical events for ${this.nodeId}`);

      // Parse and render each event
      for (const nodeEvent of events) {
        const message = this.parseEvent(nodeEvent.event, nodeEvent.timestamp);
        if (message) {
          this.messages.push(message);
          this.renderMessage(message);
        }
      }

      // Trim if over limit
      while (this.messages.length > this.maxMessages) {
        this.messages.shift();
        this.messagesContainer?.firstChild?.remove();
      }

      // Update unread badge for minimized windows
      if (this.visualState === "minimized" && this.messages.length > 0) {
        this.unreadCount = this.messages.length;
        this.updateUnreadBadge();
      }

      // Scroll to bottom after loading
      this.scrollToBottom();
    } catch (e) {
      console.error("[ChatWindow] Error loading historical events:", e);
    }
  }

  /**
   * Handle incoming stream event.
   */
  handleEvent(event: StreamEvent, timestamp: string): void {
    console.log("[ChatWindow] handleEvent:", this.nodeId, event);
    const message = this.parseEvent(event, timestamp);
    if (!message) return;

    this.messages.push(message);

    // Trim old messages
    while (this.messages.length > this.maxMessages) {
      this.messages.shift();
    }

    // Render the new message
    this.renderMessage(message);

    // Update unread count if minimized
    if (this.visualState === "minimized") {
      this.unreadCount++;
      this.updateUnreadBadge();
    }

    // Auto-scroll if user hasn't manually scrolled
    if (!this.userHasScrolled) {
      this.scrollToBottom();
    }

    // Trigger attention state if important
    if (this.visualState === "minimized" && this.isImportantMessage(message)) {
      this.pulseAttention();
    }
  }

  /**
   * Parse a StreamEvent into a ChatMessage.
   */
  private parseEvent(event: StreamEvent, timestamp: string): ChatMessage | null {
    const id = `msg-${Date.now()}-${Math.random().toString(36).slice(2, 6)}`;

    // Handle different event types based on the serde tag format
    if ("type" in event) {
      const eventType = (event as { type: string }).type;

      switch (eventType) {
        case "assistant": {
          const assistantEvent = event as {
            type: "assistant";
            message: {
              content: Array<{
                type: string;
                text?: string;
                name?: string;
                input?: unknown;
              }>;
            };
          };

          // Extract text content
          const textBlocks = assistantEvent.message.content
            .filter((b) => b.type === "text" && b.text)
            .map((b) => b.text as string);

          if (textBlocks.length > 0) {
            return {
              id,
              timestamp,
              type: "assistant",
              content: textBlocks.join("\n"),
            };
          }

          // Check for tool use
          const toolUse = assistantEvent.message.content.find(
            (b) => b.type === "tool_use"
          );
          if (toolUse && toolUse.name) {
            return {
              id,
              timestamp,
              type: "tool_use",
              content: JSON.stringify(toolUse.input, null, 2),
              toolName: toolUse.name,
            };
          }
          break;
        }

        case "result": {
          const resultEvent = event as {
            type: "result";
            result: string;
            duration_ms: number;
            num_turns: number;
          };
          return {
            id,
            timestamp,
            type: "system",
            content: `Completed in ${resultEvent.num_turns} turns (${(resultEvent.duration_ms / 1000).toFixed(1)}s)`,
          };
        }

        case "error": {
          const errorEvent = event as { type: "error"; error: string };
          return {
            id,
            timestamp,
            type: "system",
            content: `Error: ${errorEvent.error}`,
          };
        }

        case "system": {
          const systemEvent = event as { type: "system"; message: string };
          return {
            id,
            timestamp,
            type: "system",
            content: systemEvent.message,
          };
        }
      }
    }

    return null;
  }

  /**
   * Render a single message to the DOM.
   */
  private renderMessage(message: ChatMessage): void {
    if (!this.messagesContainer) return;

    const msgEl = document.createElement("div");
    msgEl.className = `chat-message message-${message.type}`;
    msgEl.dataset.messageId = message.id;

    const time = new Date(message.timestamp).toLocaleTimeString("en-US", {
      hour: "2-digit",
      minute: "2-digit",
      second: "2-digit",
      hour12: false,
    });

    if (message.type === "tool_use" && message.toolName) {
      msgEl.innerHTML = `
        <span class="msg-time">${time}</span>
        <span class="msg-tool">[${message.toolName}]</span>
        <pre class="msg-content">${this.escapeHtml(message.content)}</pre>
      `;
    } else {
      msgEl.innerHTML = `
        <span class="msg-time">${time}</span>
        <span class="msg-content">${this.escapeHtml(message.content)}</span>
      `;
    }

    this.messagesContainer.appendChild(msgEl);
  }

  /**
   * Escape HTML for safe rendering.
   */
  private escapeHtml(text: string): string {
    const div = document.createElement("div");
    div.textContent = text;
    return div.innerHTML;
  }

  /**
   * Check if user has scrolled away from bottom.
   */
  private checkUserScroll(): void {
    if (!this.messagesContainer) return;

    const { scrollTop, scrollHeight, clientHeight } = this.messagesContainer;
    const distanceFromBottom = scrollHeight - scrollTop - clientHeight;

    this.userHasScrolled = distanceFromBottom > this.autoScrollThreshold;
  }

  /**
   * Scroll messages container to bottom.
   */
  private scrollToBottom(): void {
    if (!this.messagesContainer) return;
    this.messagesContainer.scrollTop = this.messagesContainer.scrollHeight;
  }

  /**
   * Update the unread badge display.
   */
  private updateUnreadBadge(): void {
    if (!this.unreadBadge) return;

    if (this.unreadCount > 0) {
      this.unreadBadge.textContent = this.unreadCount > 99 ? "99+" : String(this.unreadCount);
      this.unreadBadge.classList.remove("hidden");
    } else {
      this.unreadBadge.classList.add("hidden");
    }
  }

  /**
   * Brief pulse animation for attention.
   */
  private pulseAttention(): void {
    if (!this.element) return;
    this.element.classList.add("attention-pulse");
    setTimeout(() => {
      this.element?.classList.remove("attention-pulse");
    }, 1000);
  }

  /**
   * Check if a message is important enough to trigger attention.
   */
  private isImportantMessage(message: ChatMessage): boolean {
    return message.type === "system" || message.content.toLowerCase().includes("error");
  }

  /**
   * Update node state (for header coloring and wrapper styling).
   */
  private updateNodeState(state: NodeState): void {
    this.nodeState = state;
    if (this.headerElement) {
      this.headerElement.dataset.state = state;
    }
    if (this.element) {
      this.element.dataset.nodeState = state;
    }
  }

  /**
   * Handle visual state transitions.
   */
  protected onStateChange(
    newState: DecoratorVisualState,
    _oldState: DecoratorVisualState
  ): void {
    // Clear unread when expanding
    if (newState === "expanded") {
      this.unreadCount = 0;
      this.updateUnreadBadge();
      this.userHasScrolled = false;
      // Scroll to bottom after transition
      requestAnimationFrame(() => this.scrollToBottom());
    }

    // Emit state change event
    visualizationBus.emit("decorator:state-change", {
      decoratorId: this.id,
      oldState: _oldState,
      newState,
    });
  }

  /**
   * Start dragging the chat window.
   */
  private startDrag(e: MouseEvent): void {
    if (!this.element) return;

    this.isDragging = true;
    this.element.classList.add("dragging");

    const rect = this.element.getBoundingClientRect();
    this.dragOffset = {
      x: e.clientX - rect.left,
      y: e.clientY - rect.top,
    };

    document.addEventListener("mousemove", this.onDrag);
    document.addEventListener("mouseup", this.stopDrag);
    e.preventDefault();
  }

  /**
   * Handle drag movement with viewport clamping.
   */
  private onDrag = (e: MouseEvent): void => {
    if (!this.isDragging || !this.element) return;

    const width = this.element.offsetWidth;
    const padding = 20; // Keep at least this much visible

    // Calculate new position
    let x = e.clientX - this.dragOffset.x;
    let y = e.clientY - this.dragOffset.y;

    // Clamp to viewport bounds
    const maxX = window.innerWidth - padding;
    const maxY = window.innerHeight - padding;
    const minX = padding - width;
    const minY = 0;

    x = Math.max(minX, Math.min(x, maxX));
    y = Math.max(minY, Math.min(y, maxY));

    this.element.style.transform = `translate(${x}px, ${y}px)`;
  };

  /**
   * Stop dragging.
   */
  private stopDrag = (): void => {
    this.isDragging = false;
    document.removeEventListener("mousemove", this.onDrag);
    document.removeEventListener("mouseup", this.stopDrag);

    if (this.element) {
      this.element.classList.remove("dragging");
    }
  };

  /**
   * Clean up resources.
   */
  dispose(): void {
    // Clean up drag listeners
    document.removeEventListener("mousemove", this.onDrag);
    document.removeEventListener("mouseup", this.stopDrag);

    if (this.unsubscribe) {
      this.unsubscribe();
      this.unsubscribe = null;
    }
    this.messages = [];
    this.messagesContainer = null;
    this.headerElement = null;
    this.unreadBadge = null;
    super.dispose();
  }
}

/**
 * Factory for creating ChatWindowDecorators.
 * Attaches to running, completed, and failed nodes.
 */
export const chatWindowFactory: DecoratorFactory<ChatWindowDecorator> = {
  type: "chat-window",

  create(nodeId: string, config?: DecoratorConfig): ChatWindowDecorator {
    return new ChatWindowDecorator(nodeId, config as ChatWindowConfig);
  },

  shouldAttach(node: NodeSnapshot): boolean {
    // Attach to running, completed, and failed nodes (all that might have events)
    return (
      node.state === "running" ||
      node.state === "completed" ||
      node.state === "failed"
    );
  },
};
