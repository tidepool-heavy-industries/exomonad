// WebSocket service for mantle-hub frontend

import type {
  SessionInfo,
  NodeInfo,
  NodeResult,
  StreamEvent,
  WebSocketMessage,
} from "./types";

export interface HubWebSocketCallbacks {
  onConnect?: () => void;
  onDisconnect?: () => void;
  onInit?: (sessions: SessionInfo[]) => void;
  onSessionCreated?: (session: SessionInfo) => void;
  onSessionUpdated?: (session: SessionInfo) => void;
  onNodeCreated?: (node: NodeInfo) => void;
  onNodeUpdated?: (node: NodeInfo) => void;
  onNodeCompleted?: (nodeId: string, result: NodeResult) => void;
  onNodeFailed?: (nodeId: string, error: string) => void;
  onNodeEvent?: (sessionId: string, nodeId: string, event: StreamEvent, timestamp: string) => void;
}

export class HubWebSocket {
  private ws: WebSocket | null = null;
  private url: string;
  private callbacks: HubWebSocketCallbacks;
  private reconnectTimeout: number | null = null;
  private reconnectDelay = 1000;
  private maxReconnectDelay = 30000;

  constructor(callbacks: HubWebSocketCallbacks = {}) {
    const protocol = location.protocol === "https:" ? "wss:" : "ws:";
    this.url = `${protocol}//${location.host}/ws`;
    this.callbacks = callbacks;
  }

  connect(): void {
    if (this.ws) {
      return;
    }

    this.ws = new WebSocket(this.url);

    this.ws.onopen = () => {
      this.reconnectDelay = 1000;
      this.callbacks.onConnect?.();
    };

    this.ws.onclose = () => {
      this.ws = null;
      this.callbacks.onDisconnect?.();
      this.scheduleReconnect();
    };

    this.ws.onerror = (err) => {
      console.error("WebSocket error:", err);
    };

    this.ws.onmessage = (event) => {
      try {
        const msg: WebSocketMessage = JSON.parse(event.data);
        this.handleMessage(msg);
      } catch (e) {
        console.error("Failed to parse WebSocket message:", e);
      }
    };
  }

  disconnect(): void {
    if (this.reconnectTimeout) {
      clearTimeout(this.reconnectTimeout);
      this.reconnectTimeout = null;
    }
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
  }

  private scheduleReconnect(): void {
    if (this.reconnectTimeout) {
      return;
    }

    this.reconnectTimeout = window.setTimeout(() => {
      this.reconnectTimeout = null;
      this.connect();
    }, this.reconnectDelay);

    // Exponential backoff
    this.reconnectDelay = Math.min(this.reconnectDelay * 2, this.maxReconnectDelay);
  }

  private handleMessage(msg: WebSocketMessage): void {
    // Init message
    if ("type" in msg && msg.type === "init") {
      this.callbacks.onInit?.(msg.sessions);
      return;
    }

    // HubEvent variants - uses serde tag="type" with snake_case
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const event = msg as any;
    console.log("[WS] Event received:", event.type, event);

    switch (event.type) {
      case "session_created":
        this.callbacks.onSessionCreated?.(event.session as SessionInfo);
        break;
      case "session_updated":
        this.callbacks.onSessionUpdated?.(event.session as SessionInfo);
        break;
      case "node_created":
        console.log("[WS] NodeCreated:", event.node);
        this.callbacks.onNodeCreated?.(event.node as NodeInfo);
        break;
      case "node_updated":
        this.callbacks.onNodeUpdated?.(event.node as NodeInfo);
        break;
      case "node_completed":
        this.callbacks.onNodeCompleted?.(event.node_id as string, event.result as NodeResult);
        break;
      case "node_failed":
        this.callbacks.onNodeFailed?.(event.node_id as string, event.error as string);
        break;
      case "node_event":
        console.log("[WS] NodeEvent:", event.node_id, event.event);
        this.callbacks.onNodeEvent?.(
          event.session_id as string,
          event.node_id as string,
          event.event as StreamEvent,
          event.timestamp as string
        );
        break;
    }
  }
}
