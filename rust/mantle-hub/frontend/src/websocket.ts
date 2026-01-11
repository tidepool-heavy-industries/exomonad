// WebSocket service for mantle-hub frontend

import type {
  SessionInfo,
  NodeInfo,
  NodeResult,
  StreamEvent,
  WebSocketMessage,
  HubEvent,
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

    // HubEvent variants
    const event = msg as HubEvent;

    if ("SessionCreated" in event) {
      this.callbacks.onSessionCreated?.(event.SessionCreated.session);
    } else if ("SessionUpdated" in event) {
      this.callbacks.onSessionUpdated?.(event.SessionUpdated.session);
    } else if ("NodeCreated" in event) {
      this.callbacks.onNodeCreated?.(event.NodeCreated.node);
    } else if ("NodeUpdated" in event) {
      this.callbacks.onNodeUpdated?.(event.NodeUpdated.node);
    } else if ("NodeCompleted" in event) {
      this.callbacks.onNodeCompleted?.(event.NodeCompleted.node_id, event.NodeCompleted.result);
    } else if ("NodeFailed" in event) {
      this.callbacks.onNodeFailed?.(event.NodeFailed.node_id, event.NodeFailed.error);
    } else if ("NodeEvent" in event) {
      this.callbacks.onNodeEvent?.(
        event.NodeEvent.session_id,
        event.NodeEvent.node_id,
        event.NodeEvent.event,
        event.NodeEvent.timestamp
      );
    }
  }
}
