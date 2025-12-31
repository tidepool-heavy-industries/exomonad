import type {
  ClientMessage,
  ServerMessage,
  SerializableEffect,
  EffectResult,
} from "../types";

export interface WebSocketCallbacks {
  onOpen?: () => void;
  onClose?: (code: number, reason: string) => void;
  onError?: (error: Event) => void;
  onProgress?: (node: string, effectType: string) => void;
  onSuspend?: (effect: SerializableEffect) => void;
  onDone?: (result: unknown) => void;
  onServerError?: (message: string) => void;
}

export class GraphWebSocket {
  private ws: WebSocket | null = null;
  private callbacks: WebSocketCallbacks;
  private reconnectAttempts = 0;
  private maxReconnectAttempts = 5;
  private reconnectDelay = 1000;
  private url: string | null = null;

  constructor(callbacks: WebSocketCallbacks) {
    this.callbacks = callbacks;
  }

  async connect(wsUrl: string): Promise<void> {
    this.url = wsUrl;

    return new Promise((resolve, reject) => {
      try {
        this.ws = new WebSocket(wsUrl);

        this.ws.onopen = () => {
          this.reconnectAttempts = 0;
          this.callbacks.onOpen?.();
          resolve();
        };

        this.ws.onclose = (event) => {
          this.callbacks.onClose?.(event.code, event.reason);
          this.attemptReconnect();
        };

        this.ws.onerror = (error) => {
          this.callbacks.onError?.(error);
          reject(error);
        };

        this.ws.onmessage = (event) => {
          this.handleMessage(event.data);
        };
      } catch (err) {
        reject(err);
      }
    });
  }

  private handleMessage(data: string): void {
    try {
      const msg: ServerMessage = JSON.parse(data);

      switch (msg.type) {
        case "progress":
          this.callbacks.onProgress?.(msg.node, msg.effect);
          break;
        case "suspend":
          this.callbacks.onSuspend?.(msg.effect);
          break;
        case "done":
          this.callbacks.onDone?.(msg.result);
          break;
        case "error":
          this.callbacks.onServerError?.(msg.message);
          break;
      }
    } catch (err) {
      console.error("Failed to parse WebSocket message:", err, data);
    }
  }

  /** Initialize graph execution with input */
  init(input: unknown): void {
    this.send({ type: "init", input });
  }

  /** Resume after effect execution */
  resume(result: EffectResult): void {
    this.send({ type: "resume", result });
  }

  private send(msg: ClientMessage): void {
    if (this.ws?.readyState === WebSocket.OPEN) {
      this.ws.send(JSON.stringify(msg));
    } else {
      console.error("WebSocket not connected, cannot send:", msg);
    }
  }

  private attemptReconnect(): void {
    if (!this.url) return;

    if (this.reconnectAttempts < this.maxReconnectAttempts) {
      this.reconnectAttempts++;
      const delay = this.reconnectDelay * Math.pow(2, this.reconnectAttempts - 1);

      console.log(
        `Reconnection attempt ${this.reconnectAttempts}/${this.maxReconnectAttempts} in ${delay}ms...`
      );

      setTimeout(() => {
        if (this.url) {
          this.connect(this.url).catch((err) => {
            console.error("Reconnection failed:", err);
          });
        }
      }, delay);
    } else {
      console.error("Max reconnection attempts reached");
    }
  }

  disconnect(): void {
    this.url = null; // Prevent reconnection
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
  }

  get isConnected(): boolean {
    return this.ws?.readyState === WebSocket.OPEN;
  }

  get readyState(): number {
    return this.ws?.readyState ?? WebSocket.CLOSED;
  }
}

/** Create a new session and get the WebSocket URL */
export async function createSession(
  baseUrl: string
): Promise<{ sessionId: string; wsUrl: string }> {
  const response = await fetch(`${baseUrl}/new`, { method: "POST" });

  if (!response.ok) {
    throw new Error(`Failed to create session: ${response.statusText}`);
  }

  const data = await response.json();
  return {
    sessionId: data.sessionId,
    wsUrl: data.wsUrl,
  };
}
