import { createSignal } from "solid-js";
import type { UIState, UserAction } from "./types";

export type ConnectionStatus = "connecting" | "connected" | "disconnected" | "error";

export interface SocketState {
  status: ConnectionStatus;
  uiState: UIState | null;
  error: string | null;
}

const DEFAULT_UI_STATE: UIState = {
  messages: [],
  textInput: { placeholder: "Type a message..." },
  photoUpload: null,
  choices: null,
  graphNode: "init",
  thinking: false,
};

export function createSocket(url: string) {
  const [state, setState] = createSignal<SocketState>({
    status: "disconnected",
    uiState: DEFAULT_UI_STATE,
    error: null,
  });

  let ws: WebSocket | null = null;
  let reconnectAttempts = 0;
  let reconnectTimer: ReturnType<typeof setTimeout> | null = null;
  const maxReconnectAttempts = 5;
  const baseDelay = 1000;

  function connect() {
    if (ws?.readyState === WebSocket.OPEN || ws?.readyState === WebSocket.CONNECTING) {
      return;
    }

    setState((s) => ({ ...s, status: "connecting", error: null }));

    try {
      ws = new WebSocket(url);

      ws.onopen = () => {
        reconnectAttempts = 0;
        setState((s) => ({ ...s, status: "connected", error: null }));
      };

      ws.onmessage = (event) => {
        try {
          const uiState = JSON.parse(event.data) as UIState;
          setState((s) => ({ ...s, uiState }));
        } catch (e) {
          console.error("Failed to parse UIState:", e);
        }
      };

      ws.onclose = () => {
        ws = null;
        setState((s) => ({ ...s, status: "disconnected" }));
        scheduleReconnect();
      };

      ws.onerror = () => {
        setState((s) => ({ ...s, status: "error", error: "Connection error" }));
      };
    } catch (e) {
      setState((s) => ({
        ...s,
        status: "error",
        error: e instanceof Error ? e.message : "Failed to connect",
      }));
      scheduleReconnect();
    }
  }

  function scheduleReconnect() {
    if (reconnectAttempts >= maxReconnectAttempts) {
      setState((s) => ({ ...s, error: "Max reconnection attempts reached" }));
      return;
    }

    const delay = baseDelay * Math.pow(2, reconnectAttempts);
    reconnectAttempts++;

    reconnectTimer = setTimeout(() => {
      connect();
    }, delay);
  }

  function send(action: UserAction) {
    if (ws?.readyState === WebSocket.OPEN) {
      ws.send(JSON.stringify(action));
    }
  }

  function disconnect() {
    if (reconnectTimer) {
      clearTimeout(reconnectTimer);
      reconnectTimer = null;
    }
    reconnectAttempts = maxReconnectAttempts; // Prevent auto-reconnect
    ws?.close();
    ws = null;
    setState((s) => ({ ...s, status: "disconnected" }));
  }

  function reconnect() {
    reconnectAttempts = 0;
    disconnect();
    connect();
  }

  return {
    state,
    connect,
    disconnect,
    reconnect,
    send,
  };
}
