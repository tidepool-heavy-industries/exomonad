import {
  createContext,
  useContext,
  type ParentComponent,
  type Accessor,
} from "solid-js";
import { createStore } from "solid-js/store";
import type { ConnectionStatus } from "../types";

interface ConnectionState {
  status: ConnectionStatus;
  wsUrl: string | null;
  sessionId: string | null;
  error: string | null;
  lastPing: Date | null;
}

interface ConnectionActions {
  connect: (wsUrl: string, sessionId: string) => void;
  disconnect: () => void;
  setStatus: (status: ConnectionStatus) => void;
  setError: (error: string | null) => void;
  updatePing: () => void;
}

type ConnectionStore = [ConnectionState, ConnectionActions];

const ConnectionContext = createContext<ConnectionStore>();

export const ConnectionProvider: ParentComponent = (props) => {
  const [state, setState] = createStore<ConnectionState>({
    status: "disconnected",
    wsUrl: null,
    sessionId: null,
    error: null,
    lastPing: null,
  });

  const actions: ConnectionActions = {
    connect(wsUrl, sessionId) {
      setState({
        wsUrl,
        sessionId,
        status: "connecting",
        error: null,
      });
    },

    disconnect() {
      setState({
        status: "disconnected",
        wsUrl: null,
        error: null,
      });
    },

    setStatus(status) {
      setState({ status });
    },

    setError(error) {
      setState({
        error,
        status: error ? "error" : state.status,
      });
    },

    updatePing() {
      setState({ lastPing: new Date() });
    },
  };

  return (
    <ConnectionContext.Provider value={[state, actions]}>
      {props.children}
    </ConnectionContext.Provider>
  );
};

export function useConnection(): ConnectionStore {
  const ctx = useContext(ConnectionContext);
  if (!ctx) {
    throw new Error("useConnection must be used within ConnectionProvider");
  }
  return ctx;
}

/** Accessor for just the status */
export function useConnectionStatus(): Accessor<ConnectionStatus> {
  const [state] = useConnection();
  return () => state.status;
}
