import {
  createContext,
  useContext,
  type ParentComponent,
  type Accessor,
} from "solid-js";
import { createStore, produce } from "solid-js/store";
import type {
  ExecutionPhase,
  GraphInfo,
  SerializableEffect,
  GraphState,
} from "../types";

interface GraphStoreState {
  /** Static graph metadata (from getGraphInfo) */
  graphInfo: GraphInfo | null;
  /** Current execution phase */
  phase: ExecutionPhase;
  /** Nodes that have completed */
  completedNodes: string[];
  /** Current effect being executed */
  activeEffect: SerializableEffect | null;
  /** When the current effect started */
  effectStartTime: Date | null;
}

interface GraphStoreActions {
  setGraphInfo: (info: GraphInfo) => void;
  updatePhase: (phase: ExecutionPhase) => void;
  markNodeCompleted: (nodeName: string) => void;
  setActiveEffect: (effect: SerializableEffect | null) => void;
  updateFromState: (state: GraphState) => void;
  reset: () => void;
}

type GraphStore = [GraphStoreState, GraphStoreActions];

const initialState: GraphStoreState = {
  graphInfo: null,
  phase: { type: "idle" },
  completedNodes: [],
  activeEffect: null,
  effectStartTime: null,
};

const GraphContext = createContext<GraphStore>();

export const GraphProvider: ParentComponent = (props) => {
  const [state, setState] = createStore<GraphStoreState>({ ...initialState });

  const actions: GraphStoreActions = {
    setGraphInfo(info) {
      setState({ graphInfo: info });
    },

    updatePhase(phase) {
      setState({ phase });
    },

    markNodeCompleted(nodeName) {
      setState(
        produce((s) => {
          if (!s.completedNodes.includes(nodeName)) {
            s.completedNodes.push(nodeName);
          }
        })
      );
    },

    setActiveEffect(effect) {
      setState({
        activeEffect: effect,
        effectStartTime: effect ? new Date() : null,
      });
    },

    updateFromState(graphState) {
      setState({
        phase: graphState.phase,
        completedNodes: [...graphState.completedNodes],
      });
    },

    reset() {
      setState({
        phase: { type: "idle" },
        completedNodes: [],
        activeEffect: null,
        effectStartTime: null,
      });
    },
  };

  return (
    <GraphContext.Provider value={[state, actions]}>
      {props.children}
    </GraphContext.Provider>
  );
};

export function useGraph(): GraphStore {
  const ctx = useContext(GraphContext);
  if (!ctx) {
    throw new Error("useGraph must be used within GraphProvider");
  }
  return ctx;
}

/** Accessor for current phase */
export function usePhase(): Accessor<ExecutionPhase> {
  const [state] = useGraph();
  return () => state.phase;
}

/** Accessor for active effect */
export function useActiveEffect(): Accessor<SerializableEffect | null> {
  const [state] = useGraph();
  return () => state.activeEffect;
}
