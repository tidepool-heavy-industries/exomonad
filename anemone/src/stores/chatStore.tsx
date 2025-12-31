import { createContext, useContext, type ParentComponent } from "solid-js";
import { createStore, produce } from "solid-js/store";
import type { ChatMessage } from "../types";

interface ChatStoreState {
  messages: ChatMessage[];
  isLoading: boolean;
}

interface ChatStoreActions {
  addMessage: (message: Omit<ChatMessage, "id" | "timestamp">) => void;
  setLoading: (loading: boolean) => void;
  clear: () => void;
}

type ChatStore = [ChatStoreState, ChatStoreActions];

const ChatContext = createContext<ChatStore>();

export const ChatProvider: ParentComponent = (props) => {
  const [state, setState] = createStore<ChatStoreState>({
    messages: [],
    isLoading: false,
  });

  const actions: ChatStoreActions = {
    addMessage(msg) {
      const message: ChatMessage = {
        ...msg,
        id: crypto.randomUUID(),
        timestamp: new Date(),
      };
      setState(
        produce((s) => {
          s.messages.push(message);
        })
      );
    },

    setLoading(loading) {
      setState({ isLoading: loading });
    },

    clear() {
      setState({ messages: [], isLoading: false });
    },
  };

  return (
    <ChatContext.Provider value={[state, actions]}>
      {props.children}
    </ChatContext.Provider>
  );
};

export function useChat(): ChatStore {
  const ctx = useContext(ChatContext);
  if (!ctx) {
    throw new Error("useChat must be used within ChatProvider");
  }
  return ctx;
}
