import { type Component, createSignal, Show } from "solid-js";
import { useConnection, useChat } from "../../stores";
import { useWebSocket } from "../../hooks";
import { Spinner } from "../shared";
import { API_URL } from "../../config";

export const ChatInput: Component = () => {
  const [input, setInput] = createSignal("");
  const [connection] = useConnection();
  const [chat] = useChat();
  const { sendMessage, connect } = useWebSocket();

  const canSend = () =>
    connection.status === "connected" &&
    !chat.isLoading &&
    input().trim().length > 0;

  const handleSubmit = (e: Event) => {
    e.preventDefault();
    const text = input().trim();
    if (text && canSend()) {
      sendMessage(text);
      setInput("");
    }
  };

  const handleConnect = async () => {
    try {
      await connect(API_URL);
    } catch (err) {
      console.error("Failed to connect:", err);
    }
  };

  const handleKeyDown = (e: KeyboardEvent) => {
    if (e.key === "Enter" && !e.shiftKey) {
      e.preventDefault();
      handleSubmit(e);
    }
  };

  return (
    <div class="p-4 border-t border-bg-hover bg-bg-secondary">
      {/* Connect button when disconnected */}
      <Show when={connection.status === "disconnected"}>
        <button
          type="button"
          onClick={handleConnect}
          class="w-full py-3 px-4 bg-phase-active text-white rounded-lg font-medium hover:bg-phase-active/90 transition-colors"
        >
          Connect to Session
        </button>
      </Show>

      {/* Connecting state */}
      <Show when={connection.status === "connecting"}>
        <div class="flex items-center justify-center gap-2 py-3 text-text-muted">
          <Spinner size="sm" />
          <span>Connecting...</span>
        </div>
      </Show>

      {/* Error state */}
      <Show when={connection.status === "error"}>
        <div class="space-y-2">
          <div class="text-sm text-phase-failed text-center">
            {connection.error || "Connection failed"}
          </div>
          <button
            type="button"
            onClick={handleConnect}
            class="w-full py-2 px-4 bg-phase-failed/20 text-phase-failed rounded-lg font-medium hover:bg-phase-failed/30 transition-colors"
          >
            Retry Connection
          </button>
        </div>
      </Show>

      {/* Input form when connected */}
      <Show when={connection.status === "connected"}>
        <form onSubmit={handleSubmit} class="flex gap-2">
          <input
            type="text"
            value={input()}
            onInput={(e) => setInput(e.currentTarget.value)}
            onKeyDown={handleKeyDown}
            placeholder={chat.isLoading ? "Waiting for response..." : "Type a message..."}
            disabled={chat.isLoading}
            class="flex-1 bg-bg-primary border border-bg-hover rounded-lg px-4 py-2 text-text-primary placeholder-text-dim focus:outline-none focus:border-phase-active disabled:opacity-50 transition-colors"
          />
          <button
            type="submit"
            disabled={!canSend()}
            class="px-4 py-2 bg-phase-active text-white rounded-lg font-medium hover:bg-phase-active/90 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
          >
            <Show when={chat.isLoading} fallback="Send">
              <Spinner size="sm" class="text-white" />
            </Show>
          </button>
        </form>
      </Show>
    </div>
  );
};
