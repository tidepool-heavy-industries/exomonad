import { createEffect, For, onMount, Show, type Component } from "solid-js";
import { createSocket, type ConnectionStatus } from "./lib/socket";
import type { UserAction } from "./lib/types";
import ChatMessage from "./components/ChatMessage";
import InputArea from "./components/InputArea";
import ButtonGroup from "./components/ButtonGroup";

const App: Component = () => {
  // Default to port 8080, allow override via env
  const wsUrl = import.meta.env.VITE_WS_URL || "ws://localhost:8080";
  const socket = createSocket(wsUrl);

  let chatContainerRef: HTMLDivElement | undefined;

  onMount(() => {
    socket.connect();
  });

  // Auto-scroll to bottom when messages change
  createEffect(() => {
    const state = socket.state();
    if (state.uiState?.messages.length && chatContainerRef) {
      chatContainerRef.scrollTop = chatContainerRef.scrollHeight;
    }
  });

  const handleSend = (action: UserAction) => {
    socket.send(action);
  };

  const statusColor = (status: ConnectionStatus) => {
    switch (status) {
      case "connected":
        return "bg-green-500";
      case "connecting":
        return "bg-yellow-500";
      case "disconnected":
      case "error":
        return "bg-red-500";
    }
  };

  const statusText = (status: ConnectionStatus) => {
    switch (status) {
      case "connected":
        return "Connected";
      case "connecting":
        return "Connecting...";
      case "disconnected":
        return "Disconnected";
      case "error":
        return "Error";
    }
  };

  return (
    <div class="h-screen flex flex-col bg-gray-900 text-gray-100">
      {/* Header */}
      <header class="flex items-center justify-between px-4 py-3 bg-gray-800 border-b border-gray-700">
        <h1 class="text-lg font-semibold">Tidepool</h1>
        <div class="flex items-center gap-3">
          {/* Graph node indicator (for debugging) */}
          <Show when={socket.state().uiState}>
            {(state) => (
              <span class="text-xs text-gray-400 font-mono">
                {state().graphNode}
              </span>
            )}
          </Show>
          {/* Connection status */}
          <div class="flex items-center gap-2">
            <span
              class={`w-2 h-2 rounded-full ${statusColor(socket.state().status)}`}
            />
            <span class="text-sm text-gray-400">
              {statusText(socket.state().status)}
            </span>
          </div>
          {/* Reconnect button when disconnected */}
          <Show when={socket.state().status === "disconnected" || socket.state().status === "error"}>
            <button
              onClick={() => socket.reconnect()}
              class="text-sm text-blue-400 hover:text-blue-300"
            >
              Reconnect
            </button>
          </Show>
        </div>
      </header>

      {/* Chat messages */}
      <div
        ref={chatContainerRef}
        class="flex-1 overflow-y-auto p-4 chat-scroll"
        role="list"
        aria-label="Chat messages"
      >
        <For each={socket.state().uiState?.messages ?? []}>
          {(message) => <ChatMessage message={message} />}
        </For>

        {/* Thinking indicator */}
        <Show when={socket.state().uiState?.thinking}>
          <div class="flex justify-start mb-3">
            <div class="bg-gray-700 text-gray-400 rounded-2xl rounded-bl-sm px-4 py-3">
              <div class="flex gap-1 animate-pulse-slow">
                <span class="w-2 h-2 bg-gray-400 rounded-full" />
                <span class="w-2 h-2 bg-gray-400 rounded-full" style="animation-delay: 0.2s" />
                <span class="w-2 h-2 bg-gray-400 rounded-full" style="animation-delay: 0.4s" />
              </div>
            </div>
          </div>
        </Show>
      </div>

      {/* Input area - shows based on server affordances */}
      <Show when={socket.state().uiState?.buttons?.length}>
        <ButtonGroup
          buttons={socket.state().uiState!.buttons!}
          disabled={socket.state().uiState?.thinking ?? false}
          onSend={handleSend}
        />
      </Show>

      <Show when={socket.state().uiState?.textInput || socket.state().uiState?.photoUpload}>
        <InputArea
          textInput={socket.state().uiState?.textInput ?? null}
          photoUpload={socket.state().uiState?.photoUpload ?? null}
          disabled={socket.state().uiState?.thinking ?? false}
          onSend={handleSend}
        />
      </Show>

      {/* Error display */}
      <Show when={socket.state().error}>
        <div class="bg-red-900/50 border-t border-red-700 px-4 py-2 text-sm text-red-200">
          {socket.state().error}
        </div>
      </Show>
    </div>
  );
};

export default App;
