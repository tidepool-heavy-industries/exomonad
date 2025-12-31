import { type Component, For, Show, createEffect } from "solid-js";
import { useChat } from "../../stores";
import { useAutoScroll } from "../../hooks";
import { MessageBubble } from "./MessageBubble";
import { TypingIndicator } from "../shared";

export const MessageList: Component = () => {
  let containerRef: HTMLDivElement | undefined;
  const [chat] = useChat();
  const { scrollToBottom, checkScroll } = useAutoScroll(() => containerRef);

  // Auto-scroll on new messages
  createEffect(() => {
    chat.messages.length; // Subscribe to length changes
    scrollToBottom();
  });

  return (
    <div
      ref={containerRef}
      class="flex-1 overflow-y-auto p-4 space-y-4"
      onScroll={checkScroll}
    >
      {/* Empty state */}
      <Show when={chat.messages.length === 0 && !chat.isLoading}>
        <div class="flex flex-col items-center justify-center h-full text-center">
          <div class="text-4xl mb-4">🐚</div>
          <h3 class="text-lg font-medium text-text-primary mb-2">
            Welcome to Anemone
          </h3>
          <p class="text-sm text-text-muted max-w-xs">
            Send a message to start interacting with the graph execution engine.
          </p>
        </div>
      </Show>

      {/* Messages */}
      <For each={chat.messages}>
        {(message) => <MessageBubble message={message} />}
      </For>

      {/* Typing indicator */}
      <Show when={chat.isLoading}>
        <div class="flex justify-start">
          <div class="bg-bubble-assistant rounded-2xl rounded-bl-sm">
            <TypingIndicator />
          </div>
        </div>
      </Show>
    </div>
  );
};
