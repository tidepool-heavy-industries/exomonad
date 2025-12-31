import { type Component, Show } from "solid-js";
import type { ChatMessage } from "../../types";

interface MessageBubbleProps {
  message: ChatMessage;
}

export const MessageBubble: Component<MessageBubbleProps> = (props) => {
  const isUser = () => props.message.role === "user";
  const isSystem = () => props.message.role === "system";

  const bubbleClasses = () => {
    if (isUser()) {
      return "bg-bubble-user text-white rounded-br-sm ml-auto";
    }
    if (isSystem()) {
      return "bg-phase-failed/20 text-phase-failed rounded-sm mx-auto text-center";
    }
    return "bg-bubble-assistant text-text-primary rounded-bl-sm";
  };

  return (
    <div class={`flex ${isUser() ? "justify-end" : isSystem() ? "justify-center" : "justify-start"}`}>
      <div class={`max-w-[80%] rounded-2xl px-4 py-2 ${bubbleClasses()}`}>
        {/* Node label for assistant messages */}
        <Show when={!isUser() && !isSystem() && props.message.node}>
          <span class="text-xs text-accent block mb-1">
            {props.message.node}
          </span>
        </Show>

        {/* Message content */}
        <p class="whitespace-pre-wrap break-words">{props.message.content}</p>

        {/* Timestamp */}
        <span class="text-xs opacity-60 mt-1 block">
          {formatTime(props.message.timestamp)}
        </span>
      </div>
    </div>
  );
};

function formatTime(date: Date): string {
  return date.toLocaleTimeString([], {
    hour: "2-digit",
    minute: "2-digit",
  });
}
