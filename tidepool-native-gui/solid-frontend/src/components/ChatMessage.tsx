import type { Component } from "solid-js";
import type { ChatMessage as ChatMessageType } from "../lib/types";

interface Props {
  message: ChatMessageType;
}

const ChatMessage: Component<Props> = (props) => {
  const isUser = () => props.message.role === "user";
  const isSystem = () => props.message.role === "system";

  const formatTime = (timestamp: string) => {
    try {
      const date = new Date(timestamp);
      return date.toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" });
    } catch {
      return "";
    }
  };

  return (
    <div
      class={`flex ${isUser() ? "justify-end" : "justify-start"} mb-3`}
      role="listitem"
    >
      <div
        class={`max-w-[80%] rounded-2xl px-4 py-2 ${
          isUser()
            ? "bg-blue-600 text-white rounded-br-sm"
            : isSystem()
            ? "bg-yellow-900/50 text-yellow-100 border border-yellow-700/50"
            : "bg-gray-700 text-gray-100 rounded-bl-sm"
        }`}
      >
        <p class="whitespace-pre-wrap break-words">{props.message.content}</p>
        <time
          class={`text-xs mt-1 block ${
            isUser() ? "text-blue-200" : isSystem() ? "text-yellow-400" : "text-gray-400"
          }`}
          datetime={props.message.timestamp}
        >
          {formatTime(props.message.timestamp)}
        </time>
      </div>
    </div>
  );
};

export default ChatMessage;
