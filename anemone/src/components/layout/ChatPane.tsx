import type { Component } from "solid-js";
import { MessageList } from "../chat/MessageList";
import { ChatInput } from "../chat/ChatInput";
import { ConnectionStatus } from "../chat/ConnectionStatus";

export const ChatPane: Component = () => {
  return (
    <div class="flex-1 flex flex-col h-full bg-bg-primary">
      {/* Header */}
      <div class="flex items-center justify-between p-3 border-b border-bg-hover">
        <h2 class="text-sm font-semibold text-text-muted uppercase tracking-wider">
          Chat
        </h2>
        <ConnectionStatus />
      </div>

      {/* Messages */}
      <MessageList />

      {/* Input */}
      <ChatInput />
    </div>
  );
};
