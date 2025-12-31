import { onCleanup } from "solid-js";
import { useConnection } from "../stores/connectionStore";
import { useGraph } from "../stores/graphStore";
import { useChat } from "../stores/chatStore";
import { useEvents } from "../stores/eventStore";
import { GraphWebSocket, createSession } from "../services/websocket";
import type { LlmCompleteEffect } from "../types";

/**
 * Singleton WebSocket instance.
 * WARNING: Only ONE component should use useWebSocket() at a time.
 * The onCleanup will disconnect when that component unmounts.
 * Currently only ChatInput uses this hook.
 */
let wsInstance: GraphWebSocket | null = null;

export function useWebSocket() {
  const [, connectionActions] = useConnection();
  const [, graphActions] = useGraph();
  const [, chatActions] = useChat();
  const [, eventActions] = useEvents();

  const connect = async (baseUrl: string) => {
    // Create new session
    try {
      const { sessionId, wsUrl } = await createSession(baseUrl);

      // Build full WebSocket URL
      // In dev mode with Vite proxy: baseUrl="/api", need "/ws" prefix for WS
      // In production: baseUrl="https://...", convert to "wss://..."
      let fullWsUrl: string;
      if (baseUrl.startsWith("/")) {
        // Dev mode - use /ws prefix for WebSocket proxy
        fullWsUrl = `/ws${wsUrl}`;
      } else {
        // Production - convert http(s) to ws(s)
        fullWsUrl = baseUrl.replace(/^http/, "ws") + wsUrl;
      }

      connectionActions.connect(fullWsUrl, sessionId);

      wsInstance = new GraphWebSocket({
        onOpen() {
          connectionActions.setStatus("connected");
          eventActions.addEvent({
            type: "connected",
            summary: `Connected to session ${sessionId}`,
            data: { sessionId },
          });
        },

        onClose(code, reason) {
          connectionActions.setStatus("disconnected");
          eventActions.addEvent({
            type: "disconnected",
            summary: `Disconnected: ${reason || "Connection closed"}`,
            data: { code, reason },
          });
        },

        onError() {
          connectionActions.setError("Connection failed");
        },

        onProgress(node, effectType) {
          graphActions.updatePhase({ type: "in_node", nodeName: node });
          eventActions.addEvent({
            type: "progress",
            node,
            summary: `Executing ${effectType} in ${node}`,
            data: { effectType },
          });
        },

        onSuspend(effect) {
          graphActions.setActiveEffect(effect);

          if (effect.type === "LlmComplete") {
            const llmEffect = effect as LlmCompleteEffect;
            eventActions.addEvent({
              type: "llm_start",
              node: llmEffect.eff_node,
              summary: `LLM call: ${llmEffect.eff_node}`,
              data: {
                systemPrompt: llmEffect.eff_system_prompt.slice(0, 100) + "...",
                hasSchema: !!llmEffect.eff_schema,
              },
            });
          } else if (effect.type === "HttpFetch") {
            eventActions.addEvent({
              type: "http_start",
              summary: `HTTP ${effect.eff_method} ${effect.eff_url}`,
              data: effect,
            });
          } else if (effect.type === "LogInfo") {
            eventActions.addEvent({
              type: "log_info",
              summary: effect.eff_message,
            });
          } else if (effect.type === "LogError") {
            eventActions.addEvent({
              type: "log_error",
              summary: effect.eff_message,
            });
          }

          eventActions.addEvent({
            type: "suspend",
            summary: `Awaiting ${effect.type}`,
            data: effect,
          });
        },

        onDone(result) {
          graphActions.updatePhase({ type: "completed", result });
          graphActions.setActiveEffect(null);
          chatActions.setLoading(false);

          eventActions.addEvent({
            type: "done",
            summary: "Graph execution completed",
            data: result,
          });

          // Add result as assistant message if it has responseText
          if (
            typeof result === "object" &&
            result !== null &&
            "responseText" in result
          ) {
            chatActions.addMessage({
              role: "assistant",
              content: (result as { responseText: string }).responseText,
            });
          }
        },

        onServerError(message) {
          graphActions.updatePhase({ type: "failed", error: message });
          connectionActions.setError(message);
          chatActions.setLoading(false);

          eventActions.addEvent({
            type: "error",
            summary: message,
          });

          chatActions.addMessage({
            role: "system",
            content: `Error: ${message}`,
          });
        },
      });

      await wsInstance.connect(fullWsUrl);
    } catch (err) {
      const message = err instanceof Error ? err.message : "Connection failed";
      connectionActions.setError(message);
      throw err;
    }
  };

  const sendMessage = (text: string) => {
    if (!wsInstance?.isConnected) {
      console.error("Cannot send message: not connected");
      return;
    }

    // Add user message to chat
    chatActions.addMessage({ role: "user", content: text });
    chatActions.setLoading(true);

    // Reset graph state for new execution
    graphActions.reset();

    // Send to server
    wsInstance.init({ messageText: text });

    eventActions.addEvent({
      type: "progress",
      summary: "Graph initialized",
      data: { input: text },
    });
  };

  const disconnect = () => {
    wsInstance?.disconnect();
    wsInstance = null;
    connectionActions.disconnect();
  };

  // Cleanup on unmount
  onCleanup(() => {
    disconnect();
  });

  return {
    connect,
    disconnect,
    sendMessage,
    isConnected: () => wsInstance?.isConnected ?? false,
  };
}
