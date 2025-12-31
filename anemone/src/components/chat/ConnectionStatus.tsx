import { type Component, Match, Switch } from "solid-js";
import { useConnection } from "../../stores";

export const ConnectionStatus: Component = () => {
  const [connection] = useConnection();

  const dotColor = () => {
    switch (connection.status) {
      case "connected":
        return "bg-phase-completed";
      case "connecting":
        return "bg-phase-transitioning animate-pulse";
      case "error":
        return "bg-phase-failed";
      case "disconnected":
      default:
        return "bg-text-dim";
    }
  };

  return (
    <div class="flex items-center gap-2">
      <div class={`w-2 h-2 rounded-full ${dotColor()}`} />
      <span class="text-xs text-text-muted">
        <Switch fallback="Disconnected">
          <Match when={connection.status === "connected"}>
            Connected
            {connection.sessionId && (
              <span class="text-text-dim ml-1">
                ({connection.sessionId.slice(0, 8)})
              </span>
            )}
          </Match>
          <Match when={connection.status === "connecting"}>Connecting...</Match>
          <Match when={connection.status === "error"}>Error</Match>
        </Switch>
      </span>
    </div>
  );
};
