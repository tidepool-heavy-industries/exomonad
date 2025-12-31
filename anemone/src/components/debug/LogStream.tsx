import { type Component, For, createMemo, createEffect } from "solid-js";
import { useEvents } from "../../stores";
import { useAutoScroll } from "../../hooks";

export const LogStream: Component = () => {
  let containerRef: HTMLDivElement | undefined;
  const [events] = useEvents();
  const { scrollToBottom, checkScroll } = useAutoScroll(() => containerRef);

  // Filter to log events only
  const logs = createMemo(() =>
    events.events.filter(
      (e) => e.type === "log_info" || e.type === "log_error"
    )
  );

  // Auto-scroll on new logs
  createEffect(() => {
    logs().length; // Subscribe to length changes
    scrollToBottom();
  });

  return (
    <div
      ref={containerRef}
      class="flex-1 overflow-y-auto font-mono text-xs p-2 space-y-1"
      onScroll={checkScroll}
    >
      <For each={logs()}>
        {(log) => (
          <div
            class={`py-0.5 ${
              log.type === "log_error" ? "text-phase-failed" : "text-text-muted"
            }`}
          >
            <span class="text-text-dim">
              {formatTime(log.timestamp)}
            </span>{" "}
            {log.summary}
          </div>
        )}
      </For>

      {logs().length === 0 && (
        <div class="text-text-dim italic">No logs yet</div>
      )}
    </div>
  );
};

function formatTime(date: Date): string {
  return date.toLocaleTimeString([], {
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
  });
}
