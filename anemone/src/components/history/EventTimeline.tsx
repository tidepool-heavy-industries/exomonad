import { type Component, For, Show, createEffect, createMemo } from "solid-js";
import { useEventFilter } from "../../hooks";
import { useAutoScroll } from "../../hooks";
import { EventItem } from "./EventItem";

export const EventTimeline: Component = () => {
  let containerRef: HTMLDivElement | undefined;
  const { filteredEvents } = useEventFilter();
  const { scrollToBottom, checkScroll } = useAutoScroll(() => containerRef);

  // Group events by minute
  const grouped = createMemo(() => {
    const groups: { time: string; events: ReturnType<typeof filteredEvents> }[] = [];
    let currentGroup: typeof groups[number] | null = null;

    for (const event of filteredEvents()) {
      const time = formatMinute(event.timestamp);
      if (!currentGroup || currentGroup.time !== time) {
        currentGroup = { time, events: [] };
        groups.push(currentGroup);
      }
      currentGroup.events.push(event);
    }

    return groups;
  });

  // Auto-scroll on new events
  createEffect(() => {
    filteredEvents().length;
    scrollToBottom();
  });

  return (
    <div
      ref={containerRef}
      class="flex-1 overflow-y-auto"
      onScroll={checkScroll}
    >
      <Show
        when={filteredEvents().length > 0}
        fallback={
          <div class="p-4 text-sm text-text-dim italic text-center">
            No events yet
          </div>
        }
      >
        <For each={grouped()}>
          {(group) => (
            <div class="mb-2">
              {/* Time header */}
              <div class="sticky top-0 px-3 py-1 text-xs text-text-dim bg-bg-secondary/90 backdrop-blur-sm border-b border-bg-hover">
                {group.time}
              </div>

              {/* Events in this group */}
              <For each={group.events}>
                {(event) => <EventItem event={event} />}
              </For>
            </div>
          )}
        </For>
      </Show>
    </div>
  );
};

function formatMinute(date: Date): string {
  return date.toLocaleTimeString([], {
    hour: "2-digit",
    minute: "2-digit",
  });
}
