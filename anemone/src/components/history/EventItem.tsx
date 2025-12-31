import { type Component, Show, createSignal } from "solid-js";
import type { TimelineEvent } from "../../types";
import { Badge, eventTypeVariant, JsonViewer } from "../shared";

interface EventItemProps {
  event: TimelineEvent;
}

export const EventItem: Component<EventItemProps> = (props) => {
  const [expanded, setExpanded] = createSignal(false);

  const hasData = () => props.event.data !== undefined;

  return (
    <div
      class={`px-3 py-2 hover:bg-bg-hover cursor-pointer transition-colors ${
        expanded() ? "bg-bg-hover" : ""
      }`}
      onClick={() => hasData() && setExpanded(!expanded())}
    >
      {/* Main row */}
      <div class="flex items-start gap-2">
        {/* Timestamp */}
        <span class="text-xs text-text-dim w-12 flex-shrink-0 pt-0.5">
          {formatTimestamp(props.event.timestamp)}
        </span>

        {/* Badge */}
        <Badge variant={eventTypeVariant(props.event.type)} class="flex-shrink-0">
          {formatEventType(props.event.type)}
        </Badge>

        {/* Summary */}
        <span class="text-sm text-text-primary flex-1 truncate">
          {props.event.summary}
        </span>

        {/* Expand indicator */}
        <Show when={hasData()}>
          <span class="text-xs text-text-dim">
            {expanded() ? "▼" : "▶"}
          </span>
        </Show>
      </div>

      {/* Node label */}
      <Show when={props.event.node}>
        <div class="mt-1 ml-14 text-xs text-accent">
          {props.event.node}
        </div>
      </Show>

      {/* Expanded data */}
      <Show when={expanded() && hasData()}>
        <div class="mt-2 ml-14 p-2 bg-bg-primary rounded max-h-40 overflow-y-auto">
          <JsonViewer data={props.event.data} initialExpanded={false} />
        </div>
      </Show>
    </div>
  );
};

function formatTimestamp(date: Date): string {
  return date.toLocaleTimeString([], {
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
  }).slice(-5); // Just show :ss
}

function formatEventType(type: string): string {
  return type.replace(/_/g, " ").replace(/\b\w/g, (c) => c.toUpperCase());
}
