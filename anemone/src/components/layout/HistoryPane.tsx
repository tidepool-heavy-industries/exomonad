import { type Component, Show } from "solid-js";
import { EventTimeline } from "../history/EventTimeline";
import { EventFilter } from "../history/EventFilter";

interface HistoryPaneProps {
  onClose?: () => void;
}

export const HistoryPane: Component<HistoryPaneProps> = (props) => {
  return (
    <div class="w-80 md:w-80 w-full flex-shrink-0 border-l border-bg-hover bg-bg-secondary flex flex-col h-full overflow-hidden">
      {/* Header */}
      <div class="p-3 border-b border-bg-hover flex items-center justify-between">
        <h2 class="text-sm font-semibold text-text-muted uppercase tracking-wider">
          History
        </h2>
        <Show when={props.onClose}>
          <button
            type="button"
            onClick={props.onClose}
            class="md:hidden p-1 rounded text-text-muted hover:text-text-primary hover:bg-bg-hover transition-colors"
            aria-label="Close history panel"
          >
            <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12" />
            </svg>
          </button>
        </Show>
      </div>

      {/* Filter */}
      <EventFilter />

      {/* Timeline */}
      <EventTimeline />
    </div>
  );
};
