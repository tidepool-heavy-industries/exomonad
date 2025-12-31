import { type Component, For, createSignal, Show } from "solid-js";
import { useEventFilter } from "../../hooks";
import type { EventType } from "../../types";

interface FilterGroup {
  label: string;
  types: { type: EventType; label: string }[];
}

const filterGroups: FilterGroup[] = [
  {
    label: "Execution",
    types: [
      { type: "progress", label: "Progress" },
      { type: "transition", label: "Transition" },
      { type: "suspend", label: "Suspend" },
      { type: "done", label: "Done" },
      { type: "error", label: "Error" },
    ],
  },
  {
    label: "Effects",
    types: [
      { type: "llm_start", label: "LLM Start" },
      { type: "llm_complete", label: "LLM Complete" },
      { type: "http_start", label: "HTTP Start" },
      { type: "http_complete", label: "HTTP Complete" },
    ],
  },
  {
    label: "Logs",
    types: [
      { type: "log_info", label: "Info" },
      { type: "log_error", label: "Error" },
    ],
  },
  {
    label: "Connection",
    types: [
      { type: "connected", label: "Connected" },
      { type: "disconnected", label: "Disconnected" },
    ],
  },
];

export const EventFilter: Component = () => {
  const { filter, toggleType, clearFilter } = useEventFilter();
  const [expanded, setExpanded] = createSignal(false);

  const isActive = (type: EventType) => filter().types.includes(type);
  const hasFilters = () => filter().types.length > 0;
  const activeCount = () => filter().types.length;

  // Quick filter - common types shown by default
  const quickFilters: { type: EventType; label: string }[] = [
    { type: "progress", label: "Progress" },
    { type: "llm_start", label: "LLM" },
    { type: "error", label: "Errors" },
    { type: "log_info", label: "Logs" },
  ];

  return (
    <div class="border-b border-bg-hover">
      {/* Quick filters row */}
      <div class="p-2 flex flex-wrap items-center gap-1">
        <For each={quickFilters}>
          {(btn) => (
            <button
              type="button"
              onClick={() => toggleType(btn.type)}
              class={`px-2 py-1 text-xs rounded-full transition-colors ${
                isActive(btn.type)
                  ? "bg-phase-active text-white"
                  : "bg-bg-hover text-text-muted hover:bg-bg-active"
              }`}
            >
              {btn.label}
            </button>
          )}
        </For>

        {/* Expand/collapse button */}
        <button
          type="button"
          onClick={() => setExpanded(!expanded())}
          class="px-2 py-1 text-xs text-text-dim hover:text-text-muted transition-colors flex items-center gap-1"
        >
          <Show when={expanded()} fallback="More">
            Less
          </Show>
          <svg
            class={`w-3 h-3 transition-transform ${expanded() ? "rotate-180" : ""}`}
            fill="none"
            stroke="currentColor"
            viewBox="0 0 24 24"
          >
            <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M19 9l-7 7-7-7" />
          </svg>
        </button>

        {/* Clear button */}
        <Show when={hasFilters()}>
          <button
            type="button"
            onClick={clearFilter}
            class="px-2 py-1 text-xs text-text-dim hover:text-phase-failed transition-colors ml-auto"
          >
            Clear ({activeCount()})
          </button>
        </Show>
      </div>

      {/* Expanded filter groups */}
      <Show when={expanded()}>
        <div class="px-2 pb-2 space-y-2">
          <For each={filterGroups}>
            {(group) => (
              <div class="space-y-1">
                <div class="text-xs text-text-dim">{group.label}</div>
                <div class="flex flex-wrap gap-1">
                  <For each={group.types}>
                    {(btn) => (
                      <button
                        type="button"
                        onClick={() => toggleType(btn.type)}
                        class={`px-2 py-0.5 text-xs rounded transition-colors ${
                          isActive(btn.type)
                            ? "bg-phase-active text-white"
                            : "bg-bg-hover text-text-muted hover:bg-bg-active"
                        }`}
                      >
                        {btn.label}
                      </button>
                    )}
                  </For>
                </div>
              </div>
            )}
          </For>
        </div>
      </Show>
    </div>
  );
};
