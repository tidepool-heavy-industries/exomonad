import type { Component } from "solid-js";
import type { MobilePane } from "./AppLayout";

interface MobileHeaderProps {
  activePane: MobilePane;
  onSelectPane: (pane: MobilePane) => void;
}

export const MobileHeader: Component<MobileHeaderProps> = (props) => {
  return (
    <div class="md:hidden flex items-center justify-between px-4 py-2 border-b border-bg-hover bg-bg-secondary">
      {/* Debug button */}
      <button
        type="button"
        onClick={() => props.onSelectPane("debug")}
        class={`p-2 rounded-lg transition-colors ${
          props.activePane === "debug"
            ? "bg-phase-active text-white"
            : "text-text-muted hover:bg-bg-hover"
        }`}
        aria-label="Open debug panel"
      >
        <svg
          class="w-5 h-5"
          fill="none"
          stroke="currentColor"
          viewBox="0 0 24 24"
        >
          <path
            stroke-linecap="round"
            stroke-linejoin="round"
            stroke-width="2"
            d="M12 6V4m0 2a2 2 0 100 4m0-4a2 2 0 110 4m-6 8a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4m6 6v10m6-2a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4"
          />
        </svg>
      </button>

      {/* Title */}
      <button
        type="button"
        onClick={() => props.onSelectPane("chat")}
        class="text-sm font-semibold text-text-primary"
      >
        Anemone
      </button>

      {/* History button */}
      <button
        type="button"
        onClick={() => props.onSelectPane("history")}
        class={`p-2 rounded-lg transition-colors ${
          props.activePane === "history"
            ? "bg-phase-active text-white"
            : "text-text-muted hover:bg-bg-hover"
        }`}
        aria-label="Open event history"
      >
        <svg
          class="w-5 h-5"
          fill="none"
          stroke="currentColor"
          viewBox="0 0 24 24"
        >
          <path
            stroke-linecap="round"
            stroke-linejoin="round"
            stroke-width="2"
            d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z"
          />
        </svg>
      </button>
    </div>
  );
};
