import { type Component, Show } from "solid-js";
import { ExecutionPhase } from "../debug/ExecutionPhase";
import { NodeList } from "../debug/NodeList";
import { ActiveEffect } from "../debug/ActiveEffect";
import { LogStream } from "../debug/LogStream";

interface DebugPaneProps {
  onClose?: () => void;
}

export const DebugPane: Component<DebugPaneProps> = (props) => {
  return (
    <div class="w-full md:w-72 flex-shrink-0 border-r border-bg-hover bg-bg-secondary flex flex-col h-full overflow-hidden">
      {/* Header */}
      <div class="p-3 border-b border-bg-hover flex items-center justify-between">
        <h2 class="text-sm font-semibold text-text-muted uppercase tracking-wider">
          Debug
        </h2>
        <Show when={props.onClose}>
          <button
            type="button"
            onClick={props.onClose}
            class="md:hidden p-1 rounded text-text-muted hover:text-text-primary hover:bg-bg-hover transition-colors"
            aria-label="Close debug panel"
          >
            <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M6 18L18 6M6 6l12 12" />
            </svg>
          </button>
        </Show>
      </div>

      {/* Content */}
      <div class="flex-1 overflow-y-auto p-3 space-y-4">
        {/* Execution Phase */}
        <section>
          <h3 class="text-xs font-semibold text-text-dim uppercase tracking-wider mb-2">
            Phase
          </h3>
          <ExecutionPhase />
        </section>

        {/* Node List */}
        <section>
          <h3 class="text-xs font-semibold text-text-dim uppercase tracking-wider mb-2">
            Nodes
          </h3>
          <NodeList />
        </section>

        {/* Active Effect */}
        <section>
          <h3 class="text-xs font-semibold text-text-dim uppercase tracking-wider mb-2">
            Active Effect
          </h3>
          <ActiveEffect />
        </section>
      </div>

      {/* Log Stream at bottom - flexible height */}
      <div class="min-h-32 max-h-64 flex-shrink-0 border-t border-bg-hover flex flex-col">
        <div class="p-2 border-b border-bg-hover flex-shrink-0">
          <h3 class="text-xs font-semibold text-text-dim uppercase tracking-wider">
            Logs
          </h3>
        </div>
        <LogStream />
      </div>
    </div>
  );
};

export type { DebugPaneProps };
