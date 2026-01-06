import { type Component, createSignal, onMount, Show } from "solid-js";
import { A } from "@solidjs/router";
import { renderGraph } from "../lib/graphRenderer";
import type { GraphExport } from "../lib/types";

const GraphView: Component = () => {
  const [graph, setGraph] = createSignal<GraphExport | null>(null);
  const [error, setError] = createSignal<string | null>(null);
  const [loading, setLoading] = createSignal(true);
  let svgContainer: HTMLDivElement | undefined;

  onMount(async () => {
    try {
      const response = await fetch("/graph/info");
      if (!response.ok) {
        if (response.status === 404) {
          throw new Error("No graph configured. Server is running a plain agent.");
        }
        throw new Error(`Failed to fetch graph: ${response.statusText}`);
      }
      const data: GraphExport = await response.json();
      setGraph(data);

      // Render after state update
      setTimeout(() => {
        if (svgContainer && data) {
          renderGraph(svgContainer, data);
        }
      }, 0);
    } catch (e) {
      setError(e instanceof Error ? e.message : "Unknown error");
    } finally {
      setLoading(false);
    }
  });

  return (
    <div class="h-screen flex flex-col bg-gray-900 text-gray-100">
      {/* Header */}
      <header class="flex items-center justify-between px-4 py-3 bg-gray-800 border-b border-gray-700">
        <h1 class="text-lg font-semibold">Graph Visualization</h1>
        <div class="flex items-center gap-3">
          <A
            href="/"
            class="text-sm text-blue-400 hover:text-blue-300 transition-colors"
          >
            Back to Chat
          </A>
          {/* Graph info */}
          <Show when={graph()}>
            {(g) => (
              <span class="text-xs text-gray-400">
                {Object.keys(g().nodes).length} nodes, {g().edges.length} edges
              </span>
            )}
          </Show>
        </div>
      </header>

      {/* Graph container */}
      <div class="flex-1 overflow-hidden relative">
        <Show when={loading()}>
          <div class="absolute inset-0 flex items-center justify-center">
            <div class="text-gray-400 flex flex-col items-center gap-2">
              <div class="w-8 h-8 border-2 border-gray-600 border-t-blue-500 rounded-full animate-spin" />
              <span>Loading graph...</span>
            </div>
          </div>
        </Show>

        <Show when={error()}>
          <div class="absolute inset-0 flex items-center justify-center">
            <div class="bg-red-900/30 border border-red-700 rounded-lg p-6 max-w-md text-center">
              <h2 class="text-lg font-semibold text-red-400 mb-2">Error</h2>
              <p class="text-red-200">{error()}</p>
              <A
                href="/"
                class="mt-4 inline-block px-4 py-2 bg-gray-700 hover:bg-gray-600 rounded-lg text-sm"
              >
                Return to Chat
              </A>
            </div>
          </div>
        </Show>

        <Show when={graph() && !loading() && !error()}>
          <div ref={svgContainer} class="w-full h-full" />
        </Show>
      </div>

      {/* Legend */}
      <Show when={graph() && !loading() && !error()}>
        <div class="bg-gray-800 border-t border-gray-700 px-4 py-2 flex items-center gap-6 text-xs text-gray-400">
          <div class="flex items-center gap-2">
            <div class="w-4 h-4 rounded-full bg-gray-600 border-2 border-green-500" />
            <span>Entry</span>
          </div>
          <div class="flex items-center gap-2">
            <div class="w-4 h-4 rounded-full bg-gray-600 border-2 border-red-500" />
            <span>Exit</span>
          </div>
          <div class="flex items-center gap-2">
            <div class="w-6 h-4 rounded-md bg-blue-900 border border-blue-500" />
            <span>LLM Node</span>
          </div>
          <div class="flex items-center gap-2">
            <svg width="20" height="16" viewBox="0 0 20 16">
              <polygon
                points="0,8 5,0 15,0 20,8 15,16 5,16"
                fill="#14532d"
                stroke="#22c55e"
                stroke-width="1"
              />
            </svg>
            <span>Logic Node</span>
          </div>
          <div class="flex-1" />
          <span class="text-gray-500">Scroll to zoom, drag to pan</span>
        </div>
      </Show>
    </div>
  );
};

export default GraphView;
