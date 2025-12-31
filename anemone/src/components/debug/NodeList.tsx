import { type Component, For, Show } from "solid-js";
import { useGraph } from "../../stores";
import { getCurrentNode } from "../../types";

export const NodeList: Component = () => {
  const [state] = useGraph();

  const nodes = () => state.graphInfo?.nodes ?? [];
  const completedNodes = () => state.completedNodes;
  const activeNode = () => getCurrentNode(state.phase);

  return (
    <div class="space-y-1">
      <Show
        when={nodes().length > 0}
        fallback={
          <div class="text-sm text-text-dim italic p-2">
            No graph info available
          </div>
        }
      >
        <For each={nodes()}>
          {(node) => (
            <NodeItem
              name={node.niName}
              kind={node.niKind}
              isActive={activeNode() === node.niName}
              isCompleted={completedNodes().includes(node.niName)}
            />
          )}
        </For>
      </Show>
    </div>
  );
};

interface NodeItemProps {
  name: string;
  kind: "LLM" | "Logic";
  isActive: boolean;
  isCompleted: boolean;
}

const NodeItem: Component<NodeItemProps> = (props) => {
  const statusIcon = () => {
    if (props.isCompleted) return "✓";
    if (props.isActive) return "▶";
    return "○";
  };

  const statusColor = () => {
    if (props.isCompleted) return "text-phase-completed";
    if (props.isActive) return "text-phase-active";
    return "text-text-dim";
  };

  return (
    <div
      class={`flex items-center gap-2 px-2 py-1.5 rounded transition-colors ${
        props.isActive ? "bg-phase-active/10" : "hover:bg-bg-hover"
      }`}
    >
      {/* Status icon */}
      <span class={`w-4 text-center ${statusColor()}`}>{statusIcon()}</span>

      {/* Node name */}
      <span
        class={`flex-1 text-sm truncate ${
          props.isActive ? "text-phase-active font-medium" : "text-text-primary"
        }`}
      >
        {props.name}
      </span>

      {/* Kind badge */}
      <span
        class={`text-xs px-1.5 py-0.5 rounded ${
          props.kind === "LLM"
            ? "bg-accent/20 text-accent"
            : "bg-bg-hover text-text-muted"
        }`}
      >
        {props.kind}
      </span>
    </div>
  );
};
