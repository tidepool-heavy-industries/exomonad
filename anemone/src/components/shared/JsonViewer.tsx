import { type Component, For, Show, createSignal } from "solid-js";

interface JsonViewerProps {
  data: unknown;
  initialExpanded?: boolean;
  depth?: number;
  maxDepth?: number;
}

export const JsonViewer: Component<JsonViewerProps> = (props) => {
  const depth = props.depth ?? 0;
  const maxDepth = props.maxDepth ?? 5;

  // For primitive values, just render them
  if (props.data === null) {
    return <span class="text-text-muted italic">null</span>;
  }

  if (props.data === undefined) {
    return <span class="text-text-muted italic">undefined</span>;
  }

  if (typeof props.data === "boolean") {
    return (
      <span class="text-phase-active">{props.data ? "true" : "false"}</span>
    );
  }

  if (typeof props.data === "number") {
    return <span class="text-phase-transitioning">{props.data}</span>;
  }

  if (typeof props.data === "string") {
    // Truncate long strings
    const truncated =
      props.data.length > 100 ? props.data.slice(0, 100) + "..." : props.data;
    return <span class="text-phase-completed">"{truncated}"</span>;
  }

  // For arrays and objects, render expandable tree
  if (Array.isArray(props.data)) {
    return (
      <CollapsibleNode
        label={`Array(${props.data.length})`}
        initialExpanded={props.initialExpanded ?? depth < 1}
        disabled={depth >= maxDepth}
      >
        <div class="pl-4 border-l border-bg-hover">
          <For each={props.data}>
            {(item, index) => (
              <div class="py-0.5">
                <span class="text-text-dim">{index()}: </span>
                <JsonViewer
                  data={item}
                  depth={depth + 1}
                  maxDepth={maxDepth}
                />
              </div>
            )}
          </For>
        </div>
      </CollapsibleNode>
    );
  }

  if (typeof props.data === "object") {
    const entries = Object.entries(props.data);
    return (
      <CollapsibleNode
        label={`Object(${entries.length})`}
        initialExpanded={props.initialExpanded ?? depth < 1}
        disabled={depth >= maxDepth}
      >
        <div class="pl-4 border-l border-bg-hover">
          <For each={entries}>
            {([key, value]) => (
              <div class="py-0.5">
                <span class="text-accent">{key}: </span>
                <JsonViewer
                  data={value}
                  depth={depth + 1}
                  maxDepth={maxDepth}
                />
              </div>
            )}
          </For>
        </div>
      </CollapsibleNode>
    );
  }

  // Fallback for other types
  return <span class="text-text-muted">{String(props.data)}</span>;
};

interface CollapsibleNodeProps {
  label: string;
  initialExpanded?: boolean;
  disabled?: boolean;
  children: any;
}

const CollapsibleNode: Component<CollapsibleNodeProps> = (props) => {
  const [expanded, setExpanded] = createSignal(props.initialExpanded ?? false);

  return (
    <div>
      <button
        type="button"
        class="flex items-center gap-1 text-text-muted hover:text-text-primary transition-colors disabled:cursor-not-allowed"
        onClick={() => setExpanded(!expanded())}
        disabled={props.disabled}
      >
        <span class="w-4 text-xs">
          {props.disabled ? "..." : expanded() ? "▼" : "▶"}
        </span>
        <span class="font-mono text-sm">{props.label}</span>
      </button>
      <Show when={expanded() && !props.disabled}>{props.children}</Show>
    </div>
  );
};

/** Simple one-line JSON preview */
export const JsonPreview: Component<{ data: unknown; maxLength?: number }> = (
  props
) => {
  const preview = () => {
    try {
      const str = JSON.stringify(props.data);
      const max = props.maxLength ?? 50;
      return str.length > max ? str.slice(0, max) + "..." : str;
    } catch {
      return "[Object]";
    }
  };

  return <span class="font-mono text-sm text-text-muted">{preview()}</span>;
};
