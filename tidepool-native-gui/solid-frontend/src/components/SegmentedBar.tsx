import { For, Show, type Component } from "solid-js";

interface Props {
  total: number;          // Total segments
  filled: number;         // Filled count
  label?: string;         // Optional label above bar
  color?: string;         // Tailwind fill color (default: "bg-blue-500")
  emptyColor?: string;    // Tailwind empty color (default: "bg-gray-700")
  showCount?: boolean;    // Show "5/9" (default: true)
  size?: "sm" | "md" | "lg";
}

/**
 * SegmentedBar renders a horizontal bar with N segments, some filled.
 * Useful for stress, heat, HP, mana, or any numeric tracker with a max.
 */
const SegmentedBar: Component<Props> = (props) => {
  const sizeClasses = () => {
    switch (props.size) {
      case "sm": return { segment: "h-2", gap: "gap-0.5" };
      case "lg": return { segment: "h-5", gap: "gap-1.5" };
      default: return { segment: "h-3", gap: "gap-1" };
    }
  };

  // Clamp values to valid ranges
  const safeTotal = () => Math.max(1, props.total);
  const safeFilled = () => Math.max(0, Math.min(props.filled, safeTotal()));
  const segments = () => Array.from({ length: safeTotal() }, (_, i) => i < safeFilled());

  return (
    <div class="flex flex-col">
      {/* Label */}
      <Show when={props.label}>
        <span class="text-xs font-medium text-gray-400 uppercase tracking-wide mb-1">
          {props.label}
        </span>
      </Show>

      <div class="flex items-center gap-2">
        {/* Segments */}
        <div class={`flex ${sizeClasses().gap} flex-1`}>
          <For each={segments()}>
            {(isFilled) => (
              <div
                class={`
                  flex-1 rounded-sm transition-colors
                  ${sizeClasses().segment}
                  ${isFilled ? (props.color ?? "bg-blue-500") : (props.emptyColor ?? "bg-gray-700")}
                `}
              />
            )}
          </For>
        </div>

        {/* Count */}
        <Show when={props.showCount !== false}>
          <span class="text-xs text-gray-500 font-mono min-w-[32px] text-right">
            {safeFilled()}/{safeTotal()}
          </span>
        </Show>
      </div>
    </div>
  );
};

export default SegmentedBar;
