import { Show, type Component } from "solid-js";

type BadgeColor = "blue" | "green" | "yellow" | "orange" | "red" | "purple" | "amber" | "gray";

interface Props {
  label: string;
  variant?: string;       // Secondary text after ":"
  color?: BadgeColor;
  glow?: boolean;
  size?: "sm" | "md";
}

const colorClasses: Record<BadgeColor, { bg: string; text: string; glow: string }> = {
  blue: { bg: "bg-blue-900/50", text: "text-blue-300", glow: "shadow-blue-500/50" },
  green: { bg: "bg-green-900/50", text: "text-green-300", glow: "shadow-green-500/50" },
  yellow: { bg: "bg-yellow-900/50", text: "text-yellow-300", glow: "shadow-yellow-500/50" },
  orange: { bg: "bg-orange-900/50", text: "text-orange-300", glow: "shadow-orange-500/50" },
  red: { bg: "bg-red-900/50", text: "text-red-300", glow: "shadow-red-500/50" },
  purple: { bg: "bg-purple-900/50", text: "text-purple-300", glow: "shadow-purple-500/50" },
  amber: { bg: "bg-amber-900/50", text: "text-amber-300", glow: "shadow-amber-500/50" },
  gray: { bg: "bg-gray-700", text: "text-gray-300", glow: "shadow-gray-500/50" },
};

/**
 * StatusBadge renders a color-coded badge/chip.
 * Format: [Label: Variant] or [Label] if no variant.
 */
const StatusBadge: Component<Props> = (props) => {
  const color = () => colorClasses[props.color ?? "gray"];
  const sizeClasses = () => props.size === "sm" ? "px-2 py-0.5 text-xs" : "px-3 py-1 text-sm";

  return (
    <span
      class={`
        inline-flex items-center rounded-full font-medium
        border border-current/20
        ${color().bg} ${color().text}
        ${sizeClasses()}
        ${props.glow ? `shadow-lg ${color().glow}` : ""}
      `}
    >
      <span class="font-semibold">{props.label}</span>
      <Show when={props.variant}>
        <span class="opacity-60 mx-1">:</span>
        <span>{props.variant}</span>
      </Show>
    </span>
  );
};

export default StatusBadge;
