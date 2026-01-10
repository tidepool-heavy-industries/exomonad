import { Show, type Component } from "solid-js";

interface Props {
  value: string | number;
  label?: string;
  labelColor?: string;    // Tailwind text color class (e.g., "text-green-400")
  hint?: string;
  selected?: boolean;
  disabled?: boolean;
  onClick?: () => void;
  shortcut?: string;      // Keyboard shortcut shown in corner (e.g., "1")
}

/**
 * HintCard renders a selectable card with:
 * - A prominent value (number or text)
 * - An optional colored label below
 * - An optional hint/description text
 * - Selection and disabled states
 * - Optional keyboard shortcut indicator
 */
const HintCard: Component<Props> = (props) => {
  const handleClick = () => {
    if (!props.disabled && props.onClick) {
      props.onClick();
    }
  };

  const handleKeyDown = (e: KeyboardEvent) => {
    if ((e.key === "Enter" || e.key === " ") && !props.disabled) {
      e.preventDefault();
      props.onClick?.();
    }
  };

  return (
    <div
      onClick={handleClick}
      onKeyDown={handleKeyDown}
      tabIndex={props.disabled ? -1 : 0}
      role="button"
      aria-pressed={props.selected}
      aria-disabled={props.disabled}
      class={`
        relative flex flex-col items-center p-4 rounded-lg cursor-pointer transition-all
        min-w-[120px] max-w-[160px]
        border-2 ${props.selected ? "border-blue-500 bg-blue-900/30" : "border-gray-600 bg-gray-800"}
        ${props.disabled ? "opacity-50 cursor-not-allowed" : "hover:bg-gray-700 hover:border-gray-500"}
        focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2 focus:ring-offset-gray-900
      `}
    >
      {/* Keyboard shortcut indicator */}
      <Show when={props.shortcut}>
        <span class="absolute top-1 right-2 text-xs text-gray-500 font-mono">
          {props.shortcut}
        </span>
      </Show>

      {/* Main value */}
      <span class="text-3xl font-bold text-gray-100 mb-1">
        {props.value}
      </span>

      {/* Label */}
      <Show when={props.label}>
        <span class={`text-sm font-semibold uppercase tracking-wide ${props.labelColor ?? "text-gray-400"}`}>
          {props.label}
        </span>
      </Show>

      {/* Divider */}
      <Show when={props.hint}>
        <div class="w-full border-t border-gray-600 my-2" />
      </Show>

      {/* Hint text */}
      <Show when={props.hint}>
        <p class="text-xs text-gray-400 text-center italic leading-relaxed">
          "{props.hint}"
        </p>
      </Show>
    </div>
  );
};

export default HintCard;
