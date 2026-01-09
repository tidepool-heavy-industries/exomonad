import { For, onCleanup, onMount, Show, type Component } from "solid-js";
import HintCard from "./HintCard";

interface CardData {
  value: string | number;
  label?: string;
  labelColor?: string;
  hint?: string;
  disabled?: boolean;
}

interface Props {
  cards: CardData[];
  onSelect: (index: number) => void;
  disabled?: boolean;
  header?: string;
  subheader?: string;
  enableNumberKeys?: boolean;  // Enable 1-9 keyboard shortcuts (default: true)
}

/**
 * HintCardGroup renders a horizontal group of HintCards with:
 * - Optional header and subheader
 * - Keyboard navigation (1-9 keys select cards)
 * - Proper accessibility attributes
 */
const HintCardGroup: Component<Props> = (props) => {
  const handleKeyDown = (e: KeyboardEvent) => {
    // Check reactive props inside handler (Solid proxies are live)
    if (props.disabled) return;
    if (props.enableNumberKeys === false) return;

    // Check for number keys 1-9
    const num = parseInt(e.key, 10);
    if (num >= 1 && num <= 9) {
      const index = num - 1;
      if (index < props.cards.length && !props.cards[index]?.disabled) {
        e.preventDefault();
        props.onSelect(index);
      }
    }
  };

  onMount(() => {
    document.addEventListener("keydown", handleKeyDown);
  });

  onCleanup(() => {
    document.removeEventListener("keydown", handleKeyDown);
  });

  return (
    <div class="bg-gray-800 border-t border-gray-700 p-4">
      {/* Header */}
      <Show when={props.header}>
        <h3 class="text-sm font-medium text-gray-300 mb-1">{props.header}</h3>
      </Show>

      {/* Subheader */}
      <Show when={props.subheader}>
        <p class="text-xs text-gray-500 mb-3">{props.subheader}</p>
      </Show>

      {/* Cards */}
      <div
        class="flex flex-wrap gap-3 justify-center"
        role="group"
        aria-label={props.header ?? "Select an option"}
      >
        <For each={props.cards}>
          {(card, index) => (
            <HintCard
              value={card.value}
              label={card.label}
              labelColor={card.labelColor}
              hint={card.hint}
              disabled={props.disabled || card.disabled}
              onClick={() => props.onSelect(index())}
              shortcut={props.enableNumberKeys !== false && index() < 9 ? String(index() + 1) : undefined}
            />
          )}
        </For>
      </div>

      {/* Keyboard hint */}
      <Show when={props.enableNumberKeys !== false && props.cards.length > 0}>
        <p class="text-xs text-gray-600 text-center mt-3">
          Press 1-{Math.min(props.cards.length, 9)} to select
        </p>
      </Show>
    </div>
  );
};

export default HintCardGroup;
