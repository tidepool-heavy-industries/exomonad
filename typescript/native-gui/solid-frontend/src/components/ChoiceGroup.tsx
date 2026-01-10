import { createSignal, For, Show, type Component } from "solid-js";
import type { ChoiceConfig, ChoiceOption, UserAction } from "../lib/types";
import { choiceAction, multiChoiceAction } from "../lib/types";

interface Props {
  config: ChoiceConfig;
  disabled: boolean;
  onSend: (action: UserAction) => void;
}

/**
 * ChoiceGroup renders a set of choice options with support for:
 * - Single selection (radio button style)
 * - Multiple selection (checkbox style)
 * - Rich metadata (descriptions, cost tags, disabled states)
 */
const ChoiceGroup: Component<Props> = (props) => {
  const [selected, setSelected] = createSignal<Set<number>>(new Set());

  const handleSelect = (option: ChoiceOption) => {
    // Skip if option is disabled
    if (option.disabled || props.disabled) return;

    if (props.config.multiSelect) {
      // Toggle selection for multi-select
      const newSelected = new Set(selected());
      if (newSelected.has(option.index)) {
        newSelected.delete(option.index);
      } else {
        newSelected.add(option.index);
      }
      setSelected(newSelected);
    } else {
      // Single select - emit immediately
      props.onSend(choiceAction(option.index));
    }
  };

  const handleSubmit = () => {
    // For multi-select, submit all selected indices
    if (props.config.multiSelect && !props.disabled) {
      props.onSend(multiChoiceAction([...selected()]));
    }
  };

  const handleKeyDown = (e: KeyboardEvent, option: ChoiceOption) => {
    if (e.key === "Enter" || e.key === " ") {
      e.preventDefault();
      handleSelect(option);
    }
  };

  return (
    <div class="border-t border-gray-700 bg-gray-800 p-4">
      <p class="text-sm text-gray-300 mb-3">{props.config.prompt}</p>

      <div class="flex flex-col gap-2" role="group" aria-label={props.config.prompt}>
        <For each={props.config.options}>
          {(option) => (
            <ChoiceCard
              option={option}
              selected={selected().has(option.index)}
              multiSelect={props.config.multiSelect}
              disabled={props.disabled || !!option.disabled}
              disabledReason={option.disabled}
              onClick={() => handleSelect(option)}
              onKeyDown={(e) => handleKeyDown(e, option)}
            />
          )}
        </For>
      </div>

      <Show when={props.config.multiSelect}>
        <button
          type="button"
          onClick={handleSubmit}
          disabled={props.disabled}
          class="mt-3 w-full px-4 py-2 bg-blue-600 hover:bg-blue-500
                 disabled:opacity-50 disabled:cursor-not-allowed
                 text-white rounded-lg transition-colors
                 focus:outline-none focus:ring-2 focus:ring-blue-500"
        >
          Confirm Selection ({selected().size} selected)
        </button>
      </Show>
    </div>
  );
};

interface ChoiceCardProps {
  option: ChoiceOption;
  selected: boolean;
  multiSelect: boolean;
  disabled: boolean;
  disabledReason?: string;
  onClick: () => void;
  onKeyDown: (e: KeyboardEvent) => void;
}

/**
 * ChoiceCard renders a single choice option with:
 * - Label and optional description
 * - Cost tags
 * - Disabled state with tooltip
 * - Selection indicator (checkbox/radio)
 */
const ChoiceCard: Component<ChoiceCardProps> = (props) => {
  return (
    <div
      onClick={props.onClick}
      onKeyDown={props.onKeyDown}
      tabIndex={props.disabled ? -1 : 0}
      role={props.multiSelect ? "checkbox" : "radio"}
      aria-checked={props.selected}
      aria-disabled={props.disabled}
      title={props.disabledReason}
      class={`
        flex items-start gap-3 p-3 rounded-lg cursor-pointer transition-all
        border ${props.selected ? "border-blue-500 bg-blue-900/30" : "border-gray-600 bg-gray-700"}
        ${props.disabled ? "opacity-50 cursor-not-allowed" : "hover:bg-gray-600 hover:border-gray-500"}
        focus:outline-none focus:ring-2 focus:ring-blue-500
      `}
    >
      {/* Selection indicator */}
      <div class="flex-shrink-0 mt-0.5">
        <Show when={props.multiSelect} fallback={
          /* Radio button */
          <div class={`w-4 h-4 rounded-full border-2 ${
            props.selected ? "border-blue-500 bg-blue-500" : "border-gray-400"
          }`}>
            <Show when={props.selected}>
              <div class="w-full h-full rounded-full flex items-center justify-center">
                <div class="w-1.5 h-1.5 rounded-full bg-white" />
              </div>
            </Show>
          </div>
        }>
          {/* Checkbox */}
          <div class={`w-4 h-4 rounded border-2 ${
            props.selected ? "border-blue-500 bg-blue-500" : "border-gray-400"
          }`}>
            <Show when={props.selected}>
              <svg class="w-full h-full text-white" viewBox="0 0 16 16">
                <path
                  fill="currentColor"
                  d="M13.78 4.22a.75.75 0 010 1.06l-7.25 7.25a.75.75 0 01-1.06 0L2.22 9.28a.75.75 0 111.06-1.06L6 10.94l6.72-6.72a.75.75 0 011.06 0z"
                />
              </svg>
            </Show>
          </div>
        </Show>
      </div>

      {/* Content */}
      <div class="flex-1 min-w-0">
        <span class="text-gray-100 font-medium">{props.option.label}</span>

        <Show when={props.option.description}>
          <p class="text-sm text-gray-400 mt-1">{props.option.description}</p>
        </Show>

        <Show when={props.option.costs.length > 0}>
          <div class="flex flex-wrap gap-1 mt-2">
            <For each={props.option.costs}>
              {(cost) => (
                <span class="px-2 py-0.5 text-xs rounded-full bg-amber-900/50 text-amber-300 border border-amber-700">
                  {cost}
                </span>
              )}
            </For>
          </div>
        </Show>

        <Show when={props.disabledReason}>
          <p class="text-xs text-red-400 mt-1">{props.disabledReason}</p>
        </Show>
      </div>
    </div>
  );
};

export default ChoiceGroup;
