import { For, type Component } from "solid-js";
import type { ButtonConfig, UserAction } from "../lib/types";
import { buttonAction } from "../lib/types";

interface Props {
  buttons: ButtonConfig[];
  disabled: boolean;
  onSend: (action: UserAction) => void;
}

const ButtonGroup: Component<Props> = (props) => {
  const handleClick = (id: string) => {
    if (!props.disabled) {
      props.onSend(buttonAction(id));
    }
  };

  return (
    <div class="border-t border-gray-700 bg-gray-800 p-4">
      <div class="flex flex-wrap gap-2 justify-center" role="group">
        <For each={props.buttons}>
          {(button) => (
            <button
              type="button"
              onClick={() => handleClick(button.id)}
              disabled={props.disabled}
              class="px-4 py-2 bg-gray-700 hover:bg-gray-600
                     disabled:opacity-50 disabled:cursor-not-allowed
                     text-gray-100 rounded-lg transition-colors
                     border border-gray-600 hover:border-gray-500
                     focus:outline-none focus:ring-2 focus:ring-blue-500"
            >
              {button.label}
            </button>
          )}
        </For>
      </div>
    </div>
  );
};

export default ButtonGroup;
