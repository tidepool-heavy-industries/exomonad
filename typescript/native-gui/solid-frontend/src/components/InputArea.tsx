import { createSignal, Show, type Component } from "solid-js";
import type { TextInputConfig, PhotoUploadConfig, UserAction } from "../lib/types";
import { textAction, photoAction } from "../lib/types";

interface Props {
  textInput: TextInputConfig | null;
  photoUpload: PhotoUploadConfig | null;
  disabled: boolean;
  onSend: (action: UserAction) => void;
}

const InputArea: Component<Props> = (props) => {
  const [text, setText] = createSignal("");
  let fileInputRef: HTMLInputElement | undefined;

  const handleSubmit = (e: Event) => {
    e.preventDefault();
    const content = text().trim();
    if (content && !props.disabled) {
      props.onSend(textAction(content));
      setText("");
    }
  };

  const handleKeyDown = (e: KeyboardEvent) => {
    if (e.key === "Enter" && !e.shiftKey) {
      e.preventDefault();
      handleSubmit(e);
    }
  };

  const handlePhotoSelect = async (e: Event) => {
    const input = e.target as HTMLInputElement;
    const file = input.files?.[0];
    if (!file || props.disabled) return;

    const reader = new FileReader();
    reader.onload = () => {
      const result = reader.result as string;
      // Extract base64 data after "data:image/...;base64,"
      const base64 = result.split(",")[1];
      props.onSend(photoAction(base64, file.type));
    };
    reader.readAsDataURL(file);

    // Reset input so same file can be selected again
    input.value = "";
  };

  return (
    <div class="border-t border-gray-700 bg-gray-800 p-4">
      <Show when={props.photoUpload}>
        {(config) => (
          <div class="mb-3">
            <p class="text-sm text-gray-300 mb-2">{config().prompt}</p>
            <input
              ref={fileInputRef}
              type="file"
              accept="image/*"
              capture="environment"
              class="hidden"
              onChange={handlePhotoSelect}
              disabled={props.disabled}
            />
            <button
              type="button"
              onClick={() => fileInputRef?.click()}
              disabled={props.disabled}
              class="flex items-center gap-2 px-4 py-2 bg-gray-700 hover:bg-gray-600
                     disabled:opacity-50 disabled:cursor-not-allowed
                     text-gray-200 rounded-lg transition-colors"
            >
              <svg
                class="w-5 h-5"
                fill="none"
                stroke="currentColor"
                viewBox="0 0 24 24"
              >
                <path
                  stroke-linecap="round"
                  stroke-linejoin="round"
                  stroke-width="2"
                  d="M3 9a2 2 0 012-2h.93a2 2 0 001.664-.89l.812-1.22A2 2 0 0110.07 4h3.86a2 2 0 011.664.89l.812 1.22A2 2 0 0018.07 7H19a2 2 0 012 2v9a2 2 0 01-2 2H5a2 2 0 01-2-2V9z"
                />
                <path
                  stroke-linecap="round"
                  stroke-linejoin="round"
                  stroke-width="2"
                  d="M15 13a3 3 0 11-6 0 3 3 0 016 0z"
                />
              </svg>
              Take Photo
            </button>
          </div>
        )}
      </Show>

      <Show when={props.textInput}>
        {(config) => (
          <form onSubmit={handleSubmit} class="flex gap-2">
            <input
              type="text"
              value={text()}
              onInput={(e) => setText(e.currentTarget.value)}
              onKeyDown={handleKeyDown}
              placeholder={config().placeholder}
              disabled={props.disabled}
              class="flex-1 bg-gray-700 text-gray-100 rounded-lg px-4 py-2
                     placeholder-gray-400 border border-gray-600
                     focus:outline-none focus:border-blue-500 focus:ring-1 focus:ring-blue-500
                     disabled:opacity-50 disabled:cursor-not-allowed"
            />
            <button
              type="submit"
              disabled={props.disabled || !text().trim()}
              class="px-4 py-2 bg-blue-600 hover:bg-blue-500
                     disabled:opacity-50 disabled:cursor-not-allowed
                     text-white rounded-lg transition-colors"
            >
              <svg class="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path
                  stroke-linecap="round"
                  stroke-linejoin="round"
                  stroke-width="2"
                  d="M12 19l9 2-9-18-9 18 9-2zm0 0v-8"
                />
              </svg>
            </button>
          </form>
        )}
      </Show>
    </div>
  );
};

export default InputArea;
