import type { Component } from "solid-js";

interface SpinnerProps {
  size?: "sm" | "md" | "lg";
  class?: string;
}

const sizeClasses = {
  sm: "w-4 h-4",
  md: "w-6 h-6",
  lg: "w-8 h-8",
};

export const Spinner: Component<SpinnerProps> = (props) => {
  return (
    <svg
      class={`animate-spin ${sizeClasses[props.size ?? "md"]} ${props.class ?? ""}`}
      xmlns="http://www.w3.org/2000/svg"
      fill="none"
      viewBox="0 0 24 24"
    >
      <circle
        class="opacity-25"
        cx="12"
        cy="12"
        r="10"
        stroke="currentColor"
        stroke-width="4"
      />
      <path
        class="opacity-75"
        fill="currentColor"
        d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
      />
    </svg>
  );
};

/** Typing indicator with bouncing dots */
export const TypingIndicator: Component = () => {
  return (
    <div class="flex items-center gap-1 px-4 py-2">
      <div class="w-2 h-2 rounded-full bg-text-muted animate-typing-1" />
      <div class="w-2 h-2 rounded-full bg-text-muted animate-typing-2" />
      <div class="w-2 h-2 rounded-full bg-text-muted animate-typing-3" />
    </div>
  );
};
