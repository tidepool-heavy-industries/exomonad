import { type Component, type JSX, ErrorBoundary as SolidErrorBoundary } from "solid-js";

interface ErrorFallbackProps {
  error: Error;
  reset: () => void;
}

const ErrorFallback: Component<ErrorFallbackProps> = (props) => {
  return (
    <div class="flex flex-col items-center justify-center h-full p-8 bg-bg-primary text-text-primary">
      <div class="max-w-md text-center space-y-4">
        {/* Error icon */}
        <div class="w-16 h-16 mx-auto rounded-full bg-phase-failed/20 flex items-center justify-center">
          <svg
            class="w-8 h-8 text-phase-failed"
            fill="none"
            stroke="currentColor"
            viewBox="0 0 24 24"
          >
            <path
              stroke-linecap="round"
              stroke-linejoin="round"
              stroke-width="2"
              d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"
            />
          </svg>
        </div>

        <h1 class="text-xl font-semibold text-text-primary">
          Something went wrong
        </h1>

        <p class="text-sm text-text-muted">
          An unexpected error occurred. The details are shown below.
        </p>

        {/* Error details */}
        <div class="p-3 rounded-lg bg-bg-secondary border border-bg-hover text-left">
          <p class="text-xs font-mono text-phase-failed break-all">
            {props.error.message}
          </p>
        </div>

        {/* Actions */}
        <div class="flex gap-3 justify-center">
          <button
            type="button"
            onClick={props.reset}
            class="px-4 py-2 bg-phase-active text-white rounded-lg font-medium hover:bg-phase-active/90 transition-colors"
          >
            Try Again
          </button>
          <button
            type="button"
            onClick={() => window.location.reload()}
            class="px-4 py-2 bg-bg-hover text-text-primary rounded-lg font-medium hover:bg-bg-active transition-colors"
          >
            Reload Page
          </button>
        </div>
      </div>
    </div>
  );
};

interface AppErrorBoundaryProps {
  children: JSX.Element;
}

export const AppErrorBoundary: Component<AppErrorBoundaryProps> = (props) => {
  return (
    <SolidErrorBoundary
      fallback={(error, reset) => <ErrorFallback error={error} reset={reset} />}
    >
      {props.children}
    </SolidErrorBoundary>
  );
};
