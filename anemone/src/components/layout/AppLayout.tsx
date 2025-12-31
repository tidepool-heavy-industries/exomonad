import { type Component, createSignal, Show } from "solid-js";
import { DebugPane } from "./DebugPane";
import { ChatPane } from "./ChatPane";
import { HistoryPane } from "./HistoryPane";
import { MobileHeader } from "./MobileHeader";

export type MobilePane = "chat" | "debug" | "history";

export const AppLayout: Component = () => {
  const [mobilePane, setMobilePane] = createSignal<MobilePane>("chat");
  const [showMobileOverlay, setShowMobileOverlay] = createSignal(false);

  const openPane = (pane: MobilePane) => {
    setMobilePane(pane);
    if (pane !== "chat") {
      setShowMobileOverlay(true);
    }
  };

  const closeOverlay = () => {
    setShowMobileOverlay(false);
    setMobilePane("chat");
  };

  return (
    <div class="flex flex-col h-full w-full overflow-hidden">
      {/* Mobile header - hidden on desktop */}
      <MobileHeader
        activePane={mobilePane()}
        onSelectPane={openPane}
      />

      {/* Desktop layout - hidden on mobile */}
      <div class="hidden md:flex flex-1 overflow-hidden">
        <DebugPane />
        <ChatPane />
        <HistoryPane />
      </div>

      {/* Mobile layout - hidden on desktop */}
      <div class="flex md:hidden flex-1 overflow-hidden relative">
        {/* Chat is always the base layer on mobile */}
        <div class="flex-1">
          <ChatPane />
        </div>

        {/* Mobile overlay for Debug/History panes */}
        <Show when={showMobileOverlay()}>
          {/* Backdrop */}
          <div
            class="absolute inset-0 bg-black/50 z-40"
            onClick={closeOverlay}
          />

          {/* Drawer */}
          <div class="absolute inset-y-0 left-0 right-0 z-50 flex">
            <Show when={mobilePane() === "debug"}>
              <div class="w-full max-w-sm bg-bg-secondary animate-slide-in-left">
                <DebugPane onClose={closeOverlay} />
              </div>
            </Show>
            <Show when={mobilePane() === "history"}>
              <div class="w-full max-w-sm ml-auto bg-bg-secondary animate-slide-in-right">
                <HistoryPane onClose={closeOverlay} />
              </div>
            </Show>
          </div>
        </Show>
      </div>
    </div>
  );
};
