import type { Component } from "solid-js";
import {
  ConnectionProvider,
  GraphProvider,
  ChatProvider,
  EventProvider,
} from "./stores";
import { AppLayout } from "./components/layout";
import { AppErrorBoundary } from "./components/shared";

const App: Component = () => {
  return (
    <AppErrorBoundary>
      <ConnectionProvider>
        <GraphProvider>
          <ChatProvider>
            <EventProvider>
              <AppLayout />
            </EventProvider>
          </ChatProvider>
        </GraphProvider>
      </ConnectionProvider>
    </AppErrorBoundary>
  );
};

export default App;
