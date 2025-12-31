import { createContext, useContext, type ParentComponent } from "solid-js";
import { createStore, produce } from "solid-js/store";
import type { TimelineEvent, EventType } from "../types";

interface EventStoreState {
  events: TimelineEvent[];
  maxEvents: number;
}

interface EventStoreActions {
  addEvent: (event: Omit<TimelineEvent, "id" | "timestamp">) => void;
  clear: () => void;
  setMaxEvents: (max: number) => void;
}

type EventStore = [EventStoreState, EventStoreActions];

const EventContext = createContext<EventStore>();

export const EventProvider: ParentComponent = (props) => {
  const [state, setState] = createStore<EventStoreState>({
    events: [],
    maxEvents: 500,
  });

  const actions: EventStoreActions = {
    addEvent(evt) {
      const event: TimelineEvent = {
        ...evt,
        id: crypto.randomUUID(),
        timestamp: new Date(),
      };

      setState(
        produce((s) => {
          s.events.push(event);
          // Trim to max - use splice to mutate in place
          if (s.events.length > s.maxEvents) {
            s.events.splice(0, s.events.length - s.maxEvents);
          }
        })
      );
    },

    clear() {
      setState({ events: [] });
    },

    setMaxEvents(max) {
      setState({ maxEvents: max });
    },
  };

  return (
    <EventContext.Provider value={[state, actions]}>
      {props.children}
    </EventContext.Provider>
  );
};

export function useEvents(): EventStore {
  const ctx = useContext(EventContext);
  if (!ctx) {
    throw new Error("useEvents must be used within EventProvider");
  }
  return ctx;
}

/** Helper to filter events by type */
export function filterEventsByType(
  events: TimelineEvent[],
  types: EventType[]
): TimelineEvent[] {
  if (types.length === 0) return events;
  return events.filter((e) => types.includes(e.type));
}

/** Helper to group events by time (minute) */
export function groupEventsByMinute(
  events: TimelineEvent[]
): Map<string, TimelineEvent[]> {
  const groups = new Map<string, TimelineEvent[]>();

  for (const event of events) {
    const key = formatMinute(event.timestamp);
    const existing = groups.get(key) ?? [];
    existing.push(event);
    groups.set(key, existing);
  }

  return groups;
}

function formatMinute(date: Date): string {
  return date.toLocaleTimeString([], {
    hour: "2-digit",
    minute: "2-digit",
  });
}
