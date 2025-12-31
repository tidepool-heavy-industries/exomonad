import { createSignal, createMemo } from "solid-js";
import { useEvents } from "../stores/eventStore";
import type { FilterState, EventType } from "../types";

const defaultFilter: FilterState = {
  types: [],
  nodeFilter: null,
  searchQuery: "",
};

export function useEventFilter() {
  const [events] = useEvents();
  const [filter, setFilter] = createSignal<FilterState>(defaultFilter);

  const filteredEvents = createMemo(() => {
    const f = filter();
    let result = events.events;

    // Type filter
    if (f.types.length > 0) {
      result = result.filter((event) => f.types.includes(event.type));
    }

    // Node filter
    if (f.nodeFilter) {
      result = result.filter((event) => event.node === f.nodeFilter);
    }

    // Search filter
    if (f.searchQuery) {
      const query = f.searchQuery.toLowerCase();
      result = result.filter((event) => {
        const matchesSummary = event.summary.toLowerCase().includes(query);
        const matchesNode = event.node?.toLowerCase().includes(query);
        return matchesSummary || matchesNode;
      });
    }

    return result;
  });

  const toggleType = (type: EventType) => {
    setFilter((f) => {
      const types = f.types.includes(type)
        ? f.types.filter((t) => t !== type)
        : [...f.types, type];
      return { ...f, types };
    });
  };

  const setNodeFilter = (node: string | null) => {
    setFilter((f) => ({ ...f, nodeFilter: node }));
  };

  const setSearchQuery = (query: string) => {
    setFilter((f) => ({ ...f, searchQuery: query }));
  };

  const clearFilter = () => {
    setFilter(defaultFilter);
  };

  return {
    filter,
    filteredEvents,
    toggleType,
    setNodeFilter,
    setSearchQuery,
    clearFilter,
  };
}
