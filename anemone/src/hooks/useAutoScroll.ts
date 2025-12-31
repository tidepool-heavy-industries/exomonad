import { createSignal, type Accessor } from "solid-js";

interface AutoScrollOptions {
  /** Threshold in pixels from bottom to consider "at bottom" */
  threshold?: number;
}

export function useAutoScroll(
  containerRef: Accessor<HTMLElement | undefined>,
  options: AutoScrollOptions = {}
) {
  const { threshold = 100 } = options;
  const [isAtBottom, setIsAtBottom] = createSignal(true);

  const checkScroll = () => {
    const el = containerRef();
    if (!el) return;

    const atBottom =
      el.scrollHeight - el.scrollTop - el.clientHeight < threshold;
    setIsAtBottom(atBottom);
  };

  const scrollToBottom = (force = false) => {
    const el = containerRef();
    if (el && (force || isAtBottom())) {
      el.scrollTo({
        top: el.scrollHeight,
        behavior: "smooth",
      });
    }
  };

  return { scrollToBottom, checkScroll, isAtBottom };
}
