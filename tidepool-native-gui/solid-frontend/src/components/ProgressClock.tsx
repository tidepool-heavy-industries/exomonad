import { For, Show, type Component } from "solid-js";

interface Props {
  segments: number;       // Total segments (commonly 4, 6, or 8)
  filled: number;         // Filled segments
  label?: string;         // Label below clock
  fillColor?: string;     // SVG fill color (default: "#ef4444" - red)
  emptyColor?: string;    // SVG stroke color (default: "#374151" - gray-700)
  size?: number;          // Diameter in pixels (default: 48)
  urgent?: boolean;       // Show glow effect when nearly full
}

/**
 * ProgressClock renders an SVG pie/segment clock.
 * Each segment is a pie slice of the circle.
 */
const ProgressClock: Component<Props> = (props) => {
  const size = () => props.size ?? 48;
  const radius = () => size() / 2 - 2; // Leave room for stroke
  const center = () => size() / 2;
  const safeSegments = () => Math.max(1, props.segments); // Prevent division by zero

  // Calculate the SVG path for a pie segment
  const segmentPath = (index: number, total: number) => {
    const safeTotal = Math.max(1, total);
    const anglePerSegment = (2 * Math.PI) / safeTotal;
    const startAngle = index * anglePerSegment - Math.PI / 2; // Start from top
    const endAngle = startAngle + anglePerSegment;
    const gap = 0.03; // Small gap between segments

    const r = radius();
    const cx = center();
    const cy = center();

    const x1 = cx + r * Math.cos(startAngle + gap);
    const y1 = cy + r * Math.sin(startAngle + gap);
    const x2 = cx + r * Math.cos(endAngle - gap);
    const y2 = cy + r * Math.sin(endAngle - gap);

    const largeArc = anglePerSegment > Math.PI ? 1 : 0;

    return `M ${cx} ${cy} L ${x1} ${y1} A ${r} ${r} 0 ${largeArc} 1 ${x2} ${y2} Z`;
  };

  const isUrgent = () => props.urgent && props.filled >= safeSegments() - 1;
  const fillColor = () => props.fillColor ?? "#ef4444";

  return (
    <div class="flex flex-col items-center">
      <svg
        width={size()}
        height={size()}
        class={`transition-all ${isUrgent() ? "drop-shadow-lg" : ""}`}
        style={isUrgent() ? { filter: `drop-shadow(0 0 8px ${fillColor()})` } : {}}
      >
        {/* Background circle */}
        <circle
          cx={center()}
          cy={center()}
          r={radius()}
          fill="none"
          stroke={props.emptyColor ?? "#374151"}
          stroke-width="1"
        />

        {/* Segments */}
        <For each={Array.from({ length: safeSegments() }, (_, i) => i)}>
          {(index) => (
            <path
              d={segmentPath(index, safeSegments())}
              fill={index < props.filled ? fillColor() : "transparent"}
              stroke={props.emptyColor ?? "#374151"}
              stroke-width="1"
            />
          )}
        </For>
      </svg>

      {/* Label */}
      <Show when={props.label}>
        <span class="text-xs text-gray-400 mt-1 text-center max-w-[80px] truncate">
          {props.label}
        </span>
      </Show>

      {/* Urgent indicator */}
      <Show when={isUrgent()}>
        <span class="text-xs font-semibold mt-0.5" style={{ color: fillColor() }}>
          URGENT
        </span>
      </Show>
    </div>
  );
};

export default ProgressClock;
