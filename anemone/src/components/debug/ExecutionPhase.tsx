import { type Component, Match, Switch } from "solid-js";
import { usePhase } from "../../stores";

export const ExecutionPhase: Component = () => {
  const phase = usePhase();

  const phaseColor = () => {
    switch (phase().type) {
      case "idle":
        return "bg-phase-idle";
      case "in_node":
        return "bg-phase-active animate-pulse";
      case "transitioning":
        return "bg-phase-transitioning";
      case "completed":
        return "bg-phase-completed";
      case "failed":
        return "bg-phase-failed";
      default:
        return "bg-bg-hover";
    }
  };

  return (
    <div class="flex items-center gap-3 p-3 bg-bg-primary rounded-lg">
      {/* Status dot */}
      <div class={`w-3 h-3 rounded-full flex-shrink-0 ${phaseColor()}`} />

      {/* Phase info */}
      <div class="flex-1 min-w-0">
        <Switch fallback={<PhaseLabel label="Unknown" />}>
          <Match when={phase().type === "idle"}>
            <PhaseLabel label="Idle" sublabel="Waiting for input" />
          </Match>
          <Match when={phase().type === "in_node"}>
            <PhaseLabel
              label="Executing"
              sublabel={(phase() as { nodeName: string }).nodeName}
            />
          </Match>
          <Match when={phase().type === "transitioning"}>
            <PhaseLabel
              label="Transitioning"
              sublabel={`${(phase() as { fromNode: string }).fromNode} → ${(phase() as { toNode: string }).toNode}`}
            />
          </Match>
          <Match when={phase().type === "completed"}>
            <PhaseLabel label="Completed" />
          </Match>
          <Match when={phase().type === "failed"}>
            <PhaseLabel
              label="Failed"
              sublabel={(phase() as { error: string }).error}
              isError
            />
          </Match>
        </Switch>
      </div>
    </div>
  );
};

const PhaseLabel: Component<{
  label: string;
  sublabel?: string;
  isError?: boolean;
}> = (props) => {
  return (
    <div>
      <div class="text-sm font-medium text-text-primary">{props.label}</div>
      {props.sublabel && (
        <div
          class={`text-xs truncate ${props.isError ? "text-phase-failed" : "text-text-muted"}`}
        >
          {props.sublabel}
        </div>
      )}
    </div>
  );
};
