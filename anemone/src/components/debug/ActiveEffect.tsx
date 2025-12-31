import { type Component, Show, createSignal } from "solid-js";
import { useActiveEffect } from "../../stores";
import { Badge, eventTypeVariant } from "../shared";
import { JsonViewer } from "../shared";
import type { LlmCompleteEffect, SerializableEffect } from "../../types";

export const ActiveEffect: Component = () => {
  const effect = useActiveEffect();
  const [expanded, setExpanded] = createSignal(false);

  return (
    <div class="bg-bg-primary rounded-lg overflow-hidden">
      <Show
        when={effect()}
        fallback={
          <div class="p-3 text-sm text-text-dim italic">No active effect</div>
        }
      >
        {(eff) => (
          <>
            {/* Header */}
            <div class="flex items-center justify-between p-3 border-b border-bg-hover">
              <Badge variant={eventTypeVariant(eff().type)}>{eff().type}</Badge>
              <button
                type="button"
                class="text-xs text-phase-active hover:text-phase-active/80 transition-colors"
                onClick={() => setExpanded(!expanded())}
              >
                {expanded() ? "Collapse" : "Expand"}
              </button>
            </div>

            {/* Summary */}
            <div class="p-3">
              <EffectSummary effect={eff()} />
            </div>

            {/* Expanded details */}
            <Show when={expanded()}>
              <div class="p-3 border-t border-bg-hover bg-bg-secondary max-h-60 overflow-y-auto">
                <JsonViewer data={eff()} initialExpanded={true} />
              </div>
            </Show>
          </>
        )}
      </Show>
    </div>
  );
};

const EffectSummary: Component<{ effect: SerializableEffect }> = (props) => {
  return (
    <div class="space-y-1">
      <Show when={props.effect.type === "LlmComplete"}>
        <LlmSummary effect={props.effect as LlmCompleteEffect} />
      </Show>

      <Show when={props.effect.type === "LogInfo" || props.effect.type === "LogError"}>
        <div class="text-sm text-text-primary">
          {(props.effect as { eff_message: string }).eff_message}
        </div>
      </Show>
    </div>
  );
};

const LlmSummary: Component<{ effect: LlmCompleteEffect }> = (props) => (
  <>
    <div class="text-sm">
      <span class="text-text-muted">Node: </span>
      <span class="text-accent">{props.effect.eff_node}</span>
    </div>
    <div class="text-sm">
      <span class="text-text-muted">Schema: </span>
      <span class="text-text-primary">
        {props.effect.eff_schema ? "Yes" : "No"}
      </span>
    </div>
    <div class="text-xs text-text-dim mt-2 line-clamp-2">
      {props.effect.eff_system_prompt.slice(0, 100)}...
    </div>
  </>
);
