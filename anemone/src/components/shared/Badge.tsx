import type { Component, JSX } from "solid-js";

export type BadgeVariant =
  | "default"
  | "blue"
  | "green"
  | "yellow"
  | "red"
  | "gray"
  | "accent";

interface BadgeProps {
  variant?: BadgeVariant;
  children: JSX.Element;
  class?: string;
}

const variantClasses: Record<BadgeVariant, string> = {
  default: "bg-bg-hover text-text-primary",
  blue: "bg-phase-active/20 text-phase-active",
  green: "bg-phase-completed/20 text-phase-completed",
  yellow: "bg-phase-transitioning/20 text-phase-transitioning",
  red: "bg-phase-failed/20 text-phase-failed",
  gray: "bg-bg-hover text-text-muted",
  accent: "bg-accent/20 text-accent",
};

export const Badge: Component<BadgeProps> = (props) => {
  return (
    <span
      class={`inline-flex items-center px-2 py-0.5 text-xs font-medium rounded-full ${
        variantClasses[props.variant ?? "default"]
      } ${props.class ?? ""}`}
    >
      {props.children}
    </span>
  );
};

/** Get badge variant for event type */
export function eventTypeVariant(
  type: string
): BadgeVariant {
  switch (type) {
    case "progress":
    case "llm_start":
    case "http_start":
      return "blue";
    case "done":
    case "llm_complete":
    case "http_complete":
    case "connected":
      return "green";
    case "suspend":
    case "transitioning":
      return "yellow";
    case "error":
    case "log_error":
    case "disconnected":
      return "red";
    case "log_info":
    default:
      return "gray";
  }
}

/** Get badge variant for phase type */
export function phaseVariant(
  type: string
): BadgeVariant {
  switch (type) {
    case "idle":
      return "gray";
    case "in_node":
      return "blue";
    case "transitioning":
      return "yellow";
    case "completed":
      return "green";
    case "failed":
      return "red";
    default:
      return "default";
  }
}
