/**
 * Alert types for Grafana webhook integration.
 *
 * Grafana Alertmanager sends alerts via webhook with a specific payload format.
 * This module defines types and helpers for parsing and transforming alerts
 * into graph inputs.
 */

/**
 * Grafana webhook payload containing one or more alerts.
 */
export interface GrafanaWebhookPayload {
  version: string;
  groupKey: string;
  truncatedAlerts: number;
  status: "firing" | "resolved";
  receiver: string;
  groupLabels: Record<string, string>;
  commonLabels: Record<string, string>;
  commonAnnotations: Record<string, string>;
  externalURL: string;
  alerts: GrafanaAlert[];
}

/**
 * Individual alert from Grafana.
 */
export interface GrafanaAlert {
  status: "firing" | "resolved";
  labels: Record<string, string>;
  annotations: Record<string, string>;
  startsAt: string;
  endsAt: string;
  generatorURL: string;
  fingerprint: string;
  silenceURL?: string;
  dashboardURL?: string;
  panelURL?: string;
  values?: Record<string, number>;
}

/**
 * Input schema for alert graph execution.
 * Transformed from GrafanaAlert with chat_id extracted.
 */
export interface AlertInput {
  alertName: string;
  severity: string;
  status: "firing" | "resolved";
  fingerprint: string;
  chatId: number;
  summary: string;
  description: string;
  labels: Record<string, string>;
  annotations: Record<string, string>;
  startsAt: string;
  dashboardUrl?: string;
  panelUrl?: string;
  values?: Record<string, number>;
}

/**
 * Extract chat_id from alert labels or annotations.
 * Returns null if chat_id not found or invalid.
 */
export function extractChatId(alert: GrafanaAlert): number | null {
  const chatIdStr = alert.labels.chat_id ?? alert.annotations.chat_id;
  if (!chatIdStr) return null;
  const chatId = parseInt(chatIdStr, 10);
  return isNaN(chatId) ? null : chatId;
}

/**
 * Convert Grafana alert to AlertInput for graph execution.
 */
export function toAlertInput(alert: GrafanaAlert, chatId: number): AlertInput {
  return {
    alertName: alert.labels.alertname ?? "unknown",
    severity: alert.labels.severity ?? "info",
    status: alert.status,
    fingerprint: alert.fingerprint,
    chatId,
    summary: alert.annotations.summary ?? "",
    description: alert.annotations.description ?? "",
    labels: alert.labels,
    annotations: alert.annotations,
    startsAt: alert.startsAt,
    dashboardUrl: alert.dashboardURL,
    panelUrl: alert.panelURL,
    values: alert.values,
  };
}
