// Sessions list page

import type { SessionInfo, SessionState } from "./types";
import { HubWebSocket } from "./websocket";
import "./style.css";

type FilterState = "all" | SessionState;

class SessionsList {
  private sessions: SessionInfo[] = [];
  private filter: FilterState = "all";
  private tbody: HTMLTableSectionElement;
  private statusEl: HTMLElement;
  private ws: HubWebSocket;

  constructor() {
    this.tbody = document.getElementById("sessions-tbody") as HTMLTableSectionElement;
    this.statusEl = document.getElementById("status") as HTMLElement;

    this.ws = new HubWebSocket({
      onConnect: () => this.setConnectionStatus(true),
      onDisconnect: () => this.setConnectionStatus(false),
      onInit: (sessions) => {
        this.sessions = sessions;
        this.render();
      },
      onSessionCreated: (session) => {
        this.sessions.unshift(session);
        this.render();
      },
      onSessionUpdated: (session) => {
        this.updateSession(session);
      },
      onNodeCompleted: () => {
        // Refresh to get updated session state
        this.fetchSessions();
      },
      onNodeFailed: () => {
        this.fetchSessions();
      },
    });

    this.setupFilters();
    this.ws.connect();
  }

  private setConnectionStatus(connected: boolean): void {
    if (connected) {
      this.statusEl.textContent = "Connected";
      this.statusEl.className = "status connected";
    } else {
      this.statusEl.textContent = "Disconnected - Reconnecting...";
      this.statusEl.className = "status disconnected";
    }
  }

  private setupFilters(): void {
    document.querySelectorAll(".filter-btn").forEach((btn) => {
      btn.addEventListener("click", () => {
        document.querySelectorAll(".filter-btn").forEach((b) => b.classList.remove("active"));
        btn.classList.add("active");
        this.filter = (btn as HTMLButtonElement).dataset.filter as FilterState;
        this.render();
      });
    });
  }

  private updateSession(session: SessionInfo): void {
    const idx = this.sessions.findIndex((s) => s.id === session.id);
    if (idx >= 0) {
      this.sessions[idx] = session;
    } else {
      this.sessions.unshift(session);
    }
    this.render();
  }

  private async fetchSessions(): Promise<void> {
    try {
      const resp = await fetch("/api/sessions");
      if (resp.ok) {
        this.sessions = await resp.json();
        this.render();
      }
    } catch (e) {
      console.error("Failed to fetch sessions:", e);
    }
  }

  private render(): void {
    const filtered =
      this.filter === "all"
        ? this.sessions
        : this.sessions.filter((s) => s.state === this.filter);

    if (filtered.length === 0) {
      this.tbody.innerHTML = `
        <tr class="placeholder-row">
          <td colspan="4">No sessions found</td>
        </tr>
      `;
      return;
    }

    this.tbody.innerHTML = filtered
      .map(
        (session) => `
      <tr class="session-row" data-id="${session.id}">
        <td class="name-cell">
          <a href="/sessions/${session.id}">${this.escapeHtml(session.name)}</a>
        </td>
        <td class="state-cell">
          <span class="state ${session.state}">${session.state}</span>
        </td>
        <td class="nodes-cell">${session.node_count}</td>
        <td class="created-cell">${this.formatDate(session.created_at)}</td>
      </tr>
    `
      )
      .join("");

    // Add click handlers for rows
    this.tbody.querySelectorAll(".session-row").forEach((row) => {
      row.addEventListener("click", (e) => {
        const target = e.target as HTMLElement;
        if (target.tagName !== "A") {
          window.location.href = `/sessions/${(row as HTMLElement).dataset.id}`;
        }
      });
    });
  }

  private formatDate(isoString: string): string {
    const date = new Date(isoString);
    const now = new Date();
    const diff = now.getTime() - date.getTime();

    // If less than 24 hours, show relative time
    if (diff < 86400000) {
      if (diff < 60000) return "Just now";
      if (diff < 3600000) return `${Math.floor(diff / 60000)}m ago`;
      return `${Math.floor(diff / 3600000)}h ago`;
    }

    // Otherwise show date
    return date.toLocaleDateString();
  }

  private escapeHtml(text: string): string {
    const div = document.createElement("div");
    div.textContent = text;
    return div.innerHTML;
  }
}

// Initialize on page load
document.addEventListener("DOMContentLoaded", () => {
  new SessionsList();
});
