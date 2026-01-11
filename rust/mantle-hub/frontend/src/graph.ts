// Session graph visualization page

import * as d3 from "d3";
import type {
  GraphData,
  GraphNode,
  NodeInfo,
  NodeResult,
  NodeEvent,
  StreamEvent,
  NodeState,
} from "./types";
import { HubWebSocket } from "./websocket";
import { VisualizationController } from "./visualization";
import type { NodePosition } from "./visualization";
import "./style.css";
import "./styles/chat-window.css";

const stateColors: Record<NodeState, string> = {
  pending: "#facc15",
  running: "#3b82f6",
  completed: "#22c55e",
  failed: "#ef4444",
  cancelled: "#6b7280",
};

type TabName = "overview" | "events" | "spec" | "children";

interface SimNode extends GraphNode, d3.SimulationNodeDatum {
  x?: number;
  y?: number;
  fx?: number | null;
  fy?: number | null;
}

interface SimLink extends d3.SimulationLinkDatum<SimNode> {
  source: SimNode | string;
  target: SimNode | string;
}

class SessionGraph {
  private sessionId: string;
  private svg!: d3.Selection<SVGSVGElement, unknown, HTMLElement, unknown>;
  private sidebar!: HTMLElement;
  private sidebarContent!: HTMLElement;
  private statusEl!: HTMLElement;
  private sessionNameEl!: HTMLElement;
  private tabsEl!: HTMLElement;

  private nodes: SimNode[] = [];
  private edges: SimLink[] = [];
  private selectedNode: SimNode | null = null;
  private currentTab: TabName = "overview";
  private nodeEvents: Record<string, NodeEvent[]> = {};

  private simulation!: d3.Simulation<SimNode, SimLink>;
  private linksGroup!: d3.Selection<SVGGElement, unknown, HTMLElement, unknown>;
  private nodesGroup!: d3.Selection<SVGGElement, unknown, HTMLElement, unknown>;
  private linkElements!: d3.Selection<SVGLineElement, SimLink, SVGGElement, unknown>;
  private nodeElements!: d3.Selection<SVGGElement, SimNode, SVGGElement, unknown>;

  private width = 800;
  private height = 600;

  private ws!: HubWebSocket;
  private vizController!: VisualizationController;

  constructor() {
    // Extract session ID from URL: /sessions/{sessionId}
    const pathParts = location.pathname.split("/").filter((p) => p);
    this.sessionId = pathParts[pathParts.length - 1];

    if (!this.sessionId) {
      console.error("No session ID in URL");
      return;
    }

    this.svg = d3.select<SVGSVGElement, unknown>("#graph");
    this.sidebar = document.getElementById("sidebar") as HTMLElement;
    this.sidebarContent = document.getElementById("sidebar-content") as HTMLElement;
    this.statusEl = document.getElementById("status") as HTMLElement;
    this.sessionNameEl = document.getElementById("session-name") as HTMLElement;
    this.tabsEl = document.getElementById("sidebar-tabs") as HTMLElement;

    this.ws = new HubWebSocket({
      onConnect: () => this.setConnectionStatus(true),
      onDisconnect: () => this.setConnectionStatus(false),
      onNodeCreated: (node) => {
        if (node.session_id === this.sessionId) {
          this.addNode(node);
        }
      },
      onNodeCompleted: (nodeId, result) => {
        this.updateNodeResult(nodeId, result);
      },
      onNodeFailed: (nodeId) => {
        this.updateNodeState(nodeId, "failed");
      },
      onNodeEvent: (sessionId, nodeId, event, timestamp) => {
        if (sessionId === this.sessionId) {
          this.cacheEvent(nodeId, event, timestamp);
        }
      },
      onSessionUpdated: (session) => {
        if (session.id === this.sessionId) {
          this.sessionNameEl.textContent = session.name;
        }
      },
    });

    this.setupSVG();
    this.setupEventHandlers();
    this.setupVisualization();
    this.loadGraph();
    this.ws.connect();

    window.addEventListener("hashchange", () => this.handleHash());
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

  private setupSVG(): void {
    const container = document.getElementById("graph-container") as HTMLElement;
    this.width = container.clientWidth;
    this.height = container.clientHeight;

    this.svg.attr("width", this.width).attr("height", this.height);

    // Arrow marker for links
    this.svg
      .append("defs")
      .append("marker")
      .attr("id", "arrow")
      .attr("viewBox", "0 -5 10 10")
      .attr("refX", 25)
      .attr("refY", 0)
      .attr("markerWidth", 6)
      .attr("markerHeight", 6)
      .attr("orient", "auto")
      .append("path")
      .attr("d", "M0,-5L10,0L0,5")
      .attr("class", "link-arrow");

    // Groups for links and nodes
    this.linksGroup = this.svg.append("g").attr("class", "links");
    this.nodesGroup = this.svg.append("g").attr("class", "nodes");

    // Force simulation
    this.simulation = d3
      .forceSimulation<SimNode>()
      .force(
        "link",
        d3.forceLink<SimNode, SimLink>().id((d) => d.id).distance(100)
      )
      .force("charge", d3.forceManyBody().strength(-300))
      .force("center", d3.forceCenter(this.width / 2, this.height / 2))
      .force("collision", d3.forceCollide().radius(40));

    // Handle window resize
    window.addEventListener("resize", () => this.handleResize());
  }

  private setupEventHandlers(): void {
    document.getElementById("close-sidebar")?.addEventListener("click", () => {
      this.closeSidebar();
    });

    // Tab click handlers
    this.tabsEl.querySelectorAll(".tab").forEach((tab) => {
      tab.addEventListener("click", () => {
        this.tabsEl.querySelectorAll(".tab").forEach((t) => t.classList.remove("active"));
        tab.classList.add("active");
        this.currentTab = (tab as HTMLButtonElement).dataset.tab as TabName;
        if (this.selectedNode) {
          this.showNodeDetails(this.selectedNode);
        }
      });
    });
  }

  private setupVisualization(): void {
    const chatContainer = document.getElementById("chat-windows");
    if (!chatContainer) {
      console.warn("Chat windows container not found");
      return;
    }

    console.log("[Viz] Setting up visualization controller");
    this.vizController = new VisualizationController({
      container: chatContainer,
      autoAttach: true,
      sessionId: this.sessionId,
      layoutConstraints: {
        maxVisibleDecorators: 5,
        collisionPadding: 15,
      },
    });
    console.log("[Viz] Controller created:", this.vizController);
  }

  private async loadGraph(): Promise<void> {
    try {
      const resp = await fetch(`/api/sessions/${this.sessionId}/graph`);
      if (!resp.ok) {
        throw new Error(`Failed to load graph: ${resp.status}`);
      }
      const data: GraphData = await resp.json();

      // Convert to SimNode format
      this.nodes = data.nodes.map((n) => ({ ...n } as SimNode));
      this.edges = data.edges.map((e) => ({ source: e.source, target: e.target }));

      this.sessionNameEl.textContent = data.session.name;
      document.title = `${data.session.name} - Mantle Hub`;

      this.render();
      this.handleHash();

      // Attach decorators to existing nodes (for historical chat windows)
      if (this.vizController) {
        const nodeSnapshots = this.nodes.map((n) => ({
          id: n.id,
          state: n.state,
          branch: n.branch,
          sessionId: this.sessionId,
        }));
        this.vizController.attachToExistingNodes(nodeSnapshots);
      }
    } catch (e) {
      console.error("Failed to load graph:", e);
      this.sessionNameEl.textContent = "Error loading session";
    }
  }

  private handleHash(): void {
    const nodeId = location.hash.slice(1);
    if (nodeId) {
      const node = this.nodes.find((n) => n.id === nodeId);
      if (node) {
        this.selectNode(node, false);
      }
    }
  }

  private addNode(nodeInfo: NodeInfo): void {
    console.log("[Graph] addNode called:", nodeInfo.id, "state:", nodeInfo.state);
    const graphNode: SimNode = {
      id: nodeInfo.id,
      branch: nodeInfo.branch,
      state: nodeInfo.state,
      prompt: nodeInfo.prompt,
      parent_id: nodeInfo.parent_node_id ?? undefined,
      result_text: nodeInfo.result?.result_text,
      structured_output: nodeInfo.result?.structured_output,
      total_cost_usd: nodeInfo.result?.total_cost_usd,
      duration_secs: nodeInfo.result?.duration_secs,
    };

    this.nodes.push(graphNode);

    if (nodeInfo.parent_node_id) {
      this.edges.push({
        source: nodeInfo.parent_node_id,
        target: nodeInfo.id,
      });
    }

    // Notify visualization layer of new node
    if (this.vizController) {
      this.vizController.emitNodeCreated({
        id: nodeInfo.id,
        state: nodeInfo.state,
        branch: nodeInfo.branch,
        sessionId: this.sessionId,
      });
    }

    this.render();
  }

  private updateNodeResult(nodeId: string, result: NodeResult): void {
    const node = this.nodes.find((n) => n.id === nodeId);
    if (node) {
      const oldState = node.state;
      const newState = result.is_error ? "failed" : "completed";
      node.state = newState;
      node.result_text = result.result_text;
      node.structured_output = result.structured_output;
      node.total_cost_usd = result.total_cost_usd;
      node.duration_secs = result.duration_secs;

      // Notify visualization layer of state change
      if (this.vizController && oldState !== newState) {
        this.vizController.emitNodeStateChange(nodeId, oldState, newState);
      }

      this.render();

      if (this.selectedNode?.id === nodeId) {
        this.showNodeDetails(node);
      }
    }
  }

  private updateNodeState(nodeId: string, state: NodeState): void {
    const node = this.nodes.find((n) => n.id === nodeId);
    if (node) {
      const oldState = node.state;
      node.state = state;

      // Notify visualization layer of state change
      if (this.vizController && oldState !== state) {
        this.vizController.emitNodeStateChange(nodeId, oldState, state);
      }

      this.render();
    }
  }

  private cacheEvent(nodeId: string, event: StreamEvent, timestamp: string): void {
    console.log("[Graph] cacheEvent:", nodeId, event);
    if (!this.nodeEvents[nodeId]) {
      this.nodeEvents[nodeId] = [];
    }
    this.nodeEvents[nodeId].push({
      id: Date.now(),
      node_id: nodeId,
      event_type: this.getEventType(event),
      event,
      timestamp,
    });

    // Forward to visualization layer for live chat windows
    if (this.vizController) {
      console.log("[Graph] Forwarding to vizController");
      this.vizController.emitNodeEvent(nodeId, event, timestamp);
    }

    if (this.selectedNode?.id === nodeId && this.currentTab === "events") {
      this.showNodeDetails(this.selectedNode);
    }
  }

  private render(): void {
    // Links
    const links = this.linksGroup
      .selectAll<SVGLineElement, SimLink>(".link")
      .data(this.edges, (d) => {
        const src = typeof d.source === "string" ? d.source : d.source.id;
        const tgt = typeof d.target === "string" ? d.target : d.target.id;
        return `${src}-${tgt}`;
      });

    links.exit().remove();

    const linksEnter = links
      .enter()
      .append("line")
      .attr("class", "link")
      .attr("marker-end", "url(#arrow)");

    this.linkElements = linksEnter.merge(links);

    // Nodes
    const nodes = this.nodesGroup
      .selectAll<SVGGElement, SimNode>(".node")
      .data(this.nodes, (d) => d.id);

    nodes.exit().remove();

    const nodesEnter = nodes
      .enter()
      .append("g")
      .attr("class", "node")
      .call(this.drag())
      .on("click", (_event, d) => this.selectNode(d, true));

    nodesEnter.append("circle").attr("r", 20);
    nodesEnter.append("text").attr("dy", 30);

    this.nodeElements = nodesEnter.merge(nodes);

    // Update node attributes
    this.nodeElements
      .select("circle")
      .attr("fill", (d) => stateColors[d.state] || stateColors.pending);

    this.nodeElements
      .select("text")
      .text((d) => d.branch.split("/").pop()?.substring(0, 12) ?? "");

    this.nodeElements.classed("selected", (d) => d.id === this.selectedNode?.id);

    // Update simulation
    this.simulation.nodes(this.nodes);
    (this.simulation.force("link") as d3.ForceLink<SimNode, SimLink>).links(this.edges);
    this.simulation.alpha(0.3).restart();

    this.simulation.on("tick", () => {
      this.linkElements
        .attr("x1", (d) => (d.source as SimNode).x ?? 0)
        .attr("y1", (d) => (d.source as SimNode).y ?? 0)
        .attr("x2", (d) => (d.target as SimNode).x ?? 0)
        .attr("y2", (d) => (d.target as SimNode).y ?? 0);

      this.nodeElements.attr("transform", (d) => `translate(${d.x ?? 0},${d.y ?? 0})`);

      // Emit positions to visualization layer
      if (this.vizController) {
        const positions: NodePosition[] = this.nodes
          .filter((n) => n.x !== undefined && n.y !== undefined)
          .map((n) => ({
            nodeId: n.id,
            x: n.x!,
            y: n.y!,
            state: n.state,
          }));
        this.vizController.updatePositions(positions);
      }
    });
  }

  private drag() {
    return d3
      .drag<SVGGElement, SimNode>()
      .on("start", (event, d) => {
        if (!event.active) this.simulation.alphaTarget(0.3).restart();
        d.fx = d.x;
        d.fy = d.y;
      })
      .on("drag", (event, d) => {
        d.fx = event.x;
        d.fy = event.y;
      })
      .on("end", (event, d) => {
        if (!event.active) this.simulation.alphaTarget(0);
        d.fx = null;
        d.fy = null;
      });
  }

  private selectNode(node: SimNode, updateHash = true): void {
    this.selectedNode = node;
    if (updateHash) {
      history.pushState(null, "", `#${node.id}`);
    }
    this.showNodeDetails(node);
    this.render();
  }

  private showNodeDetails(node: SimNode): void {
    this.sidebar.classList.add("open");

    switch (this.currentTab) {
      case "overview":
        this.showOverviewTab(node);
        break;
      case "events":
        this.showEventsTab(node);
        break;
      case "spec":
        this.showSpecTab(node);
        break;
      case "children":
        this.showChildrenTab(node);
        break;
    }
  }

  private showOverviewTab(node: SimNode): void {
    const formatCost = (cost?: number) => (cost ? `$${cost.toFixed(4)}` : "-");
    const formatDuration = (secs?: number) => (secs ? `${secs.toFixed(1)}s` : "-");

    this.sidebarContent.innerHTML = `
      <div class="node-detail">
        <div class="field">
          <span class="label">Branch</span>
          <span class="value">${this.escapeHtml(node.branch)}</span>
        </div>

        <div class="field">
          <span class="label">State</span>
          <span class="state ${node.state}">${node.state}</span>
        </div>

        <div class="field">
          <span class="label">Prompt</span>
          <pre>${this.escapeHtml(node.prompt || "")}</pre>
        </div>

        ${
          node.result_text
            ? `
        <div class="field">
          <span class="label">Result</span>
          <pre>${this.escapeHtml(node.result_text)}</pre>
        </div>
        `
            : ""
        }

        ${
          node.structured_output
            ? `
        <div class="field">
          <span class="label">Structured Output</span>
          <pre>${this.escapeHtml(JSON.stringify(node.structured_output, null, 2))}</pre>
        </div>
        `
            : ""
        }

        ${
          node.total_cost_usd || node.duration_secs
            ? `
        <div class="metrics">
          <div class="metric">
            <div class="value">${formatCost(node.total_cost_usd)}</div>
            <div class="label">Cost</div>
          </div>
          <div class="metric">
            <div class="value">${formatDuration(node.duration_secs)}</div>
            <div class="label">Duration</div>
          </div>
        </div>
        `
            : ""
        }

        <div class="field">
          <span class="label">Node ID</span>
          <span class="value mono">${node.id}</span>
        </div>
      </div>
    `;
  }

  private async showEventsTab(node: SimNode): Promise<void> {
    // Check cache first
    if (!this.nodeEvents[node.id]) {
      this.sidebarContent.innerHTML = '<div class="loading">Loading events...</div>';

      try {
        const resp = await fetch(`/api/sessions/${this.sessionId}/nodes/${node.id}/events`);
        if (resp.ok) {
          const events: NodeEvent[] = await resp.json();
          this.nodeEvents[node.id] = events;
        } else {
          this.nodeEvents[node.id] = [];
        }
      } catch (e) {
        console.error("Failed to load events:", e);
        this.nodeEvents[node.id] = [];
      }
    }

    const events = this.nodeEvents[node.id] || [];

    if (events.length === 0) {
      this.sidebarContent.innerHTML = '<div class="empty">No events recorded</div>';
      return;
    }

    this.sidebarContent.innerHTML = `
      <div class="events-list">
        ${events.map((e) => this.renderEvent(e)).join("")}
      </div>
    `;
  }

  private renderEvent(event: NodeEvent): string {
    const eventData = event.event;
    const eventType = event.event_type;
    const timestamp = event.timestamp ? new Date(event.timestamp).toLocaleTimeString() : "";

    let content = "";
    if (typeof eventData === "object" && eventData !== null) {
      // Handle actual StreamEvent format from backend: { type: "assistant", message: { content: [...] } }
      if ("type" in eventData && eventData.type === "assistant" && "message" in eventData) {
        const msg = eventData.message as { content: Array<{ type: string; text?: string; name?: string; input?: unknown }> };
        if (msg.content && Array.isArray(msg.content)) {
          content = msg.content.map((block) => {
            if (block.type === "text" && block.text) {
              return `<pre class="event-content">${this.escapeHtml(block.text)}</pre>`;
            } else if (block.type === "tool_use" && block.name) {
              return `
                <details class="tool-use">
                  <summary>${this.escapeHtml(block.name)}</summary>
                  <pre>${this.escapeHtml(JSON.stringify(block.input || {}, null, 2))}</pre>
                </details>
              `;
            } else if (block.type === "tool_result") {
              const resultBlock = block as { content?: string };
              return `<pre class="event-content tool-result">${this.escapeHtml(resultBlock.content || "")}</pre>`;
            }
            return `<pre class="event-content">${this.escapeHtml(JSON.stringify(block, null, 2))}</pre>`;
          }).join("");
        }
      // Legacy format support
      } else if ("AssistantMessage" in eventData) {
        const msg = eventData.AssistantMessage as { content: string };
        content = `<pre class="event-content">${this.escapeHtml(msg.content || "")}</pre>`;
      } else if ("ToolUse" in eventData) {
        const tool = eventData.ToolUse as { name: string; input: unknown };
        content = `
          <details class="tool-use">
            <summary>${this.escapeHtml(tool.name || "tool")}</summary>
            <pre>${this.escapeHtml(JSON.stringify(tool.input || {}, null, 2))}</pre>
          </details>
        `;
      } else if ("ToolResult" in eventData) {
        const result = eventData.ToolResult as { output: string };
        content = `<pre class="event-content tool-result">${this.escapeHtml(result.output || "")}</pre>`;
      } else {
        content = `<pre class="event-content">${this.escapeHtml(JSON.stringify(eventData, null, 2))}</pre>`;
      }
    } else {
      content = `<pre class="event-content">${this.escapeHtml(String(eventData))}</pre>`;
    }

    return `
      <div class="event ${eventType.toLowerCase()}">
        <div class="event-header">
          <span class="event-type">${eventType}</span>
          <span class="event-time">${timestamp}</span>
        </div>
        ${content}
      </div>
    `;
  }

  private getEventType(eventData: StreamEvent): string {
    if (!eventData || typeof eventData !== "object") return "unknown";
    const keys = Object.keys(eventData);
    return keys[0] || "unknown";
  }

  private async showSpecTab(node: SimNode): Promise<void> {
    try {
      const resp = await fetch(`/api/sessions/${this.sessionId}/nodes/${node.id}`);
      const nodeInfo: NodeInfo = await resp.json();
      this.sidebarContent.innerHTML = `
        <div class="spec-view">
          <pre>${this.escapeHtml(JSON.stringify(nodeInfo, null, 2))}</pre>
        </div>
      `;
    } catch {
      this.sidebarContent.innerHTML = `
        <div class="spec-view">
          <pre>${this.escapeHtml(JSON.stringify(node, null, 2))}</pre>
        </div>
      `;
    }
  }

  private showChildrenTab(node: SimNode): void {
    const children = this.nodes.filter((n) => n.parent_id === node.id);

    if (children.length === 0) {
      this.sidebarContent.innerHTML = '<div class="empty">No child nodes</div>';
      return;
    }

    this.sidebarContent.innerHTML = `
      <div class="children-list">
        ${children
          .map(
            (child) => `
          <div class="child-node" data-id="${child.id}">
            <span class="state ${child.state}"></span>
            <span class="branch">${this.escapeHtml(child.branch)}</span>
          </div>
        `
          )
          .join("")}
      </div>
    `;

    // Add click handlers
    this.sidebarContent.querySelectorAll(".child-node").forEach((el) => {
      el.addEventListener("click", () => {
        const childId = (el as HTMLElement).dataset.id;
        const child = this.nodes.find((n) => n.id === childId);
        if (child) {
          this.selectNode(child, true);
        }
      });
    });
  }

  private closeSidebar(): void {
    this.sidebar.classList.remove("open");
    this.selectedNode = null;
    history.pushState(null, "", location.pathname);
    this.render();
  }

  private escapeHtml(text: string): string {
    if (!text) return "";
    const div = document.createElement("div");
    div.textContent = text;
    return div.innerHTML;
  }

  private handleResize(): void {
    const container = document.getElementById("graph-container") as HTMLElement;
    this.width = container.clientWidth;
    this.height = container.clientHeight;

    this.svg.attr("width", this.width).attr("height", this.height);

    this.simulation.force("center", d3.forceCenter(this.width / 2, this.height / 2));
    this.simulation.alpha(0.3).restart();
  }
}

// Initialize on page load
document.addEventListener("DOMContentLoaded", () => {
  new SessionGraph();
});
