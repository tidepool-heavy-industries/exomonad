/**
 * D3 + dagre graph renderer for Tidepool graph visualization.
 */
import * as d3 from "d3";
import * as dagre from "dagre";
import type { GraphExport } from "./types";

interface DagreNodeData {
  label: string;
  width: number;
  height: number;
  kind?: "LLM" | "Logic" | "entry" | "exit";
  x?: number;
  y?: number;
}

/**
 * Render a graph into an HTML container using D3 and dagre layout.
 */
export function renderGraph(container: HTMLElement, graph: GraphExport): void {
  // Clear any existing content
  container.innerHTML = "";

  const width = container.clientWidth || 800;
  const height = container.clientHeight || 600;

  // Create SVG
  const svg = d3
    .select(container)
    .append("svg")
    .attr("width", width)
    .attr("height", height)
    .attr("viewBox", `0 0 ${width} ${height}`);

  const g = svg.append("g");

  // Create dagre graph
  const dagreGraph = new dagre.graphlib.Graph();
  dagreGraph.setGraph({ rankdir: "TB", nodesep: 70, ranksep: 100 });
  dagreGraph.setDefaultEdgeLabel(() => ({}));

  // Add entry node if present
  if (graph.entryType) {
    dagreGraph.setNode("entry", {
      label: "Entry",
      width: 80,
      height: 80,
      kind: "entry",
    } as DagreNodeData);
  }

  // Add graph nodes
  Object.entries(graph.nodes).forEach(([name, node]) => {
    dagreGraph.setNode(name, {
      label: name,
      width: 140,
      height: node.kind === "LLM" ? 80 : 70,
      kind: node.kind,
    } as DagreNodeData);
  });

  // Add exit node if present
  if (graph.exitType) {
    dagreGraph.setNode("exit", {
      label: "Exit",
      width: 80,
      height: 80,
      kind: "exit",
    } as DagreNodeData);
  }

  // Add edges from graph data
  graph.edges.forEach((edge) => {
    dagreGraph.setEdge(edge.from, edge.to, { label: edge.payload || "" });
  });

  // Compute layout
  dagre.layout(dagreGraph);

  // Define arrowhead marker
  svg
    .append("defs")
    .append("marker")
    .attr("id", "arrowhead")
    .attr("viewBox", "0 0 10 10")
    .attr("refX", 9)
    .attr("refY", 5)
    .attr("markerWidth", 6)
    .attr("markerHeight", 6)
    .attr("orient", "auto")
    .append("path")
    .attr("d", "M 0 0 L 10 5 L 0 10 z")
    .attr("fill", "#6b7280");

  // Render edges
  const edgesGroup = g.append("g").attr("class", "edges");
  graph.edges.forEach((edge) => {
    const edgeData = dagreGraph.edge(edge.from, edge.to);
    if (!edgeData?.points) return;

    const points = edgeData.points;

    const line = d3
      .line<{ x: number; y: number }>()
      .x((d) => d.x)
      .y((d) => d.y)
      .curve(d3.curveBasis);

    edgesGroup
      .append("path")
      .attr("d", line(points))
      .attr("stroke", edge.kind === "explicit" ? "#60a5fa" : "#6b7280")
      .attr("stroke-width", 2)
      .attr("fill", "none")
      .attr("marker-end", "url(#arrowhead)");

    // Edge label (payload type)
    if (edge.payload) {
      const midpoint = points[Math.floor(points.length / 2)];
      edgesGroup
        .append("text")
        .attr("x", midpoint.x)
        .attr("y", midpoint.y - 8)
        .attr("text-anchor", "middle")
        .attr("fill", "#9ca3af")
        .attr("font-size", "11px")
        .attr("font-family", "monospace")
        .text(edge.payload);
    }
  });

  // Render nodes
  const nodesGroup = g.append("g").attr("class", "nodes");

  dagreGraph.nodes().forEach((nodeName) => {
    const nodeData = dagreGraph.node(nodeName) as DagreNodeData;
    if (!nodeData.x || !nodeData.y) return;

    const nodeGroup = nodesGroup
      .append("g")
      .attr("transform", `translate(${nodeData.x},${nodeData.y})`)
      .attr("class", "node")
      .style("cursor", "pointer");

    // Shape based on kind
    if (nodeData.kind === "entry" || nodeData.kind === "exit") {
      // Entry/Exit: circles
      nodeGroup
        .append("circle")
        .attr("r", 40)
        .attr("fill", "#374151")
        .attr("stroke", nodeData.kind === "entry" ? "#22c55e" : "#ef4444")
        .attr("stroke-width", 3);
    } else if (nodeData.kind === "LLM") {
      // LLM nodes: rounded rectangles (stadium shape)
      nodeGroup
        .append("rect")
        .attr("x", -70)
        .attr("y", -40)
        .attr("width", 140)
        .attr("height", 80)
        .attr("rx", 20)
        .attr("fill", "#1e3a8a")
        .attr("stroke", "#60a5fa")
        .attr("stroke-width", 2);
    } else {
      // Logic nodes: hexagon
      const hexPoints = [
        [-70, 0],
        [-50, -35],
        [50, -35],
        [70, 0],
        [50, 35],
        [-50, 35],
      ]
        .map((p) => p.join(","))
        .join(" ");

      nodeGroup
        .append("polygon")
        .attr("points", hexPoints)
        .attr("fill", "#14532d")
        .attr("stroke", "#22c55e")
        .attr("stroke-width", 2);
    }

    // Node label
    nodeGroup
      .append("text")
      .attr("text-anchor", "middle")
      .attr("dy", "0.3em")
      .attr("fill", "white")
      .attr("font-size", "14px")
      .attr("font-weight", "500")
      .text(nodeData.label);

    // Kind sublabel for LLM/Logic nodes
    if (nodeData.kind === "LLM" || nodeData.kind === "Logic") {
      nodeGroup
        .append("text")
        .attr("text-anchor", "middle")
        .attr("dy", "1.8em")
        .attr("fill", "#9ca3af")
        .attr("font-size", "10px")
        .text(nodeData.kind);
    }
  });

  // Add zoom behavior
  const zoom = d3
    .zoom<SVGSVGElement, unknown>()
    .scaleExtent([0.1, 4])
    .on("zoom", (event) => {
      g.attr("transform", event.transform);
    });

  svg.call(zoom);

  // Center the graph
  const graphBounds = g.node()?.getBBox();
  if (graphBounds) {
    const scale = Math.min(
      width / (graphBounds.width + 100),
      height / (graphBounds.height + 100),
      1
    );
    const translateX =
      (width - graphBounds.width * scale) / 2 - graphBounds.x * scale;
    const translateY =
      (height - graphBounds.height * scale) / 2 - graphBounds.y * scale;

    svg.call(
      zoom.transform,
      d3.zoomIdentity.translate(translateX, translateY).scale(scale)
    );
  }
}
