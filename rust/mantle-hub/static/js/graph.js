// Mantle Hub Graph Visualization

const stateColors = {
    pending: '#facc15',
    running: '#3b82f6',
    completed: '#22c55e',
    failed: '#ef4444'
};

class MantleGraph {
    constructor() {
        this.svg = d3.select('#graph');
        this.sidebar = document.getElementById('sidebar');
        this.sidebarContent = document.getElementById('sidebar-content');
        this.statusEl = document.getElementById('status');

        this.nodes = [];
        this.edges = [];
        this.selectedNode = null;

        this.setupSVG();
        this.setupWebSocket();
        this.setupEventHandlers();
    }

    setupSVG() {
        const container = document.getElementById('graph-container');
        this.width = container.clientWidth;
        this.height = container.clientHeight;

        this.svg
            .attr('width', this.width)
            .attr('height', this.height);

        // Arrow marker for links
        this.svg.append('defs').append('marker')
            .attr('id', 'arrow')
            .attr('viewBox', '0 -5 10 10')
            .attr('refX', 25)
            .attr('refY', 0)
            .attr('markerWidth', 6)
            .attr('markerHeight', 6)
            .attr('orient', 'auto')
            .append('path')
            .attr('d', 'M0,-5L10,0L0,5')
            .attr('class', 'link-arrow');

        // Groups for links and nodes
        this.linksGroup = this.svg.append('g').attr('class', 'links');
        this.nodesGroup = this.svg.append('g').attr('class', 'nodes');

        // Force simulation
        this.simulation = d3.forceSimulation()
            .force('link', d3.forceLink().id(d => d.id).distance(100))
            .force('charge', d3.forceManyBody().strength(-300))
            .force('center', d3.forceCenter(this.width / 2, this.height / 2))
            .force('collision', d3.forceCollide().radius(40));

        // Handle window resize
        window.addEventListener('resize', () => this.handleResize());
    }

    setupWebSocket() {
        const protocol = location.protocol === 'https:' ? 'wss:' : 'ws:';
        const wsUrl = `${protocol}//${location.host}/ws`;

        this.connect(wsUrl);
    }

    connect(wsUrl) {
        this.ws = new WebSocket(wsUrl);

        this.ws.onopen = () => {
            this.statusEl.textContent = 'Connected';
            this.statusEl.className = 'status connected';
        };

        this.ws.onclose = () => {
            this.statusEl.textContent = 'Disconnected - Reconnecting...';
            this.statusEl.className = 'status disconnected';
            setTimeout(() => this.connect(wsUrl), 3000);
        };

        this.ws.onerror = (err) => {
            console.error('WebSocket error:', err);
        };

        this.ws.onmessage = (event) => {
            const msg = JSON.parse(event.data);
            this.handleMessage(msg);
        };
    }

    setupEventHandlers() {
        document.getElementById('close-sidebar').addEventListener('click', () => {
            this.closeSidebar();
        });
    }

    handleMessage(msg) {
        switch (msg.type) {
            case 'init':
                this.nodes = msg.graph.nodes;
                this.edges = msg.graph.edges;
                this.render();
                break;
            case 'session_started':
            case 'session_updated':
                this.updateOrAddNode(msg.session);
                break;
            case 'session_completed':
                this.updateNodeResult(msg.session_id, msg.result);
                break;
            case 'session_failed':
                this.updateNodeState(msg.session_id, 'failed');
                break;
        }
    }

    updateOrAddNode(session) {
        const existing = this.nodes.find(n => n.id === session.id);
        if (existing) {
            Object.assign(existing, {
                branch: session.branch,
                state: session.state,
                prompt: session.prompt,
                result_text: session.result?.result_text,
                structured_output: session.result?.structured_output,
                total_cost_usd: session.result?.total_cost_usd,
                duration_secs: session.result?.duration_secs
            });
        } else {
            this.nodes.push({
                id: session.id,
                branch: session.branch,
                state: session.state,
                prompt: session.prompt
            });

            if (session.parent_id) {
                this.edges.push({
                    source: session.parent_id,
                    target: session.id
                });
            }
        }
        this.render();
    }

    updateNodeResult(sessionId, result) {
        const node = this.nodes.find(n => n.id === sessionId);
        if (node) {
            node.state = result.is_error ? 'failed' : 'completed';
            node.result_text = result.result_text;
            node.structured_output = result.structured_output;
            node.total_cost_usd = result.total_cost_usd;
            node.duration_secs = result.duration_secs;
            this.render();

            // Update sidebar if this node is selected
            if (this.selectedNode?.id === sessionId) {
                this.showNodeDetails(node);
            }
        }
    }

    updateNodeState(sessionId, state) {
        const node = this.nodes.find(n => n.id === sessionId);
        if (node) {
            node.state = state;
            this.render();
        }
    }

    render() {
        // Links
        const links = this.linksGroup.selectAll('.link')
            .data(this.edges, d => `${d.source.id || d.source}-${d.target.id || d.target}`);

        links.exit().remove();

        const linksEnter = links.enter()
            .append('line')
            .attr('class', 'link')
            .attr('marker-end', 'url(#arrow)');

        this.linkElements = linksEnter.merge(links);

        // Nodes
        const nodes = this.nodesGroup.selectAll('.node')
            .data(this.nodes, d => d.id);

        nodes.exit().remove();

        const nodesEnter = nodes.enter()
            .append('g')
            .attr('class', 'node')
            .call(this.drag())
            .on('click', (event, d) => this.selectNode(d));

        nodesEnter.append('circle')
            .attr('r', 20);

        nodesEnter.append('text')
            .attr('dy', 30);

        this.nodeElements = nodesEnter.merge(nodes);

        // Update node attributes
        this.nodeElements.select('circle')
            .attr('fill', d => stateColors[d.state] || stateColors.pending);

        this.nodeElements.select('text')
            .text(d => d.branch.split('/').pop().substring(0, 12));

        this.nodeElements.classed('selected', d => d.id === this.selectedNode?.id);

        // Update simulation
        this.simulation.nodes(this.nodes);
        this.simulation.force('link').links(this.edges);
        this.simulation.alpha(0.3).restart();

        this.simulation.on('tick', () => {
            this.linkElements
                .attr('x1', d => d.source.x)
                .attr('y1', d => d.source.y)
                .attr('x2', d => d.target.x)
                .attr('y2', d => d.target.y);

            this.nodeElements
                .attr('transform', d => `translate(${d.x},${d.y})`);
        });
    }

    drag() {
        return d3.drag()
            .on('start', (event, d) => {
                if (!event.active) this.simulation.alphaTarget(0.3).restart();
                d.fx = d.x;
                d.fy = d.y;
            })
            .on('drag', (event, d) => {
                d.fx = event.x;
                d.fy = event.y;
            })
            .on('end', (event, d) => {
                if (!event.active) this.simulation.alphaTarget(0);
                d.fx = null;
                d.fy = null;
            });
    }

    selectNode(node) {
        this.selectedNode = node;
        this.showNodeDetails(node);
        this.render();
    }

    showNodeDetails(node) {
        this.sidebar.classList.add('open');

        const formatCost = (cost) => cost ? `$${cost.toFixed(4)}` : '-';
        const formatDuration = (secs) => secs ? `${secs.toFixed(1)}s` : '-';

        this.sidebarContent.innerHTML = `
            <div class="session-detail">
                <div class="field">
                    <span class="label">Branch</span>
                    <span class="value">${node.branch}</span>
                </div>

                <div class="field">
                    <span class="label">State</span>
                    <span class="state ${node.state}">${node.state}</span>
                </div>

                <div class="field">
                    <span class="label">Prompt</span>
                    <pre>${this.escapeHtml(node.prompt)}</pre>
                </div>

                ${node.result_text ? `
                <div class="field">
                    <span class="label">Result</span>
                    <pre>${this.escapeHtml(node.result_text)}</pre>
                </div>
                ` : ''}

                ${node.structured_output ? `
                <div class="field">
                    <span class="label">Structured Output</span>
                    <pre>${this.escapeHtml(JSON.stringify(node.structured_output, null, 2))}</pre>
                </div>
                ` : ''}

                ${(node.total_cost_usd || node.duration_secs) ? `
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
                ` : ''}

                <div class="field">
                    <span class="label">Session ID</span>
                    <span class="value" style="font-family: monospace; font-size: 0.75rem;">${node.id}</span>
                </div>
            </div>
        `;
    }

    closeSidebar() {
        this.sidebar.classList.remove('open');
        this.selectedNode = null;
        this.render();
    }

    escapeHtml(text) {
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }

    handleResize() {
        const container = document.getElementById('graph-container');
        this.width = container.clientWidth;
        this.height = container.clientHeight;

        this.svg
            .attr('width', this.width)
            .attr('height', this.height);

        this.simulation.force('center', d3.forceCenter(this.width / 2, this.height / 2));
        this.simulation.alpha(0.3).restart();
    }
}

// Initialize on page load
document.addEventListener('DOMContentLoaded', () => {
    window.mantleGraph = new MantleGraph();
});
