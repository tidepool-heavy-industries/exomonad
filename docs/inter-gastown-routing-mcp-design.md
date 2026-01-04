# Inter-Gas Town Routing MCP Design

Status: Research/Design (Agent 10 - gt-6n4)

## Overview

Two Gas Town instances coordinate work via a tidepool-based MCP server. The routing graph classifies incoming requests, deduplicates, routes to appropriate Gas Town instances, and handles escalation for cross-rig coordination.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    MCP Routing Server                            │
│                                                                  │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  Tidepool Routing Graph                                   │   │
│  │                                                           │   │
│  │  Entry(RoutingRequest)                                    │   │
│  │     │                                                     │   │
│  │     ▼                                                     │   │
│  │  classify (LLM) ─────► RequestCategory                    │   │
│  │     │                                                     │   │
│  │     ▼                                                     │   │
│  │  dedupe (Logic) ────► Checks request cache                │   │
│  │     │                                                     │   │
│  │     ▼                                                     │   │
│  │  route (Logic) ─────► Determines target Gas Town(s)       │   │
│  │     │                                                     │   │
│  │     ├──► dispatchA ──► Gas Town A                         │   │
│  │     ├──► dispatchB ──► Gas Town B                         │   │
│  │     └──► escalate ───► Mayor/cross-rig coordination       │   │
│  │                                                           │   │
│  │  aggregate (Logic) ◄─── Collects responses                │   │
│  │     │                                                     │   │
│  │     ▼                                                     │   │
│  │  Exit(RoutingResponse)                                    │   │
│  └──────────────────────────────────────────────────────────┘   │
│                                                                  │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  MCP Protocol Layer (TypeScript)                          │   │
│  │                                                           │   │
│  │  Resources:                                               │   │
│  │    - gastown://rigs - List available Gas Town instances   │   │
│  │    - gastown://status/{rig} - Rig status                  │   │
│  │    - gastown://queue/{rig} - Pending work queue           │   │
│  │                                                           │   │
│  │  Tools:                                                   │   │
│  │    - route_request(request, hints?) - Route to Gas Town   │   │
│  │    - escalate(request, reason) - Escalate to Mayor        │   │
│  │    - check_status(rig) - Health check                     │   │
│  │                                                           │   │
│  │  Prompts:                                                 │   │
│  │    - coordinate_handoff - Cross-rig work handoff          │   │
│  │    - resolve_conflict - Merge conflict resolution         │   │
│  └──────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘
          │                           │                     │
          ▼                           ▼                     ▼
   ┌──────────────┐           ┌──────────────┐      ┌──────────────┐
   │  Gas Town A  │           │  Gas Town B  │      │  Mayor       │
   │  (Rig: foo)  │           │  (Rig: bar)  │      │  (Coord)     │
   └──────────────┘           └──────────────┘      └──────────────┘
```

## Graph Definition

```haskell
-- tidepool-mcp/src/Tidepool/MCP/Routing.hs

module Tidepool.MCP.Routing
  ( RoutingGraph(..)
  , RoutingRequest(..)
  , RoutingResponse(..)
  , RequestCategory(..)
  , routingHandlers
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import Tidepool.Graph.Types (type (:@), Needs, UsesEffects, Exit)
import Tidepool.Graph.Generic (GraphMode(..), type (:-))
import qualified Tidepool.Graph.Generic as G
import Tidepool.Graph.Goto (Goto, GotoChoice, To, gotoChoice, gotoExit)
import Tidepool.Graph.Memory (Memory)

-- ============================================================================
-- Domain Types
-- ============================================================================

-- | Incoming routing request
data RoutingRequest = RoutingRequest
  { reqSource      :: Text        -- Which Gas Town instance sent this
  , reqIntent      :: Text        -- What the request wants to accomplish
  , reqPayload     :: Value       -- The actual work payload (bead, mail, etc.)
  , reqPriority    :: Int         -- 0 = highest
  , reqCorrelation :: Text        -- For deduplication and tracing
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Classification of request type
data RequestCategory
  = WorkDispatch    -- Route work to a polecat
  | StatusQuery     -- Query rig/polecat status
  | EscalationReq   -- Cross-rig escalation
  | BeadSync        -- Beads synchronization
  | MailDelivery    -- Inter-rig mail
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Routing decision
data RoutingDecision = RoutingDecision
  { targetRigs    :: [Text]       -- Which Gas Town instances
  , strategy      :: RouteStrategy
  , reason        :: Text         -- For observability
  }
  deriving (Show, Eq, Generic)

data RouteStrategy
  = Unicast        -- Send to one rig
  | Multicast      -- Send to multiple rigs
  | Broadcast      -- Send to all rigs
  | Escalate       -- Send to Mayor for coordination
  deriving (Show, Eq, Generic)

-- | Final response
data RoutingResponse = RoutingResponse
  { respStatus    :: ResponseStatus
  , respPayload   :: Maybe Value
  , respTargets   :: [Text]       -- Which rigs received the request
  , respTraceId   :: Text         -- For observability
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ResponseStatus
  = Routed          -- Successfully routed
  | Deduplicated    -- Request was a duplicate
  | Escalated       -- Sent to Mayor
  | Failed Text     -- Failed with reason
  deriving (Show, Eq, Generic)

-- | Routing state (for deduplication and caching)
data RoutingState = RoutingState
  { recentRequests :: Map Text UTCTime  -- correlation -> timestamp
  , rigStatus      :: Map Text RigHealth
  , routingMetrics :: RoutingMetrics
  }

data RigHealth = RigHealth
  { healthStatus    :: HealthStatus
  , lastSeen        :: UTCTime
  , pendingWork     :: Int
  , polecatCount    :: Int
  }

-- ============================================================================
-- Graph Definition
-- ============================================================================

-- | Inter-Gas Town Routing Graph
data RoutingGraph mode = RoutingGraph
  { -- Entry: receives routing request from MCP
    entry :: mode :- G.Entry RoutingRequest

    -- Classify: LLM determines request category
  , classify :: mode :- G.LLMNode
      :@ Needs '[RoutingRequest]
      :@ Template ClassifyTpl
      :@ Schema RequestCategory

    -- Dedupe: Check if request is duplicate
  , dedupe :: mode :- G.LogicNode
      :@ Needs '[RequestCategory, RoutingRequest]
      :@ Memory RoutingState
      :@ UsesEffects '[Goto "route" RoutingRequest
                      , Goto Exit RoutingResponse]  -- Exit early if duplicate

    -- Route: Determine target Gas Town(s)
  , route :: mode :- G.LogicNode
      :@ Needs '[RoutingRequest]
      :@ Memory RoutingState
      :@ UsesEffects '[Goto "dispatchSingle" (RoutingRequest, Text)
                      , Goto "dispatchMulti" (RoutingRequest, [Text])
                      , Goto "escalate" RoutingRequest]

    -- Dispatch to single rig
  , dispatchSingle :: mode :- G.LogicNode
      :@ Needs '[(RoutingRequest, Text)]
      :@ UsesEffects '[RigCall, Goto "aggregate" (Text, DispatchResult)]

    -- Dispatch to multiple rigs
  , dispatchMulti :: mode :- G.LogicNode
      :@ Needs '[(RoutingRequest, [Text])]
      :@ UsesEffects '[RigCall, Goto "aggregate" [(Text, DispatchResult)]]

    -- Escalate to Mayor
  , escalate :: mode :- G.LogicNode
      :@ Needs '[RoutingRequest]
      :@ UsesEffects '[MayorCall, Goto Exit RoutingResponse]

    -- Aggregate responses
  , aggregate :: mode :- G.LogicNode
      :@ Needs '[[(Text, DispatchResult)]]
      :@ UsesEffects '[Goto Exit RoutingResponse]

    -- Exit: return routing response
  , exit :: mode :- G.Exit RoutingResponse
  }
  deriving Generic
```

## Effects

### RigCall Effect

```haskell
-- | Effect for calling a Gas Town rig
data RigCall :: Effect where
  -- Send request to a rig, await response
  CallRig :: Text -> RoutingRequest -> RigCall m DispatchResult

  -- Get rig health status
  GetRigHealth :: Text -> RigCall m RigHealth

  -- List available rigs
  ListRigs :: RigCall m [Text]

data DispatchResult
  = DispatchSuccess Value
  | DispatchQueued Text      -- Work ID for async tracking
  | DispatchFailed Text      -- Error message
  deriving (Show, Eq, Generic)
```

### MayorCall Effect

```haskell
-- | Effect for escalating to Mayor
data MayorCall :: Effect where
  -- Escalate request requiring coordination
  EscalateToMayor :: RoutingRequest -> Text -> MayorCall m EscalationResult

  -- Query Mayor for routing hints
  GetRoutingHints :: RoutingRequest -> MayorCall m [Text]

data EscalationResult
  = EscalationAccepted Text  -- Tracking ID
  | EscalationDeferred Text  -- Reason for deferral
  | EscalationRejected Text  -- Why it was rejected
  deriving (Show, Eq, Generic)
```

## MCP Server Wrapper

The MCP server wraps the tidepool graph, exposing it via JSON-RPC:

```typescript
// tidepool-mcp/ts/src/server.ts

import { Server } from "@modelcontextprotocol/sdk/server/index.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { initializeWasm, stepWasm } from "./wasm-bridge.js";

const server = new Server({
  name: "inter-gastown-router",
  version: "0.1.0",
});

// Resources: Read-only data about Gas Town instances
server.setRequestHandler("resources/list", async () => ({
  resources: [
    { uri: "gastown://rigs", name: "Available Rigs", mimeType: "application/json" },
    { uri: "gastown://status", name: "All Rig Status", mimeType: "application/json" },
  ]
}));

server.setRequestHandler("resources/read", async (request) => {
  const uri = request.params.uri;

  if (uri === "gastown://rigs") {
    const rigs = await getRigList();
    return { contents: [{ uri, mimeType: "application/json", text: JSON.stringify(rigs) }] };
  }

  if (uri.startsWith("gastown://status/")) {
    const rig = uri.replace("gastown://status/", "");
    const status = await getRigStatus(rig);
    return { contents: [{ uri, mimeType: "application/json", text: JSON.stringify(status) }] };
  }

  throw new Error(`Unknown resource: ${uri}`);
});

// Tools: Actions that invoke the routing graph
server.setRequestHandler("tools/list", async () => ({
  tools: [
    {
      name: "route_request",
      description: "Route a request to the appropriate Gas Town instance",
      inputSchema: {
        type: "object",
        properties: {
          intent: { type: "string", description: "What the request wants to accomplish" },
          payload: { type: "object", description: "The work payload" },
          priority: { type: "integer", description: "Priority (0 = highest)" },
          hints: { type: "array", items: { type: "string" }, description: "Optional routing hints" },
        },
        required: ["intent", "payload"],
      },
    },
    {
      name: "escalate",
      description: "Escalate a request to the Mayor for cross-rig coordination",
      inputSchema: {
        type: "object",
        properties: {
          request: { type: "object", description: "The original request" },
          reason: { type: "string", description: "Why escalation is needed" },
        },
        required: ["request", "reason"],
      },
    },
    {
      name: "check_status",
      description: "Check health status of a Gas Town rig",
      inputSchema: {
        type: "object",
        properties: {
          rig: { type: "string", description: "Rig name to check" },
        },
        required: ["rig"],
      },
    },
  ],
}));

server.setRequestHandler("tools/call", async (request) => {
  const { name, arguments: args } = request.params;

  switch (name) {
    case "route_request": {
      // Initialize routing graph with request
      const routingRequest = {
        reqSource: "mcp-client",
        reqIntent: args.intent,
        reqPayload: args.payload,
        reqPriority: args.priority ?? 2,
        reqCorrelation: crypto.randomUUID(),
      };

      // Run graph to completion via WASM
      const result = await runRoutingGraph(routingRequest);
      return { content: [{ type: "text", text: JSON.stringify(result) }] };
    }

    case "escalate": {
      const result = await escalateToMayor(args.request, args.reason);
      return { content: [{ type: "text", text: JSON.stringify(result) }] };
    }

    case "check_status": {
      const status = await getRigStatus(args.rig);
      return { content: [{ type: "text", text: JSON.stringify(status) }] };
    }

    default:
      throw new Error(`Unknown tool: ${name}`);
  }
});

// Prompts: Templated workflows
server.setRequestHandler("prompts/list", async () => ({
  prompts: [
    {
      name: "coordinate_handoff",
      description: "Template for coordinating work handoff between rigs",
      arguments: [
        { name: "source_rig", required: true },
        { name: "target_rig", required: true },
        { name: "work_description", required: true },
      ],
    },
    {
      name: "resolve_conflict",
      description: "Template for resolving conflicts between rig work",
      arguments: [
        { name: "conflict_description", required: true },
        { name: "rigs_involved", required: true },
      ],
    },
  ],
}));

// Start server
const transport = new StdioServerTransport();
await server.connect(transport);
```

## Docker Compose Topology

```yaml
# docker-compose.yml

version: "3.8"

services:
  # MCP Routing Server
  routing:
    build:
      context: ./tidepool-mcp
      dockerfile: Dockerfile
    ports:
      - "3000:3000"
    environment:
      - GASTOWN_A_URL=http://gastown-a:8080
      - GASTOWN_B_URL=http://gastown-b:8080
      - OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4318
    volumes:
      - routing-state:/app/state
    depends_on:
      - gastown-a
      - gastown-b
      - otel-collector

  # Gas Town Instance A (e.g., tidepool rig)
  gastown-a:
    build:
      context: ./gastown
      dockerfile: Dockerfile
    environment:
      - RIG_NAME=tidepool
      - MAYOR_URL=http://mayor:8080
      - OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4318
    volumes:
      - gastown-a-data:/app/data
      - ./repos/tidepool:/workspace

  # Gas Town Instance B (e.g., anemone rig)
  gastown-b:
    build:
      context: ./gastown
      dockerfile: Dockerfile
    environment:
      - RIG_NAME=anemone
      - MAYOR_URL=http://mayor:8080
      - OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4318
    volumes:
      - gastown-b-data:/app/data
      - ./repos/anemone:/workspace

  # Mayor (global coordinator)
  mayor:
    build:
      context: ./mayor
      dockerfile: Dockerfile
    ports:
      - "8090:8080"
    environment:
      - OTEL_EXPORTER_OTLP_ENDPOINT=http://otel-collector:4318
    volumes:
      - mayor-data:/app/data

  # OpenTelemetry Collector
  otel-collector:
    image: otel/opentelemetry-collector-contrib:latest
    command: ["--config=/etc/otel-collector-config.yaml"]
    volumes:
      - ./observability/otel-collector-config.yaml:/etc/otel-collector-config.yaml
    ports:
      - "4317:4317"   # gRPC
      - "4318:4318"   # HTTP

  # Grafana for visualization
  grafana:
    image: grafana/grafana:latest
    ports:
      - "3001:3000"
    environment:
      - GF_SECURITY_ADMIN_PASSWORD=admin
    volumes:
      - grafana-data:/var/lib/grafana
      - ./observability/grafana/provisioning:/etc/grafana/provisioning
    depends_on:
      - otel-collector

  # Tempo for traces
  tempo:
    image: grafana/tempo:latest
    command: ["-config.file=/etc/tempo.yaml"]
    volumes:
      - ./observability/tempo.yaml:/etc/tempo.yaml
      - tempo-data:/var/tempo
    ports:
      - "3200:3200"   # Tempo API
      - "14268:14268" # Jaeger ingest

volumes:
  routing-state:
  gastown-a-data:
  gastown-b-data:
  mayor-data:
  grafana-data:
  tempo-data:
```

## Sleeptime Formula Seeds

Sleeptime evolves the routing behavior based on observed patterns. Initial formulas:

### 1. Request Classification Accuracy

```
classification_accuracy = correct_routes / total_routes

IF classification_accuracy < 0.9:
  ADD template example to classify.jinja
  INCREASE temperature for classification LLM
```

### 2. Rig Load Balancing

```
rig_load_variance = stddev(pending_work_per_rig)

IF rig_load_variance > threshold:
  ADJUST routing weights toward underloaded rigs
  ADD "load_balance" routing hint
```

### 3. Deduplication Hit Rate

```
dedupe_hit_rate = duplicates_caught / total_requests

IF dedupe_hit_rate > 0.2:
  INVESTIGATE source of duplicates
  CONSIDER extending correlation window
```

### 4. Escalation Patterns

```
escalation_rate = escalations / total_requests

IF escalation_rate > 0.1:
  ANALYZE escalation reasons
  ADD routing rules for common patterns
  REDUCE escalation threshold if false positives
```

### 5. Response Latency

```
p95_latency = percentile(response_times, 95)

IF p95_latency > target:
  CHECK rig health
  CONSIDER parallel dispatch for multicast
  CACHE frequently requested status queries
```

## Implementation Phases

### Phase 1: Core Graph + Local Testing
- [ ] Define RoutingGraph in Haskell
- [ ] Implement handlers with mock RigCall/MayorCall
- [ ] Unit tests for routing logic
- [ ] Local graph execution (no WASM)

### Phase 2: MCP Server Wrapper
- [ ] TypeScript MCP server scaffolding
- [ ] Wire graph handlers to MCP tools
- [ ] Resources for rig status
- [ ] Integration tests with MCP SDK

### Phase 3: WASM + Deploy
- [ ] WASM build configuration
- [ ] Effect interpreters for RigCall/MayorCall
- [ ] Docker image builds
- [ ] Local Docker Compose testing

### Phase 4: Observability
- [ ] OpenTelemetry instrumentation
- [ ] Grafana dashboards
- [ ] Alerting rules
- [ ] Sleeptime log generation

### Phase 5: Multi-Instance Testing
- [ ] Two Gas Town instances coordinating
- [ ] Cross-rig work routing
- [ ] Escalation flows
- [ ] Performance benchmarks

## Open Questions

1. **Session management**: How do MCP clients maintain session state across reconnects?
2. **Authentication**: How do Gas Town instances authenticate to each other?
3. **Async patterns**: Should routing support fire-and-forget with async callbacks?
4. **Circuit breakers**: How to handle rig unavailability gracefully?
5. **Rate limiting**: Per-client or per-rig rate limits?

## References

- [MCP Specification 2025-11-25](https://modelcontextprotocol.io/specification/2025-11-25)
- [MCP TypeScript SDK](https://github.com/modelcontextprotocol/typescript-sdk)
- Tidepool Graph DSL: `tidepool-core/src/Tidepool/Graph/`
- Gas Town architecture: `~/gt/CLAUDE.md`
