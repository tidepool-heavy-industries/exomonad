# Tidepool Control Server

Unix socket server for Claude Code++ integration. Receives hook events from mantle-agent and direct HTTP MCP requests from Claude Code via Unix socket. Provides 7 MCP tools organized in tiers: LSP-only (Tier 1), LLM-enhanced (Tier 2), external orchestration (Tier 3), TUI-interactive (Tier 4), and mailbox communication (Tier 5).

## When to Read This

Read this if you're:
- Understanding the Claude Code++ integration architecture
- Working on hook handling or MCP tool implementation
- Debugging the control server or data flow
- Adding new MCP tools to the system

## Architecture Overview

```
┌────────────────────────────────────────────────────────────────────┐
│ User TTY (Zellij 2-pane layout)                                    │
│  ┌──────────────────────────┐  ┌─────────────────────────────────┐│
│  │ Pane 1: Claude Code      │  │ Pane 2: control-server          ││
│  │ (user interaction)       │  │ (logs, HLS output)              ││
│  └──────────────────────────┘  └─────────────────────────────────┘│
└────────────────────────────────────────────────────────────────────┘
                │                                    ▲
                │ Hooks/MCP                          │ Logs
                ▼                                    │
┌────────────────────────────────────────────────────────────────────┐
│ mantle-agent (Rust)                                                │
│  • hook subcommand: forwards CC hooks → Unix Socket                │
└────────────────────────────────┬───────────────────────────────────┘
                                 │ HTTP over Unix Socket
                                 │ .tidepool/sockets/control.sock
                                 ▼
┌────────────────────────────────────────────────────────────────────┐
│ control-server (Haskell)                                           │
│  • Unix socket: $TIDEPOOL_CONTROL_SOCKET                         │
│  • Protocol: ControlMessage/ControlResponse                        │
│  • Long-lived LSP session (HLS)                                    │
│  • Routes:                                                         │
│    - HookEvent → Handler.Hook (passthrough)                        │
│    - McpToolCall → Handler.MCP (scout tool)                        │
└────────────────┬───────────────────────────────────┬───────────────┘
                 │                                   │
                 ▼                                   ▼
        ┌────────────────┐              ┌────────────────────┐
        │ Hook Handler   │              │ Scout Handler      │
        │ (passthrough)  │              │ (LSP + Gemma)      │
        └────────────────┘              └──────────┬─────────┘
                                                   │
                                 ┌─────────────────┼─────────────────┐
                                 ▼                 ▼                 ▼
                        ┌────────────┐   ┌──────────────┐  ┌───────────┐
                        │ LSP (HLS)  │   │ Gemma Effect │  │ Frontier  │
                        │ workspace/ │   │ (scorer)     │  │ (BFS)     │
                        │ symbol,    │   └──────────────┘  └───────────┘
                        │ hover,     │
                        │ references │
                        └────────────┘
```

## .tidepool Directory Convention

Project-local configuration in `.tidepool/` (gitignored):

```
<project-root>/
├── .tidepool/              ← gitignored
│   ├── control.sock        ← Unix socket for IPC
│   └── config.json         ← Optional: project-specific config (future)
├── .gitignore              ← Contains: .tidepool/
└── ...
```

## Complete Data Flow

### Hybrid Tidepool Architecture

The system uses `process-compose` to orchestrate multiple services. Subagents (parallel worktrees) run a minimal `control-server` in LSP-only mode. Configuration is generated programmatically to ensure type safety and consistency.

| Component | Flag/Logic | Purpose |
|-----------|---------------|---------|
| **Control Server** | `--no-tui` | Disables the TUI sidebar listener (for subagents) |
| **Subagent Config** | `Paths.hs` + `ProcessCompose.hs` | Programmatic type-safe generation of `process-compose.yaml` |

### Subagent Environment & Config Handling

Subagents spawned by `spawn_agents` receive a **custom-generated environment and configuration** instead of relying on external templates or symlinks.

**Why:**
- Programmatic generation ensures the configuration is always valid and consistent with the current binary versions.
- Programmatic path construction (Paths.hs) avoids "stringly-typed" errors and handles OS-specific path limits.
- Merging environments (Haskell source of truth) prevents variable shadowing issues from root `.env` files.

**How it works (SpawnAgents.hs):**
1. **Paths:** Canonical paths for sockets (`/tmp/tidepool-...`) and binaries are constructed via `Tidepool.Control.Runtime.Paths`.
2. **Orchestration:** A `ProcessComposeConfig` Haskell value is constructed and serialized to YAML via `Tidepool.Control.Runtime.ProcessCompose`.
3. **Source of Truth:** The running Haskell process captures its full environment (`getEnvironment`).
4. **Filtering & Merging:** Conflicting keys (socket paths) are filtered from the captured environment, and subagent-specific overrides are merged in.
5. **Deployment:** Self-contained `.env` and `process-compose.yaml` files are written directly to the subagent worktree.
6. **Execution:** `process-compose` simply loads the generated files, ensuring an isolated and correctly configured environment.

### Hook Flow (PreToolUse Example)

```
1. User interaction
   ┌──────────────────────────────────────────────┐
   │ Claude Code: wants to call Write tool        │
   │ Generates hook JSON on stdin                 │
   └─────────────────┬────────────────────────────┘
                     │ stdin JSON
                     ▼
2. mantle-agent hook pre-tool-use
   ┌──────────────────────────────────────────────┐
   │ Read HookInput from stdin                    │
   │ Connect to control server (Unix socket)      │
   │ POST /hook (HTTP over Unix socket)           │
   └─────────────────┬────────────────────────────┘
                     │ HTTP request
                     ▼
3. control-server
   ┌──────────────────────────────────────────────┐
   │ Accept HTTP request on Unix socket           │
   │ Parse hook request body                      │
   │ Route to handleHook                          │
   │   → Log and return allowPreToolUse           │
   │ Send HTTP response                           │
   └─────────────────┬────────────────────────────┘
                     │ HTTP response
                     ▼
4. mantle-agent returns
   ┌──────────────────────────────────────────────┐
   │ Receive HookResponse                         │
   │ Print HookOutput JSON to stdout              │
   │ Exit with exit_code (0 = allow)              │
   └─────────────────┬────────────────────────────┘
                     │ stdout JSON
                     ▼
5. Claude Code processes
   ┌──────────────────────────────────────────────┐
   │ Parse hook response                          │
   │ permissionDecision = "allow"                 │
   │ → Execute Write tool                         │
   └──────────────────────────────────────────────┘
```

### MCP Tool Flow (scout)

```
1. User asks question
   ┌──────────────────────────────────────────────┐
   │ "What breaks if I add a variant to LLMKind?" │
   │ Claude plans to use scout MCP tool           │
   └─────────────────┬────────────────────────────┘
                     │
                     ▼
2. Claude Code calls MCP
   ┌──────────────────────────────────────────────┐
   │ HTTP MCP transport (http+unix://)            │
   │ POST /mcp/call                               │
   │   { name: "scout", arguments: {...} }        │
   └─────────────────┬────────────────────────────┘
                     │ HTTP over Unix socket
                     ▼
3. control-server routes
   ┌──────────────────────────────────────────────┐
   │ Parse ControlMessage::McpToolCall            │
   │ Route to handleMcpTool                       │
   │   case "scout" → handleScoutTool             │
   └─────────────────┬────────────────────────────┘
                     │
                     ▼
4. Scout exploration
   ┌──────────────────────────────────────────────┐
   │ Parse ScoutQuery arguments                   │
   │ Require GEMMA_ENDPOINT env var (no fallback) │
   │ Run: runM $ runGemmaHTTP endpoint $          │
   │      runLSP session $ exploreEff query       │
   │                                              │
   │ Exploration steps:                           │
   │  a) Find entry points (LSP workspace/symbol) │
   │  b) BFS loop:                                │
   │     - Fetch context (LSP hover + file read)  │
   │     - Score edge (Gemma effect via Ollama)   │
   │     - Decide expansion (heuristics)          │
   │     - Queue children (LSP references)        │
   │  c) Collect results                          │
   │     - Top pointers (sorted by score)         │
   │     - Training examples (query+edge+rubric)  │
   └─────────────────┬────────────────────────────┘
                     │ ScoutResponse
                     ▼
6. LSP communication (within exploration)
   ┌──────────────────────────────────────────────┐
   │ runLSP session $ workspaceSymbol "LLMKind"   │
   │   → executeSession (send to worker thread)   │
   │   → Worker in Session monad:                 │
   │      request L.SMethod_WorkspaceSymbol       │
   │   → lsp-test library:                        │
   │      JSON-RPC to HLS via stdio               │
   │   ← HLS response: [SymbolInformation]        │
   │   → Return to caller                         │
   └─────────────────┬────────────────────────────┘
                     │ LSP results
                     ▼
7. Scoring (within exploration)
   ┌──────────────────────────────────────────────┐
   │ rateEdge query edgeCtx                       │
   │   → runGemmaHTTP interpreter:                │
   │      - Format edge context as plain text     │
   │      - POST to Ollama /api/chat with tools   │
   │      - Parse tool_calls[0].function.arguments│
   │   → Returns Rubric from FunctionGemma 270M   │
   │                                              │
   │ Development alternatives (not in production):│
   │   runGemmaStub    - formats prompt, heuristic│
   │   runGemmaHeuristic - pattern-based rules    │
   │   runGemmaMock    - hardcoded rubric         │
   └─────────────────┬────────────────────────────┘
                     │ Rubric
                     ▼
8. Return response
   ┌──────────────────────────────────────────────┐
   │ Build ScoutResponse:                         │
   │  { summary, pointers, nodesVisited,          │
   │    trainingExamples }                        │
   │ Serialize to JSON (toJSON)                   │
   │ Send ControlResponse::McpToolResponse        │
   └─────────────────┬────────────────────────────┘
                     │ TCP response
                     ▼
9. mantle-agent mcp returns
   ┌──────────────────────────────────────────────┐
   │ Receive McpToolResponse                      │
   │ Format as JSON-RPC result                    │
   │ Write to stdout                              │
   └─────────────────┬────────────────────────────┘
                     │ stdout (JSON-RPC)
                     ▼
10. Claude Code processes
   ┌──────────────────────────────────────────────┐
   │ Parse JSON-RPC response                      │
   │ Extract ScoutResponse                        │
   │ Claude analyzes pointers and summary         │
   │ Generates response to user                   │
   └─────────────────┬────────────────────────────┘
                     │
                     ▼
11. User sees answer
   ┌──────────────────────────────────────────────┐
   │ "Based on semantic code exploration,         │
   │  adding a variant to LLMKind will affect     │
   │  15 locations:                               │
   │                                              │
   │  High Risk Locations:                        │
   │  • Types.hs:87 - Exhaustive pattern match    │
   │  • Handler.hs:123 - Pattern match dispatch   │
   │  ..."                                        │
   └──────────────────────────────────────────────┘
```

## Key Modules

| Module | Purpose |
|--------|---------|
| `Server.hs` | TCP listener (port 7432), LSP session management, NDJSON framing |
| `Protocol.hs` | ControlMessage/Response types (matches Rust protocol.rs) |
| `Handler.hs` | Message routing (HookEvent, McpToolCall, ToolsListRequest) |
| `Handler/Hook.hs` | Hook event handler (currently passthrough) |
| `Handler/MCP.hs` | MCP tool router (routes tool calls by name to handlers) |
| `Export.hs` | Automatic MCP tool discovery via `reifyGraphEntries`/`reifyMCPTools` |
| **Tier 1 Tools** | |
| `LSPTools.hs` | Find callers, show fields, show constructors (simplified graphs with `Return` effect + `GraphEntries`) |
| **Tier 2 Tools** | |
| `Scout/Graph.hs` | DocGenGraph definition (teach-graph tool, Entry → Init → Process → Select → Expand → Finalize → Exit) |
| `Scout/Graph/Types.hs` | Node input/output types (ProcessInput, SelectInput, etc.) |
| `Scout/Graph/Handlers.hs` | DocGenGraph handlers (init, process, expand, finalize logic) |
| `Scout/Graph/Runner.hs` | runDocGenGraph (executes graph with teaching/production modes) |
| `Scout/Graph/Templates.hs` | SelectTpl (Jinja template for Haiku symbol selection) |
| `Scout/DocGen.hs` | Legacy BFS loop (pre-graph-DSL implementation, still used for non-graph mode) |
| `Scout/DocGen/Types.hs` | TeachQuery, TeachingDoc, LSPSymbol |
| `Scout/DocGen/Gemma.hs` | ScoutGemma effect + interpreters (HTTP via Ollama) |
| `Scout/DocGen/Teacher.hs` | FineTrainingTeacher instance for teaching mode |

## MCP Tools

Tools are **automatically discovered** from graph definitions at startup via `exportMCPTools`:
- **Tier 1 (LSP-only)**: Use `GraphEntries` type family + `Return` effect, discovered via `reifyGraphEntries`
- **Tier 2 (LLM-enhanced)**: Use `MCPExport` annotation on entry node, discovered via `reifyMCPTools`
- **Tier 3 (External orchestration)**: Beads (BD), git, subprocess operations
- **Tier 4 (TUI-interactive)**: Dialog boxes, option selection, guidance requests
- **Tier 5 (Mailbox)**: Agent-to-agent messaging

**Role-based filtering:** See "Role-Based Tool Filtering" section below for `--tools` flag usage in mantle-agent.

### Tier 1: Deterministic LSP Tools

Pure LSP queries with heuristic filtering. No LLM calls.

#### `find_callers` - Find Actual Call Sites

Finds where a function is actually called, filtering out imports, type signatures, and comments.

**Request Schema:**
```json
{
  "name": "findCallersLogic",          // required
  "context_lines": 2,                  // optional, default 1
  "max_results": 50                    // optional, default 50
}
```

**Implementation:** `LSPTools.hs:156-180` (FindCallersGraph)

#### `show_type` - Inspect Type (Fields + Constructors)

**Status:** Defined but not currently exported (requires LSP session)

Inspects a Haskell type, returning both record fields and constructors. This unified tool replaces the deprecated `show_fields` and `show_constructors` tools.

**Request Schema:**
```json
{
  "type_name": "MyType"                // required
}
```

**Response includes:**
- `type_kind`: "record", "sum", "gadt", or "newtype"
- `fields`: Array of record fields (for record types)
- `constructors`: Array of constructors (for sum types/GADTs)
- `raw_definition`: The full type definition text

**Implementation:** `LSPTools.hs:461-570` (ShowTypeGraph)

### Tier 2: LLM-Enhanced Tools

Tools that use LLM nodes for intelligent filtering/selection.

#### `teach-graph` - Teaching Document Generation

Explores codebase concepts via BFS, using Haiku to select relevant type dependencies. Returns a teaching document with symbols ordered by prerequisites.

**Request Schema:**
```json
{
  "topic": "How the Memory effect works",     // required
  "seeds": ["getMem", "putMem"],              // required: starting symbols
  "budget": 20                                // optional, default 20
}
```

**Response Schema:**
```json
{
  "core": [...],       // depth 0 (seed symbols)
  "prereqs": [...],    // depth 1-2 (dependencies)
  "support": [...]     // depth 3+ (supporting types)
}
```

**Implementation:** `Scout/Graph.hs:125-165` (DocGenGraph)

### Tier 3: External Orchestration Tools (Exo)

Integration with beads (BD) and git for development workflow automation.

#### `exo_status` - Get Development Context

Gets current bead details, git status, and PR info.

**Implementation:** `ExoTools.hs:92-114` (ExoStatusGraph)

### Tier 4: TUI-Interactive Tools

Tools that show interactive UI elements in the TUI sidebar and wait for user response.

#### `confirm_action` - Show Confirmation Dialog

Shows a confirmation dialog to the user for a potentially destructive or important action.

**Request Schema:**
```json
{
  "action": "Delete 15 files",          // required
  "details": "This will permanently remove the files from disk." // required
}
```

**Implementation:** `TUITools.hs:96-123` (ConfirmActionGraph)

#### `select_option` - Select from List

Asks the user to select from a list of predefined options, or provide a custom response.

**Request Schema:**
```json
{
  "prompt": "Select the next step",     // required
  "options": [["1", "Fix bug"], ["2", "Add test"]] // required: [[id, label], ...]
}
```

**Implementation:** `TUITools.hs:176-209` (SelectOptionGraph)

#### `request_guidance` - Ask for Human Guidance

Asks the user for free-form guidance or to select from suggestions when the agent is stuck.

**Request Schema:**
```json
{
  "context": "I am unsure how to implement the memory effect.", // required
  "suggestions": ["Use a Map", "Use a State effect"]           // optional
}
```

**Implementation:** `TUITools.hs:258-290` (RequestGuidanceGraph)

### Tier 4 (continued): Project Management Dashboard Tools

Tools for sprint planning, issue triage, and team coordination via Beads (BD) and GitHub APIs. These are PM-specific tools that use `MCPExport` annotation pattern (unlike the TUI tools above which use `GraphEntries`).

#### `pm_status` - Sprint Health Dashboard

Provides PM observability: velocity, cycle time, PR lag, and current state distribution.

**Request Schema:**
```json
{
  "period_days": 7,                // optional, default 7
  "include_breakdown": false,      // optional, include label breakdown
  "label_track": "backend",        // optional, filter by track label
  "repo": "owner/repo"             // optional, GitHub repo for PR metrics
}
```

**Response Schema:**
```json
{
  "velocity": 4.2,
  "trend": 0.15,
  "cycle_time": {
    "median_days": 2.5,
    "p90_days": 5.8
  },
  "current_state": {
    "in_flight": 3,
    "ready": 5,
    "blocked": 2,
    "needs_tl_review": 1,
    "needs_pm_approval": 0
  },
  "pr_lag": {
    "median_hours": 4.2,
    "p90_hours": 12.0
  },
  "breakdown": [["backend", 5], ["frontend", 3]]  // if include_breakdown=true
}
```

**Implementation:** `PMStatus.hs:169-223` (pmStatusLogic)

#### `pm_review_dag` - DAG Analysis for Strategic Planning

Analyzes the bead dependency graph to identify critical paths, priority gaps, and blocking cascades.

**Request Schema:**
```json
{
  "include_closed": false,           // optional, include closed beads
  "aging_threshold_hours": 24,       // optional, default 24h
  "focus_track": "backend"           // optional, focus on a track label
}
```

**Response Schema:**
```json
{
  "ready": ["tidepool-abc", "tidepool-def"],
  "blocked": [
    {
      "bead_id": "tidepool-xyz",
      "depth": 2,
      "blockers": ["tidepool-123", "tidepool-456"]
    }
  ],
  "critical_path": ["tidepool-end", "tidepool-mid", "tidepool-start"],
  "priority_gaps": [
    {
      "blocked_id": "tidepool-low",
      "blocked_priority": 4,
      "blocker_id": "tidepool-high",
      "blocker_priority": 0
    }
  ],
  "aging": ["tidepool-stale"],
  "needs_tl_review": ["tidepool-123"],
  "needs_pm_approval": ["tidepool-456"]
}
```

**Implementation:** `PMReviewDAG.hs:135-246` (pmReviewDagLogic)

### Tool Registration

**Automatic discovery (23+ tools):**
```haskell
-- Export.hs:940-990
exportMCPTools :: Logger -> IO [ToolDefinition]
exportMCPTools logger = do
  -- Tier 1: Simplified LSP graphs
  let fcTools = reifyGraphEntries (Proxy @FindCallersGraph)
      sfTools = reifyGraphEntries (Proxy @ShowFieldsGraph)
      scTools = reifyGraphEntries (Proxy @ShowConstructorsGraph)

  -- Tier 2: LLM-enhanced graphs
  let dgTools = reifyMCPTools (Proxy @DocGenGraph)  -- teach-graph

  -- Tier 3: External orchestration (Beads + Git)
  let esTools = reifyMCPTools (Proxy @ExoStatusGraph)
      ecTools = reifyMCPTools (Proxy @ExoCompleteGraph)
      saTools = reifyMCPTools (Proxy @SpawnAgentsGraph)
      fpTools = reifyMCPTools (Proxy @FilePRGraph)
      pmPriTools = reifyMCPTools (Proxy @PmPrioritizeGraph)
      paeTools = reifyMCPTools (Proxy @PmApproveExpansionGraph)

  -- Tier 4: TUI-interactive + PM dashboard tools
  let caTools = reifyGraphEntries (Proxy @ConfirmActionGraph)
      soTools = reifyGraphEntries (Proxy @SelectOptionGraph)
      rgTools = reifyGraphEntries (Proxy @RequestGuidanceGraph)
      pmStatTools = reifyMCPTools (Proxy @PmStatusGraph)
      pmRevTools = reifyMCPTools (Proxy @PmReviewDagGraph)
      pmProTools = reifyMCPTools (Proxy @PMProposeGraph)

  -- Tier 5: Mailbox communication + Feedback
  let smTools = reifyMCPTools (Proxy @SendMessageGraph)
      ciTools = reifyMCPTools (Proxy @CheckInboxGraph)
      rmTools = reifyMCPTools (Proxy @ReadMessageGraph)
      mrTools = reifyMCPTools (Proxy @MarkReadGraph)
      rfTools = reifyMCPTools (Proxy @RegisterFeedbackGraph)
      prTools = reifyMCPTools (Proxy @PrReviewStatusGraph)

  let allTools = concat [fcTools, sfTools, scTools, caTools, soTools, rgTools,
                         dgTools, esTools, ecTools, erTools, saTools, fpTools,
                         paeTools, pmPriTools, pmStatTools, pmRevTools, pmProTools,
                         smTools, ciTools, rmTools, mrTools, rfTools, prTools]
  pure $ map reifyToToolDef allTools
```

**How it works:**
1. **Simplified pattern**: `GraphEntries` type family declares entry points, `reifyGraphEntries` extracts metadata
2. **Legacy pattern**: `MCPExport` annotation marks entry node, `reifyMCPTools` extracts metadata
3. `exportMCPTools` called on control-server startup
4. mantle-agent queries at connection, caches tools
5. **Role-based filtering:** mantle-agent uses `--tools` flag to restrict which tools are exposed to Claude

## Hook Handlers

Hook events are processed by `Handler/Hook.hs`.

**PreToolUse Policy:**
- `PreToolUse` hooks are evaluated against a policy defined in `.tidepool/hook-policy.json`.
- Policy supports `PolicyAllow`, `PolicyDeny`, and `PolicyAsk` decisions.
- Rules are evaluated in order; the first match wins.
- Supports literal tool name matches or `*` wildcard.

**Example Policy (.tidepool/hook-policy.json):**
```json
{
  "rules": [
    { "toolPattern": "Write", "decision": { "tag": "PolicyDeny", "contents": "Writing is disabled" } }
  ],
  "defaultDecision": { "tag": "PolicyAllow", "contents": ["Allowed by default", null] }
}
```

**Other Hooks:**
- `SessionStart` → Injects bead context into the conversation.
- `Stop` → Enforces PR filing and pre-commit checks with templated guidance.
- `PostToolUse` → allow with no additional context.
- Other hooks → continue with default response.

**Implementation:** `Handler/Hook.hs`, `Hook/Policy.hs`

## Role-Based Tool Filtering

The `--tools` flag in mantle-agent enables different roles (PM, TL, etc.) to connect to the same control server with different tool visibility.

**How it works:**

1. **Control server exposes all 23+ tools** by default via `exportMCPTools`
2. **mantle-agent mcp --tools <TOOL1>,<TOOL2>,...** filters at the MCP protocol level
3. **On `tools/list` request**: Returns only allowlisted tools
4. **On `tools/call` request**: Rejects calls to non-allowlisted tools with clear error
5. **Without `--tools` flag**: All tools are exposed (backwards compatible)

**PM Role Example:**
```bash
mantle-agent mcp --tools pm_propose,pm_approve_expansion,pm_prioritize,pm_status,pm_review_dag,exo_status
```

PM users see:
- `pm_propose` - Propose new beads
- `pm_approve_expansion` - Approve/reject expansion plans
- `pm_prioritize` - Batch prioritize beads
- `pm_status` - Sprint health dashboard
- `pm_review_dag` - DAG analysis for strategic planning
- `exo_status` - Get development context

**TL Role Example:**
```bash
mantle-agent mcp  # Omit --tools for full access, or specify TL-specific tools
```

TL users see: All tools (full access), or customize with:
- `find_callers`, `show_type` - LSP tools (when enabled)
- `teach-graph` - Code exploration
- `spawn_agents`, `exo_*`, `file_pr` - Development workflow

**Developer Example (no filtering):**
```bash
mantle-agent mcp  # No --tools flag: access all 23+ tools
```

**Implementation:**
- **Control server side**: `exportMCPTools` returns full tool list; `Handler/MCP.hs` routes calls by name
- **mantle-agent side**: `mcp.rs:handle_tools_list()` filters based on `--tools` allowlist; `handle_tools_call()` rejects non-allowlisted tools

## Protocol Types

### ControlMessage (Rust → Haskell)

```haskell
data ControlMessage
  = HookEvent { input :: HookInput }
  | McpToolCall { mcpId :: Text, toolName :: Text, arguments :: Value }
```

### ControlResponse (Haskell → Rust)

```haskell
data ControlResponse
  = HookResponse { output :: HookOutput, exitCode :: Int }
  | McpToolResponse { mcpId :: Text, result :: Maybe Value, mcpError :: Maybe McpError }
```

**IMPORTANT:** Types must serialize identically to Rust `mantle_shared::protocol` types.

## LSP Session Management

The control server maintains a **long-lived LSP session** via `withLSPSession`:

```haskell
main :: IO ()
main = do
  config <- readServerConfig
  withLSPSession config.projectDir $ \lspSession -> do
    bracket (setupSocket) (closeSocket) $ \sock -> do
      forever $ do
        (conn, _) <- accept sock
        forkIO $ handleConnection lspSession conn
```

**Benefits:**
- Amortizes HLS startup cost (~5s) across many queries
- Thread-safe via Chan + worker pattern (multiple concurrent queries)
- Shared workspace index (HLS only indexes once)

## Ollama Setup (Required)

The scout tool requires Ollama running FunctionGemma 270M for semantic scoring.

**Why Ollama?** mistralrs doesn't support Gemma 3 architecture (`Unknown GGUF architecture 'gemma3'`). Ollama has native FunctionGemma support with automatic tool translation.

```bash
# Install Ollama (macOS)
brew install ollama

# Pull FunctionGemma 270M model (~300MB)
ollama pull functiongemma:270m

# Start Ollama server (runs on port 11434 by default)
ollama serve
```

**Verify Ollama is running:**
```bash
curl http://localhost:11434/api/tags
# Should show: {"models":[{"name":"functiongemma:270m",...}]}
```

**API format:** Ollama auto-translates OpenAI-style `tools` array → FunctionGemma's `<start_function_declaration>` format. Response is in `message.tool_calls[0].function.arguments`.

## Running

### Hybrid Tidepool (Recommended)
```bash
cd /path/to/tidepool
./start-augmented.sh
```

Launches control-server via process-compose with:
- Unix socket health check for robust readiness
- Automatic dependency management (tui-sidebar waits for health)
- Centralized logging to `.tidepool/logs/`
- Automatic restart on failure

### Standalone (Development)
```bash
# Start in project directory
cd /path/to/your/project

# GEMMA_ENDPOINT is REQUIRED (no heuristic fallback)
GEMMA_ENDPOINT=http://localhost:11434 cabal run tidepool-control-server

# Or set project directory via environment
TIDEPOOL_PROJECT_DIR=/path/to/project GEMMA_ENDPOINT=http://localhost:11434 cabal run tidepool-control-server
```

### Health Check
The server supports a ping-pong protocol over Unix socket for health checks:
```bash
# Check if control-server is ready using mantle-agent
./scripts/health-check.sh
```

**Used by process-compose:**
The `control-server` readiness probe executes `mantle-agent health`, which sends a `Ping` message to `$TIDEPOOL_CONTROL_SOCKET` and waits for a `Pong`.

```bash
# Verify server is listening
Control server listening on Unix socket: /path/to/control.sock
```


**Server logs to stdout:**
```
Created .tidepool directory at ./.tidepool
Starting LSP session for project: .
[LSP] Session started, HLS initialized
LSP session initialized
Control server listening on Unix socket: .tidepool/sockets/control.sock
Connection received
[MCP] tool=teach-graph
  topic=how Memory effect works
  seeds=getMem,putMem
  gemma=http://localhost:11434
[Gemma] HTTP call to http://localhost:11434 for: /path/to/Memory.hs:42
[Gemma] -> relevance=5, depth=2
  found 8 symbols
[MCP] -> success
```

## Testing

```bash
# Test tool discovery
echo '{"type":"ToolsListRequest"}' | nc localhost 7432
# Returns available tools (LSP tools disabled by default)

# Test exo_status tool
echo '{"type":"MCPToolCall","id":"1","tool_name":"exo_status","arguments":{}}' | nc localhost 7432
```

## Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `TIDEPOOL_PROJECT_DIR` | Current directory | Project root (where .tidepool/ lives) |
| `GEMMA_ENDPOINT` | **Required** | Ollama endpoint (e.g., `http://localhost:11434`) |
| `MANTLE_CONTROL_HOST` | (set by mantle-agent) | TCP host (unused by server, only client) |
| `MANTLE_CONTROL_PORT` | (set by mantle-agent) | TCP port (unused by server, only client) |

**Note:** `GEMMA_ENDPOINT` must be set. The scout tool will fail with an error if not configured. No heuristic fallback.

## Integration with Claude Code

### Configuration (.claude/settings.local.json)

```json
{
  "hooks": {
    "PreToolUse": "mantle-agent hook pre-tool-use"
  },
  "mcpServers": {
    "tidepool": {
      "command": "mantle-agent",
      "args": ["mcp"],
      "env": {
        "MANTLE_CONTROL_HOST": "127.0.0.1",
        "MANTLE_CONTROL_PORT": "7432"
      }
    }
  }
}
```

### Gemini CLI Support

The control server now includes support for Gemini CLI config generation via `writeGeminiConfig` (SpawnAgents.hs:476).

**Key differences from Claude:**
- Config location: `.gemini/settings.json` (NOT `.gemini/settings.local.json`)
- Combines hooks AND MCP in single file (unlike Claude which uses separate `.mcp.json`)
- Hooks require explicit enable: `"hooksConfig": {"enabled": true}`
- Path variable: `$GEMINI_PROJECT_DIR` (not `$CLAUDE_PROJECT_DIR`)
- API key: `GEMINI_API_KEY` (not `ANTHROPIC_API_KEY`)
- CLI command: Just `gemini` (no `--debug --verbose` flags)
- MCP command: Uses full path `$GEMINI_PROJECT_DIR/../runtime/bin/mantle-agent` for consistency with hooks

**Status:** Implemented and wired into spawn_agents backend selection (pass `backend: "gemini"` parameter).

**Note:** `.gemini/` and `.claude/` are automatically added to worktree `.gitignore` during bootstrap. Both backends are gitignored for consistency.

### Workflow

1. **Start control-server** in project directory (pane 2)
2. **Start Claude Code** in same directory (pane 1)
3. **Claude calls MCP tool** (teach-graph, find_callers, etc.)
4. **mantle-agent forwards** via TCP to control-server
5. **control-server executes** tool logic (LSP queries, graph execution)
6. **Returns result** → Claude uses in response

## Structured Error Responses

All MCP tools return structured errors instead of plain text strings. This allows Claude to categorize failures and provide targeted guidance.

### Error Codes

| Code | Name | Meaning | Example |
|------|------|---------|---------|
| -32001 | NotFound | Resource does not exist | Bead not found, binary missing, symbol not found |
| -32002 | InvalidInput | Arguments failed validation | Invalid bead ID, empty env vars, malformed JSON |
| -32003 | ExternalFailure | Subprocess or I/O error | File system error, git command failure |
| -32004 | StateError | Invalid operation state | Bead is blocked, worktree already exists |
| -32005 | EnvironmentError | Missing configuration | Not in Zellij, missing API key |

### Error Response Format

```json
{
  "code": -32001,
  "message": "Bead tidepool-xyz not found",
  "details": {
    "searched_in": ".beads/beads.jsonl",
    "available_beads": ["tidepool-m1j", "tidepool-abc"]
  },
  "suggestion": "Use 'bd list' to see all available beads"
}
```

**Fields:**
- `code` (required): Numeric error code identifying error category
- `message` (required): Human-readable explanation of what went wrong
- `details` (optional): Structured data for debugging (varies by tool)
- `suggestion` (optional): Actionable guidance to resolve the issue

### Benefits

- **Type Categorization**: Claude can distinguish between validation errors (user's fault), not-found errors (resource missing), and external failures (system issue)
- **Consistency**: All 26+ MCP tools use the same error format
- **Actionable**: Suggestions help Claude guide users toward solutions
- **Debuggable**: Details field provides context for troubleshooting
- **Backward Compatible**: Existing clients still work (errors serialize to JSON-RPC format)

### Implementation

Error responses are built using helper functions:
- `mcpToolError reqId code message` - Basic error with code, message
- `mcpToolErrorWithDetails reqId code message details suggestion` - Full error with all fields

Example from spawn_agents tool:
```haskell
-- Validation error
pure $ mcpToolError reqId InvalidInput $ "Invalid bead ID: contains path separators"

-- Not found error with suggestion
pure $ mcpToolErrorWithDetails reqId NotFound
  "Bead not found"
  (Just (object ["bead_id" .= beadId]))
  (Just "Use 'bd list' to find available beads")
```

## Related Documentation

- **[ADR-003: MCP Tool Design Patterns](../../docs/architecture/ADR-003-MCP-Tool-Design-Patterns.md)** ⭐ - Tool tier architecture, role-based filtering design decisions
- **[rust/mantle-agent/CLAUDE.md](../../rust/mantle-agent/CLAUDE.md)** - Hook/MCP forwarding, `--tools` flag usage
- **[rust/mantle-shared/CLAUDE.md](../../rust/mantle-shared/CLAUDE.md)** - Protocol types (Rust side)
- **[haskell/effects/lsp-interpreter/CLAUDE.md](../effects/lsp-interpreter/CLAUDE.md)** - LSP integration
- **[haskell/tools/training-generator/CLAUDE.md](../tools/training-generator/CLAUDE.md)** - Training data format
- **[haskell/agents/semantic-scout/CLAUDE.md](../agents/semantic-scout/CLAUDE.md)** - Scout implementation details
- **[Root CLAUDE.md](../../CLAUDE.md)** - Project overview

## Next Steps

1. Wire real hook logic (currently passthrough)
2. Add more MCP tools via Graph DSL (beads task tracking, git operations, etc.)
3. Add metrics collection to mantle-hub
4. Test teach-graph end-to-end with Claude inside Claude Code
5. Generate training data for FunctionGemma fine-tuning via teaching mode

## Completed

✅ **MCP Tool Infrastructure**
- Automatic tool discovery via `MCPExport` + `reifyMCPTools` + `GraphEntries`
- 23+ tools across 5 tiers: LSP, LLM-enhanced, external orchestration, TUI-interactive, mailbox
- Type-safe schema generation from `HasJSONSchema` instances
- Role-based filtering via mantle-agent `--tools` flag

✅ **Tier 1 Tools (Logic-only)**
- FindCallersGraph, ShowFieldsGraph, ShowConstructorsGraph
- LSP integration (workspace/symbol, hover, references)
- Heuristic filtering (imports, type sigs, comments)

✅ **Tier 2 Tools (LLM-enhanced)**
- DocGenGraph with MCPExport annotation
- BFS exploration with Haiku symbol selection
- Teaching document generation with depth-ordered symbols

✅ **Infrastructure**
- FunctionGemma HTTP interpreter via Ollama (`Scout/DocGen/Gemma.hs`)
- Long-lived LSP session management
- TCP protocol (NDJSON) between mantle-agent ↔ control-server

✅ **Error Handling**
- Structured error responses with error codes (NotFound, InvalidInput, ExternalFailure, StateError, EnvironmentError)
- All 26+ MCP tools return consistent error format
- Optional details and suggestion fields for debugging and guidance
- Full documentation in Protocol.hs with examples
