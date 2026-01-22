# ADR-003: MCP Tool Design Patterns

**Status:** Accepted
**Date:** 2026-01-22
**Deciders:** Tidepool Core Team
**Affected:** control-server, mantle-agent, Schema-shaped cognition

## Context

Tidepool exposes 23+ MCP tools to Claude Code++. As the tool count grows (LSP utilities, LLM-enhanced exploration, project management, agent mailbox), we need:

1. **Scalable patterns** for adding new tools without boilerplate
2. **Type safety** at compile time (invalid schemas caught early)
3. **Tier-based organization** (LSP-only vs LLM-enhanced vs interactive)
4. **Role-based filtering** (PM sees PM tools, TL sees dev tools)
5. **Auto-discovery** (tools registered via type-level reflection, not manual lists)

## Decision

### Tiered Tool Architecture

Organize tools into 5 tiers based on dependencies and interaction model:

#### Tier 1: LSP-Only (Deterministic)
- **Dependencies:** LSP session only
- **Pattern:** `GraphEntries` type family + `Return` effect
- **Discovery:** `reifyGraphEntries` at startup
- **Tools:** `find_callers`, `show_fields`, `show_constructors`
- **Rationale:** Fast, no LLM calls, pure data queries via LSP hover/references

#### Tier 2: LLM-Enhanced (Semantic)
- **Dependencies:** LSP + LLM (Haiku or Gemma)
- **Pattern:** `MCPExport` annotation on entry node
- **Discovery:** `reifyMCPTools` at startup
- **Tools:** `teach-graph`
- **Rationale:** BFS exploration with intelligent symbol selection, produce teaching docs

#### Tier 3: External Orchestration (Beads + Git)
- **Dependencies:** Beads (BD), git, subprocess
- **Pattern:** `MCPExport` annotation
- **Discovery:** `reifyMCPTools` at startup
- **Tools:** `spawn_agents`, `exo_status`, `exo_complete`, `exo_reconstitute`, `file_pr`, `pm_prioritize`, `pm_approve_expansion`
- **Rationale:** Workflow automation, cross-worktree coordination

#### Tier 4: TUI-Interactive (User Decision)
- **Dependencies:** Beads + TUI sidebar (interactive)
- **Pattern:** `GraphEntries` type family + TUI effect
- **Discovery:** `reifyGraphEntries` at startup
- **Tools:** `confirm_action`, `select_option`, `request_guidance`, `pm_status`, `pm_review_dag`, `pm_propose`
- **Rationale:** User provides input via sidebar (blocking call), tools render dialog boxes

#### Tier 5: Mailbox (Agent-to-Agent)
- **Dependencies:** Beads + mailbox effects
- **Pattern:** `MCPExport` annotation
- **Discovery:** `reifyMCPTools` at startup
- **Tools:** `send_message`, `check_inbox`, `read_message`, `mark_read`
- **Rationale:** Agents can message each other (e.g., subagent -> PM -> TL)

### Implementation Patterns

#### Pattern A: Simplified Graph (Tier 1 LSP Tools)
```haskell
data FindCallersGraph mode = FindCallersGraph
  { entry :: mode :- EntryNode FindCallersArgs
      :@ MCPExport
      :@ MCPToolDef '("find_callers", "Find actual call sites of a function")
  , run :: mode :- LogicNode
      :@ Input FindCallersArgs
      :@ UsesEffects '[LSP, Goto Exit FindCallersResult]
  , exit :: mode :- ExitNode FindCallersResult
  }
  deriving Generic

-- Discovery: reifyGraphEntries (Proxy @FindCallersGraph)
```

**Advantages:**
- Minimal boilerplate
- `GraphEntries` type family handles endpoint extraction
- No state machine (pure LSP queries + Return)

#### Pattern B: Legacy Graph with MCPExport (Tier 2-5)
```haskell
data TeachGraphGraph mode = TeachGraphGraph
  { entry :: mode :- EntryNode TeachQuery
      :@ MCPExport
      :@ MCPToolDef '("teach-graph", "Generate teaching document via semantic exploration")
  , init :: mode :- LogicNode
      :@ Input TeachQuery
      :@ UsesEffects '[LSP, State TeachState, ...]
  , process :: mode :- LogicNode
      :@ Input ProcessInput
      :@ UsesEffects '[LSP, State TeachState, LLM, ...]
  , finalize :: mode :- LogicNode
      :@ Input FinalizeInput
      :@ UsesEffects '[State TeachState, Goto Exit TeachingDoc]
  }
  deriving Generic

-- Discovery: reifyMCPTools (Proxy @TeachGraphGraph)
-- Annotation: MCPExport on entry node marks it as MCP-callable
```

**Advantages:**
- Complex workflows with state machines
- Multi-node graphs with different effect sets per node
- Supports LLM calls, file I/O, external APIs

### Role-Based Tool Filtering

The `--tools` flag in mantle-agent enables different roles:

```bash
# PM role: project management + status only
mantle-agent mcp --tools pm_propose,pm_approve_expansion,pm_prioritize,pm_status,pm_review_dag,exo_status

# TL role: dev tools + orchestration (or omit --tools for all)
mantle-agent mcp --tools find_callers,show_fields,show_constructors,teach-graph,spawn_agents,exo_*,file_pr

# Developer: all tools (default)
mantle-agent mcp  # No --tools flag
```

**Implementation:**
- **Control server:** `exportMCPTools` returns all 23+ tools
- **mantle-agent:** `mcp.rs:handle_tools_list()` filters by allowlist
- **Error handling:** `tools/call` rejects non-allowlisted tools with clear error code

**Backward compatibility:** If `--tools` is omitted, all tools are exposed.

### Schema-Shaped Cognition Integration

Tools follow a **schema-shaped** design where input/output types are:
1. Defined as Haskell records
2. Derive `HasJSONSchema` instances (via `objectSchema`)
3. Auto-converted to JSON Schema for MCP protocol
4. Claude reads schema and shapes its prompts accordingly

**Example (pm_status):**
```haskell
data PmStatusArgs = PmStatusArgs
  { psaPeriodDays       :: Int         -- described: "Period in days for velocity calculation"
  , psaIncludeBreakdown :: Bool        -- described: "Whether to include label breakdown"
  , psaLabelTrack       :: Maybe Text  -- described: "Filter by track label"
  }
  deriving stock Generic

instance HasJSONSchema PmStatusArgs where
  jsonSchema = objectSchema
    [ ("period_days", describeField "period_days" "..." (emptySchema TInteger))
    , ("include_breakdown", describeField "include_breakdown" "..." (emptySchema TBoolean))
    , ("label_track", describeField "label_track" "..." (emptySchema TString))
    ]
    []
```

Claude sees:
```json
{
  "type": "object",
  "properties": {
    "period_days": { "type": "integer", "description": "Period in days for velocity..." },
    "include_breakdown": { "type": "boolean", "description": "..." },
    "label_track": { "type": ["string", "null"], "description": "..." }
  },
  "required": []
}
```

**Rationale:** Tight coupling of types + schemas ensures consistency. Claude uses schema to generate correct arguments without trial-and-error.

### Auto-Discovery at Startup

`exportMCPTools` (Export.hs:940-990) automatically discovers all tools:

```haskell
exportMCPTools :: Logger -> IO [ToolDefinition]
exportMCPTools logger = do
  -- Tier 1: LSP-only
  let fcTools = reifyGraphEntries (Proxy @FindCallersGraph)

  -- Tier 2: LLM-enhanced
  let dgTools = reifyMCPTools (Proxy @DocGenGraph)

  -- Tier 3-4: Orchestration + PM
  let esTools = reifyMCPTools (Proxy @ExoStatusGraph)
  let pmStatTools = reifyMCPTools (Proxy @PmStatusGraph)

  -- Tier 5: Mailbox
  let smTools = reifyMCPTools (Proxy @SendMessageGraph)

  let allTools = concat [fcTools, dgTools, esTools, pmStatTools, smTools, ...]
  pure $ map reifyToToolDef allTools
```

**Why type-level reflection:**
- No manual registration lists to keep in sync
- Compile error if tool missing or wrongly typed
- Adding a new graph = adding one line + one reify call

## Rationale

### Why Tiers?

Different tools have different trade-offs:
- **Tier 1 (LSP-only):** Instant, deterministic, works offline
- **Tier 2 (LLM-enhanced):** Slower, semantic, intelligent filtering
- **Tier 3 (External orchestration):** Side effects (git, file I/O), coordination
- **Tier 4 (TUI-interactive):** Blocking user interaction, modal dialogs
- **Tier 5 (Mailbox):** Async, agent-to-agent coordination

Tiers clarify **dependency boundaries** and help PMs choose tools for their role.

### Why GraphEntries vs MCPExport?

**GraphEntries (Tier 1):**
- Minimal: entry point + return value, no internal state
- Fast discovery via type family lookup
- Good for: LSP queries, PM dashboards

**MCPExport (Tier 2-5):**
- Flexible: multi-node state machines
- Slower discovery (needs reflection), but fine at startup
- Good for: complex workflows (LLM calls, file I/O)

Both patterns coexist: use GraphEntries for simple tools, MCPExport for complex workflows.

### Why Role-Based Filtering?

**Problem:** PMs don't need `find_callers` or `teach-graph`. Giving them all tools creates cognitive load and accidents (e.g., accidentally calling a dev tool).

**Solution:** `--tools` flag at mantle-agent level provides clean role separation:
- Filtering is **stateless** (no database, just string matching)
- Backward compatible (omitting `--tools` exposes all tools)
- Easy to extend: add new role by passing new `--tools` list

## Consequences

### Positive

1. **Scalable:** Adding new tools requires only:
   - Define graph type
   - Add `reifyGraphEntries` or `reifyMCPTools` line
   - Tools auto-discover at startup

2. **Type-safe:** Invalid schemas caught at compile time, not runtime

3. **Role-aware:** PMs/TLs see only relevant tools via `--tools` flag

4. **Schema-shaped cognition:** Claude uses input/output types to generate correct calls

5. **Deterministic:** No hallucination of tool arguments

### Negative

1. **Complexity:** Two discovery patterns (GraphEntries vs MCPExport) requires learning

2. **Type-level metaprogramming:** Reify functions are opaque; errors can be cryptic

3. **No runtime tool registration:** Adding new tools requires recompilation (acceptable trade-off for type safety)

## Alternatives Considered

### A. Single pattern for all tools
**Rejected:** GraphEntries too simple for complex workflows, MCPExport too verbose for simple LSP queries.

### B. Dynamic tool registration at startup
**Rejected:** Loses type safety; tools could be invalid or missing at runtime.

### C. Manual registration list
**Rejected:** Requires keeping list in sync; easy to forget new tools.

## Related Decisions

- **ADR-002: Schema-Shaped Cognition** - Tools use `HasJSONSchema` for compile-time validation
- **ADR-001: (hypothetical)** - Graph DSL fundamentals

## Implementation Checklist

- [x] Tier 1: `find_callers`, `show_fields`, `show_constructors` via GraphEntries
- [x] Tier 2: `teach-graph` via MCPExport
- [x] Tier 3: `spawn_agents`, `exo_*`, `file_pr`, PM workflow tools via MCPExport
- [x] Tier 4: `confirm_action`, `select_option`, `request_guidance` via GraphEntries; PM dashboard tools via MCPExport
- [x] Tier 5: `send_message`, `check_inbox`, etc. via MCPExport
- [x] Role-based filtering via `--tools` flag in mantle-agent
- [x] Auto-discovery via `exportMCPTools` with logging

## References

- `haskell/control-server/CLAUDE.md` - Tool implementation details
- `haskell/dsl/core/CLAUDE.md` - Graph DSL reference
- `rust/mantle-agent/CLAUDE.md` - Role-based filtering
- `haskell/control-server/src/Tidepool/Control/Export.hs:940-990` - Tool discovery code
- `haskell/control-server/src/Tidepool/Control/Protocol.hs` - Error codes + structured responses
