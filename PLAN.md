# Spawn Template Design Space

Seven options for structuring Gemini teammate prompts. Each trades off token cost, TL effort, agent autonomy, and correctness.

## 1. Structured Fields in Tool Schema

Tool args replace freeform `prompt` with typed fields:

```json
{
  "name": "topology-plumbing",
  "read_first": ["proto/effects/agent.proto", "rust/.../agent.rs"],
  "steps": [
    {"action": "Add enum WorkspaceTopology to agent.proto", "snippet": "enum WorkspaceTopology {...}"}
  ],
  "verify": ["cargo check --workspace", "cargo test --workspace"],
  "done_criteria": ["Both proto copies byte-identical", "All tests pass"],
  "no_commit": true
}
```

WASM tool handler assembles the prompt from fields. Standard scaffolding (PROTOC env, nix develop prefix, "send me a note when done") injected automatically.

**Dispatch model:** TL does full decomposition upfront. Agent is a pure executor. Highest correctness, highest TL effort per spawn. Best for cross-cutting changes where precision matters.

## 2. Template Library (Named Patterns)

Predefined templates in `.exomonad/templates/spawn/` — `proto-change.md`, `rust-service.md`, `haskell-tool.md`, etc. Each has slots:

```json
{
  "name": "topology-plumbing",
  "template": "proto-change",
  "vars": {
    "proto_file": "effects/agent.proto",
    "new_types": "enum WorkspaceTopology { ... }",
    "affected_messages": ["SpawnRequest", "AgentInfo"],
    "haskell_modules": ["Effects/Agent.hs"]
  }
}
```

Template knows: both proto copies must match, proto-gen must run, verify commands for proto changes. TL only provides the semantic delta.

**Dispatch model:** TL picks a pattern and fills slots. Templates encode institutional knowledge ("proto changes always need vendor copy sync"). Good for recurring task shapes — proto changes, new tools, new effects. Bad for novel work.

## 3. Two-Phase: Plan Artifact → Spawn

TL writes a plan (markdown or structured), persisted as `.exomonad/plans/{name}.md`. Spawn references the plan:

```json
{
  "name": "topology-plumbing",
  "plan": ".exomonad/plans/topology.md",
  "assigned_section": "Wave 1: Proto Changes"
}
```

Tool injects the plan section as context + standard verify/boundary sections. Plan is a first-class artifact that survives agent death — respawn picks up where the last agent failed.

**Dispatch model:** Decouples planning from execution. TL can plan a whole wave, then spawn agents for each section in parallel. Plans are reviewable before any agent touches code. Best for multi-agent coordinated work. Adds a persistence layer.

## 4. Prompt Compiler (Enrichment)

TL writes natural language task description. WASM analyzes mentioned file paths and auto-injects:
- Relevant CLAUDE.md pointers (based on directory)
- Build/verify commands (cargo for rust/, cabal for haskell/, both for proto/)
- Env vars (PROTOC path when proto files mentioned)
- Standard boundary rules (no commit, note when done)

```json
{
  "name": "topology-plumbing",
  "task": "Add WorkspaceTopology enum to proto/effects/agent.proto. Add topology field to SpawnRequest, SpawnBatchRequest, SpawnGeminiTeammateRequest, AgentInfo. Regenerate. Plumb defaults in Haskell tools."
}
```

WASM parses file paths from task text, enriches with boilerplate.

**Dispatch model:** Lowest TL effort. Agent gets structured context without TL writing it. Risk: enrichment heuristics might inject wrong things. Works well for experienced TLs who write precise natural language. Fails when task description is vague.

## 5. Role-Scoped Defaults

Each file ownership zone has a registered default context. Spawn specifies which zones the task touches:

```json
{
  "name": "topology-plumbing",
  "zones": ["proto", "haskell-wasm", "rust-core"],
  "task": "Add WorkspaceTopology enum...",
  "extra_files": ["rust/exomonad-core/src/handlers/agent.rs"]
}
```

Zone registry (in WASM config):
- `proto` → read proto/CLAUDE.md, verify: `just proto-gen`, both copies must match
- `rust-core` → read rust/CLAUDE.md, verify: `cargo check`, `cargo test`
- `haskell-wasm` → read haskell/CLAUDE.md, verify: `cabal build wasm-guest`, `just wasm-all`

Zones compose: `["proto", "rust-core"]` merges both sets of read-first/verify/rules.

**Dispatch model:** Middle ground between template library and prompt compiler. Zones are coarser than templates (5-6 zones vs potentially dozens of templates) but more structured than free text. Good for the common case where tasks map cleanly to ownership zones. Bad for tasks that cut across zones in unusual ways.

## 6. Diff-Spec (Most Constrained)

TL provides the actual code changes as before/after snippets. Agent's job is mechanical application + verification:

```json
{
  "name": "topology-plumbing",
  "diffs": [
    {
      "file": "proto/effects/agent.proto",
      "after_line": "AGENT_STATUS_WAITING = 4;\n}",
      "insert": "enum WorkspaceTopology {\n  WORKSPACE_TOPOLOGY_UNSPECIFIED = 0;\n  ..."
    }
  ],
  "also_apply_to": ["rust/exomonad-proto/proto/effects/agent.proto"],
  "then_run": ["just proto-gen"],
  "verify": ["cargo check --workspace"]
}
```

Agent applies diffs, runs codegen, fixes any compilation errors from downstream breakage.

**Dispatch model:** Agent has near-zero autonomy. TL did all the thinking, agent does the typing. Highest correctness for mechanical changes (proto additions, field plumbing). Terrible for tasks requiring judgment. The TL spends tokens writing the diff — might be cheaper to just `Edit` the file directly. Best when: one diff cascades into many files (proto-gen), or when the change is trivial but the build/test cycle is slow.

## 7. Context Forwarding (TL's Reasoning as Prompt)

TL's exploration and planning conversation is summarized and forwarded. Agent gets the *reasoning* behind the task, not just the instructions:

```json
{
  "name": "topology-plumbing",
  "context_summary": "We identified that WorkspaceTopology is implicit (two tools = two topologies). Making it explicit in proto forces consistent handling in list/cleanup/spawn. Codex recommended proto over WASM-only for drift resistance.",
  "task": "Add WorkspaceTopology enum to agent.proto...",
  "key_decisions": [
    "Enum in proto (not WASM args) — forces both sides",
    "Field on AgentInfo (lifecycle property, not spawn-time only)",
    "UNSPECIFIED default for existing agents in list_agents"
  ]
}
```

**Dispatch model:** Agent understands *why*, not just *what*. When it hits an ambiguous choice (should cleanup dispatch on topology?), it has the reasoning to decide correctly instead of guessing or asking. Higher token cost per spawn (context is verbose). Best for tasks with judgment calls that are hard to enumerate in advance. The `key_decisions` field is the TL encoding its intent without micromanaging every line.

---

## Comparison Matrix

| Option | TL Effort | Token Cost | Agent Autonomy | Correctness | Best For |
|--------|-----------|------------|----------------|-------------|----------|
| 1. Structured Fields | High | Low (no boilerplate) | Minimal | Highest | Cross-cutting precision work |
| 2. Template Library | Low | Low | Low | High | Recurring task shapes |
| 3. Plan Artifact | Medium | Medium | Low-Medium | High | Multi-agent coordinated waves |
| 4. Prompt Compiler | Lowest | Medium | Medium | Medium | Experienced TL, clear tasks |
| 5. Role-Scoped | Low-Medium | Low | Low-Medium | High | Standard zone-aligned work |
| 6. Diff-Spec | Highest | Lowest | Minimal | Highest | Mechanical cascading changes |
| 7. Context Forward | Medium | Highest | Highest | Medium-High | Tasks requiring judgment |

## Recommendation

Not mutually exclusive. The tool schema should support composing these:

- **Default path:** Role-Scoped Defaults (5) as the base — every spawn gets zone-appropriate boilerplate for free.
- **Upgrade path:** TL adds structured steps (1) or diff-specs (6) when precision matters.
- **Context path:** TL adds context forwarding (7) when judgment matters.
- **Library path:** Template Library (2) grows organically as patterns recur.

The WASM tool handler composes layers: zone defaults + optional structured steps + optional context. One tool, multiple levels of specificity.
