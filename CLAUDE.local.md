# Session Scratchpad

## Agent Supervision Lessons (this session)

### Agents must cite sources, not just state facts
When an agent claims an infrastructure limitation ("axum nest_service doesn't support dynamic segments"), demand: WHERE did you learn this? Show me the code/docs. Confident hallucinations about nonexistent constraints can steer design decisions wrong. We almost chose headers over URL routing to work around a problem that didn't exist.

### Agents overengineer when unsupervised
Given "add a route with a path extractor" the agent built a full tower middleware stack (AgentIdentityLayer, AgentIdentityService). Always specify the complexity budget: "this is 10 lines, not a new module."

### Plan mode is for planning only
Plan mode gates every file write with approval. Never spawn implementation agents in plan mode. Pattern: plan mode agent → review plan → shutdown → respawn in default mode with plan as context.

### Fewer messages, more context per message
Agents that get short directives ("go") tend to drift. Agents that get full context in one shot (files, format, examples) produce better first attempts.

## Implementation Status

All spawn tools production-ready: `spawn_subtree` (Claude worktree+tab), `spawn_leaf_subtree` (Gemini worktree+tab), `spawn_workers` (Gemini panes, ephemeral). Per-agent identity resolved via URL path: `/agents/{role}/{name}/mcp`.
