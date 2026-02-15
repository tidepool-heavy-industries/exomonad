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

### spawn_gemini_teammate — Production Ready ✅
Audit complete (2026-02-11). All GEMINI_NOTES issues resolved:
- Branch naming uses `-` separator (no git collision risk)
- Names slugified for filesystem/git safety
- Cleanup reconstructs internal_name properly, removes all artifacts
- Agent-type-aware config: Claude gets `.mcp.json`, Gemini gets settings via `GEMINI_CLI_SYSTEM_SETTINGS_PATH` env var
- Both use per-agent endpoints (`/agents/{name}/mcp`) for identity
- Tabs vs Panes distinction consistent (subtree → Tab for isolation, worker → Pane for lightweight)
- Subrepo support integrated into cleanup path resolution

**spawn_worker is WIP** — no config.json registration (ephemeral/prototype only).

## Open Design Questions

### Per-agent identity flow
Each Gemini agent gets `.exo/agents/{name}/settings.json` pointing to `http://localhost:{port}/agents/{name}/mcp`. Server extracts identity from URL path param. Simple axum route, not middleware.
