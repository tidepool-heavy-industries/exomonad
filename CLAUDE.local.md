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

## Claude Teams++ Strategy

Claude Teams is early beta — the filesystem protocol (JSON task files, config.json) is solid, the UX around it is half-baked (idle spam, shutdown ceremony, plan mode death spiral). Our approach: **use Teams as a task bus, route around the jank.**

**What we leech onto:** TaskCreate/TaskUpdate/TaskList — the CRUD primitives over `~/.claude/tasks/`. Simple, predictable, file-based. This is the bus.

**What we route around:** Teammate registration ceremony, idle notification noise, plan mode gating, shutdown negotiation. Scaffolding we tolerate for bus access.

**The bet:** Anthropic iterates on the bus (better task lifecycle, richer status, dependency resolution) and we get those upgrades for free. Jank we work around today might get fixed upstream.

**The edge:** When it doesn't get fixed, our Haskell WASM layer is where compensating logic lives — typed, hot-reloadable, not buried in prompts. This is why the DSL matters for Claude Code++: rapid iteration on orchestration without rebuilding binaries or editing prompt strings. The crash cage is code, not configuration.

## Open Design Questions

### Per-agent identity flow
Each Gemini agent gets `.exomonad/agents/{name}/settings.json` pointing to `http://localhost:{port}/agents/{name}/mcp`. Server extracts identity from URL path param. Simple axum route, not middleware.

### Long-lived teams
Documented in ecosystem CLAUDE.md (Ambient Teams section). TL checks for existing team on `--continue`, Gemini teammates persist across Claude restarts.
