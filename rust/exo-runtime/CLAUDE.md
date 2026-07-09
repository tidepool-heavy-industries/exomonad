# exo-runtime — the IO side of the capability seam

The single concrete `Runtime` struct that implements **every** `exo-caps` trait. Policy monomorphizes against this `R`. Where it can, it **reuses exomonad-shared's proven services** (tmux injection, agent-launch builder) rather than reimplementing — `exo-runtime` depends on `exomonad-shared`.

> Part of the v2 node-mode swarm (`exo`). See `rust/CLAUDE.md`.

## Layout — one file per cap

`runtime.rs` owns **only** the `Runtime` struct + accessors, so cap leaves never collide. Every other module is one `impl <Cap> for Runtime`.

| File | Cap / role |
|------|-----------|
| `runtime` | The `Runtime` struct: birth identity (`node_path`, `branch`, `own_pane`) + ambient context (`working_dir`, `parent_inbox`, `run_id`, `tmux_session`) + `own_kind: ChildKind`. Identity baked in at construction — always present, no `Option`, no task-locals. `Runtime::is_inline()` (`own_kind == Inline`) gates team isolation and the children-ledger shortcircuit. |
| `git` | `impl Git` — `tokio::process` git. `merge` = `git merge --no-edit <branch>` (the local fold). |
| `bus` | `impl Bus` — **the genuinely-new piece.** Append a line to the target's jsonl inbox; resolve `Addressee`→`InboxPath` (Parent = papers pointer; child = fold `children.jsonl`); stamp the envelope; append + flush, no fsync. A line > `PIPE_BUF` (4096) is **spilled** to a `.spill/` side-file + replaced by a small claim-check pointer (`IngestionEntry::spill`), so the wire line is always one atomic append while the payload (e.g. a rich verdict) can be arbitrarily large; `inbound::resolve_spilled` loads it back. **Instrumented with detailed success/failure logs.** |
| `spawner` | `impl Spawner` — the recursion (birth + teardown). `read_child_records` returns `Ok(Vec::new())` immediately when `self.is_inline()` (chokepoint for `any_child_busy` + `read_children` + shutdown live-children count). See below. |
| `tmux` | `impl Tmux` — `paste` delegates to exomonad-shared `TmuxIpc::inject_input` (hardened buffer-paste); `list_panes` (`tmux list-panes -a`) is the one liveness probe both `Topology` and `ChildLiveness` consume (an `Err` is a probe failure, never "no panes"). |
| `liveness` | `impl ChildLiveness` — any *direct* child's pane currently exist? A direct `Tmux::list_panes` probe over the child ledger; probe failure ⇒ assume busy (never manufacture a false idle). There used to be an in-memory busy-bit map here too (mutated at birth in `spawner`, on child-deliver in `bus`, on a Claude Code `Stop`-hook-derived `ChildIdle` report in `exo-node`), combined with the pane probe as a floor — removed, since `Stop` fires on every turn-end (including a legitimate async-wait yield) and the bit was routinely wrong. Pure truth table split out + unit-tested. |
| `fs` `kv` `process` | The remaining cap impls. |
| `node_config` | `write_node_agent_config(settings_path, mcp_path, papers)` — writes a node's MCP server + hooks to **private files** (siblings of its papers, via `paths::node_config_paths`), NOT the cwd. `claude` is pointed at them with `--settings <settings_path> --mcp-config <mcp_path>` (added in `build_agent_command`), which **merge** over the cwd config (plain flags — no `--strict-mcp-config`/`--setting-sources`, so the user's own `.mcp.json`/settings survive). This is what stops an **inline worker** (shared cwd) from clobbering the parent's `.claude/settings.local.json`/`.mcp.json`, and keeps `.mcp.json` out of the repo entirely. The role steering protocol is delivered via launch-time `--append-system-prompt`, not config. |
| `session_boot` | `boot_root_session` — creates the detached `{session}` tmux window for the root, returns its pane. Raw tmux on purpose: it runs *before* a `Runtime` exists (the Runtime is constructed *with* the pane this returns). |

**Composite impls go through the Runtime's own primitive impls** (the `exo-caps` supertrait edges): `spawner` births/reclaims via `Git::worktree_add`/`worktree_remove` + `Tmux::*` + `Fs::write_atomic` (papers) / `Fs::read` (ledger, policy, protocol override); `topology`/`liveness` probe via `Tmux::list_panes`. Two deliberate exceptions stay raw: the **ledger/inbox appends** (`append_child_record`, the `Bus` append — there is no `Fs::append` by design; the two append disciplines live inside the impls) and **`topology`'s recursive walk** (sync `std::fs` inside one `spawn_blocking`; an async cap can't be awaited there).


## Spawn: `birth` and record-first ordering (read before editing `spawner.rs`)

The one generic `Spawner::spawn<S: SpawnSpec>` reads `(role, kind, name, task,
fork_session)` off the domain spec (the domain tool fixed the role/kind) and funnels through one
private `birth(BirthCore)` tail. The agent backend is `RoleKind::agent_type(role)`; the branch is
safe-generated for a Worktree child or the parent's branch for an Inline one. The prompt arrives
**pre-rendered** (`spec.into_task()` — the domain owns `render_spec_prompt` now), and the node's spec
is persisted to `.exo/acceptance.md` by the spawning **domain tool** via the `Fs` cap, **not** by
birth (the runtime no longer knows the review-gate's filename). The ordering is a **load-bearing
race guard** — do not reorder:

1. (Worktree child only) `git worktree add` at `.exo/worktrees/{name}`.
2. `Tmux::new_window`/`new_pane` opens a **holding shell** (NOT the agent) → captures `%N`.
3. **Append `ChildRecord::Spawned{pane:%N}` to `children.jsonl`.** ← THE GUARD: the record precedes the *agent* launch, so a crash leaves at most a bare shell, never an untracked agent.
4. Write the child's `node.json` papers (`parent_inbox` = my own inbox).
5. Write the node's MCP config (`write_node_agent_config`), then `Tmux::paste` the launch command (via exomonad-shared's `build_agent_command` + `write_prompt_file` — prompt goes in a file, never inline) into the holding shell. The role protocol (override-or-const) is passed via `--append-system-prompt`. The per-role model (`RoleKind::model()` → `BirthCore.model` → `ClaudeSpawnFlags.model`) becomes `build_agent_command`'s `--model` flag — `sonnet` for dev/worker/reviewer leaves, `opus` for a spawned TL. **Every spawned node gets an explicit model** — `None` ("inherit the launcher's default") is reserved for `root` alone, since root is never spawned via `birth` (it's the human's own top-level `exo init` session); a spawned node inheriting "whatever the launcher's default happens to be" would burn whatever tier the human's own interactive session is set to (which may be a cheap/fast model picked for chat, not subagent work) on real decomposition/implementation work.

**Do not collapse steps 2+5** into a one-shot `new_pane(cwd, launch_cmd)` — that reopens the orphan window the two-phase split closes. Worktree children get `CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS=1` so the Bus→Teams last hop can deliver native `<teammate-message>`s. **Inline workers do NOT get this flag** — they share the parent's cwd, so team resolution would land in the parent's team (team-leak fix). `birth_finish` also stamps `kind: core.kind` into the child's papers so bootstrap reads it back and calls `Runtime::is_inline()` correctly.

### Launch profiles (`RoleKind::launch_profile_env_prefix`) — per-role non-default brain

A role may opt into running its Claude on a **non-default Anthropic-compatible endpoint/model** (e.g. the reviewer → Kimi via a local [`claude-code-proxy`](https://github.com/raine/claude-code-proxy)). It stays `AgentType::Claude` — only the launch env changes, so Teams/hooks/MCP all keep working. **Backend-agnostic** (Kimi, ChatGPT, …): the code names no vendor. Flow:

1. **Resolve** (`Spawner::spawn`, role still typed): `LaunchProfile::resolve` reads `{prefix}_{BASE_URL,MODEL,AUTH_TOKEN,LABEL}` from this node's own env. **Gated on `_BASE_URL` present** (the defining field of a redirect; a half-set env — only `MODEL`/`LABEL` — does not half-activate). The `auth_token` is **optional** (`Option<String>`): a local proxy holds the OAuth, so when none is given the **translate** step supplies a placeholder (`DEFAULT_PROXY_TOKEN`) — Claude requires a non-empty `ANTHROPIC_AUTH_TOKEN` whenever `ANTHROPIC_BASE_URL` is set. The token lives in `BirthCore.launch_profile` **in memory only** — never written to papers.
2. **Propagate** (`birth_finish`, unconditional): every `EXO_*_{BASE_URL,MODEL,AUTH_TOKEN,LABEL}` in this node's env is re-copied into each child's launch env (opaque — NOT `ANTHROPIC_*`), so a deep submitter that spawns a profiled reviewer still carries the config. Pattern-matched — a new role/backend needs no edit here. `exo init` seeds these from the operator's shell into the root launch.
3. **Translate** (`birth_finish`, profiled child only): insert `ANTHROPIC_BASE_URL` / `ANTHROPIC_AUTH_TOKEN` (+ `ANTHROPIC_SMALL_FAST_MODEL` = the model, so a single-model proxy's background calls don't 404). The profile's model wins over the role's default via `ClaudeSpawnFlags.model` → `--model`.
4. **Label**: the non-secret `LABEL` tags the tmux window (`🤖 reviewer-0 (kimi)`) and is recorded on the `ChildRecord::Spawned.model_label` so the `tree` tool shows it. Unset env ⇒ no profile ⇒ launch byte-identical.

### Opt-in context inheritance (`fork_session`, default false)

`BirthCore.fork_session` (set only from `ForkSpec::fork_session` in `fork_wave`; false for every other op) opts a **worktree TL** child into inheriting the parent's context: the launch resolves the parent's Claude session UUID via `exo_scry::resolve_self_or_portable()` (observed live OS state — no registry) and links the child's Claude project dir to the parent's (`exomonad_shared::…::fork_session::link_parent_project_dir`), so `build_agent_command` emits `--resume <uuid> --fork-session`. Honored ONLY for Worktree + `fork_session`; inline/worker and reviewer spawns always pass `None`. Any miss (no team, no `lead_session_id`, resolution error) logs `tracing::warn` and falls back to a fresh launch — never crashes, never blocks the spawn. Default-false keeps the launch byte-identical unless explicitly opted in.

### Child launch policy (`yolo` / `wrap_nix` / `review_enabled`) — inherited via papers

Three tree-wide knobs are config, not literals: `yolo` and `wrap_nix` (wrap the launch in `nix develop` when the cwd has a `flake.nix`), plus `review_enabled` (does `submit_branch` spawn a reviewer at all — the `exo` domain's concern, but propagated by this same seam). They live on `NodePapers` (the `exo-caps` config seam) with behavior-preserving defaults (`DEFAULT_YOLO = true`, `DEFAULT_WRAP_NIX = false`, `DEFAULT_REVIEW_ENABLED = false`), so a node with no config set launches children exactly as before. `yolo` only reaches the shared `build_agent_command`'s Gemini arm, so for a Claude tree node it is **inert** — every node launches `claude --dangerously-skip-permissions` unconditionally; the knob is retained on the papers seam for the shared launch builder. `birth` reads the spawning node's *own* papers (`own_launch_policy` — `{working_dir}/.exo/node.json`, or the run-namespaced `root.json` for the root), stamps the same policy onto each child's papers, and passes it to the launch builder (`review_enabled` isn't a launch-command flag — it's read straight back off a node's own papers by the domain's `submit_branch` tool via `Fs::read`, the same file `own_launch_policy` reads). So policy set on one node flows down its whole subtree; an unreadable/older papers file (the root's defaults, a pre-field papers) falls back to the defaults. `exo/src/config.rs` + `init.rs` wire `.exo/config.toml` → the root's papers for all three knobs.

## The Runtime's stamp is the anti-spoof guarantee

`Bus::deliver` stamps `from = Agent(self.name())` from the runtime's own identity. Policy hands over only a `Message` (no `from`), so a tool **cannot** forge its sender. Same discipline as the spawn ledger: the runtime owns identity, policy never asserts it.

## Reclaim ordering: kill nested panes before nested removal

`Spawner::reclaim_worktree` (`spawner.rs`) walks a child's worktree for **nested** worktree
directories (`.exo/worktrees/**` inside it — e.g. a reviewer spawned inside a leaf being merged)
and force-removes them innermost-first. A nested worktree's pane is **not** recorded in the
outermost child's own ledger — it's recorded in its immediate ENCLOSING directory's
`children.jsonl` (the parent that actually spawned it). Before removing a nested dir, the walk
reads that enclosing ledger (`Runtime::read_child_records_at`, a path-parametrized sibling of
`read_child_records` sharing the same tolerant-parse helper) and kills the recorded pane
(`kill_nested_pane_before_removal`, wrapped in `retry_teardown`) — so a live nested agent's process
dies before its cwd is force-removed out from under it. A missing ledger, missing record, or a
kill that fails after retries is logged at `warn!` and swallowed: the pane-kill is best-effort, not
a precondition for the removal. The **outermost** worktree's own pane-kill stays the caller's job
(`merge`/reviewer-verdict teardown already call `Spawner::kill_pane` before `reclaim_worktree`).

The discovery walk (`read_dir`/`next_entry`/`file_type` over each `.exo/worktrees` dir) used to
silently drop errors via `if let Ok`; every failure now logs at `warn!` naming the path — discovery
stays best-effort but loud. A nested removal that still fails after retries is collected and
surfaced as an `Err` (`op: "reclaim_nested"`) **after** the outermost removal is still attempted —
so a caller (the `merge` tool) never reports a clean "(reclaimed X)" when nested dirs were left
behind; the outermost-failure path (already an `Err`) is unchanged.

## Gaps / not-yet

- **`birth` itself is not unit-tested** (it needs live tmux+git). Only its helpers (ledger append/read, name resolution, inbox-path derivation) and `Bus` have automated tests; the converge integration test (`exo-node/tests/converge.rs`) covers the bus round-trip end-to-end.
