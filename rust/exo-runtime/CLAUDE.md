# exo-runtime — the IO side of the capability seam

The single concrete `Runtime` struct that implements **every** `exo-caps` trait. Policy monomorphizes against this `R`. Where it can, it **reuses exomonad-shared's proven services** (tmux injection, agent-launch builder) rather than reimplementing — `exo-runtime` depends on `exomonad-shared`.

> Part of the v2 node-mode swarm (`exo`). See `rust/CLAUDE.md`.

## Layout — one file per cap

`runtime.rs` owns **only** the `Runtime` struct + accessors, so cap leaves never collide. Every other module is one `impl <Cap> for Runtime`.

| File | Cap / role |
|------|-----------|
| `runtime` | The `Runtime` struct: birth identity (`node_path`, `branch`, `own_pane`) + ambient context (`working_dir`, `parent_inbox`, `run_id`, `tmux_session`). Identity baked in at construction — always present, no `Option`, no task-locals. |
| `git` | `impl Git` — `tokio::process` git. `merge` = `git merge --no-edit <branch>` (the local fold). |
| `bus` | `impl Bus` — **the genuinely-new piece.** Append a line to the target's jsonl inbox; resolve `Addressee`→`InboxPath` (Parent = papers pointer; child = fold `children.jsonl`); stamp the envelope; assert line ≤ `PIPE_BUF` (4096) and **never spill**; append + flush, no fsync. **Instrumented with detailed success/failure logs.** |
| `spawner` | `impl Spawner` — the recursion (birth + teardown). See below. |
| `tmux` | `impl Tmux` — delegates to exomonad-shared `TmuxIpc::inject_input` (hardened buffer-paste). |
| `liveness` | `impl ChildLiveness` — the idle gate's read: any *direct* child still working? Combines the in-memory busy-bit map (mutated at birth in `spawner`, on child-deliver in `bus`, on `ChildIdle` in `exo-node`) with a tmux pane probe (`topology::live_panes`); a dead pane forces idle. Pure truth table split out + unit-tested. |
| `fs` `kv` `log` `process` | The remaining cap impls. |
| `node_config` | `write_node_agent_config` (Claude: `.mcp.json` + `.claude/settings.local.json` in the child's worktree, CWD-discovered) / `write_gemini_node_config` (Gemini: `settings.json` at a **per-pane** path `paths::gemini_settings_path`, env-var-discovered; also writes the role steering protocol to a sibling `protocol.md` and references it in `context.fileName` — Gemini's session-start steering channel, mirroring classic). Gemini's is per-pane, NOT worktree-local, because **inline** siblings share the parent's worktree — a worktree-local file would clobber each other's papers pointer → identity collision. Also generates a `policy.toml` with `allowRedirection = true` (and points to it via `adminPolicyPaths` in `settings.json`) so compound/redirected shell commands don't trigger permission prompts even under `--yolo`. The `settings.json` wiring is the primary mechanism; `--admin-policy` is the fallback wiring. |
| `session_boot` | `boot_root_session` — creates the detached `{session}` tmux window for the root, returns its pane. |


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
5. Write per-runtime MCP config, then `Tmux::paste` the launch command (via exomonad-shared's `build_agent_command` + `write_prompt_file` — prompt goes in a file, never inline) into the holding shell. The role protocol (override-or-const) is passed via `--append-system-prompt` for Claude; for Gemini, the `settings.json` context file is its system-prompt equivalent.

**Do not collapse steps 2+5** into a one-shot `new_pane(cwd, launch_cmd)` — that reopens the orphan window the two-phase split closes. Claude children get `CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS=1` so the Bus→Teams last hop can deliver native `<teammate-message>`s; Gemini children discover the sidecar via `GEMINI_CLI_SYSTEM_SETTINGS_PATH`.

### Opt-in context inheritance (`fork_session`, default false)

`BirthCore.fork_session` (set only from `ForkSpec::fork_session` in `fork_wave`; false for every other op) opts a **Claude worktree** child into inheriting the parent's context: the launch resolves the parent's Claude session UUID via `exo_scry::resolve_self_or_portable()` (observed live OS state — no registry) and links the child's Claude project dir to the parent's (`exomonad_shared::…::fork_session::link_parent_project_dir`), so `build_agent_command` emits `--resume <uuid> --fork-session`. Honored ONLY for Claude + Worktree + `fork_session`; Gemini, inline/worker, and reviewer spawns always pass `None`. Any miss (non-Claude, no team, no `lead_session_id`, resolution error) logs `tracing::warn` and falls back to a fresh launch — never crashes, never blocks the spawn. Default-false keeps the launch byte-identical unless explicitly opted in.

### Child launch policy (`yolo` / `wrap_nix`) — inherited via papers

The two launch knobs are config, not literals: `yolo` (pass `--yolo` to Gemini children) and `wrap_nix` (wrap the launch in `nix develop` when the cwd has a `flake.nix`). They live on `NodePapers` (the `exo-caps` config seam) with behavior-preserving defaults (`DEFAULT_YOLO = true`, `DEFAULT_WRAP_NIX = false`), so a node with no config set launches children exactly as before. `birth` reads the spawning node's *own* papers (`own_launch_policy` — `{working_dir}/.exo/node.json`, or the run-namespaced `root.json` for the root), stamps the same policy onto each child's papers, and passes it to the launch builder. So policy set on one node flows down its whole subtree; an unreadable/older papers file (the root's defaults, a pre-field papers) falls back to the defaults. Writing non-default values into a node's papers (e.g. wiring `config.toml` → `root.json` at init) is the remaining seam to expose it end-to-end.

## The Runtime's stamp is the anti-spoof guarantee

`Bus::deliver` stamps `from = Agent(self.name())` from the runtime's own identity. Policy hands over only a `Message` (no `from`), so a tool **cannot** forge its sender. Same discipline as the spawn ledger: the runtime owns identity, policy never asserts it.

## Gaps / not-yet

- **`birth` itself is not unit-tested** (it needs live tmux+git). Only its helpers (ledger append/read, name resolution, inbox-path derivation) and `Bus` have automated tests; the converge integration test (`exo-node/tests/converge.rs`) covers the bus round-trip end-to-end.
