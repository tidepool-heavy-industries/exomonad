# exo-runtime — the IO side of the capability seam

The single concrete `Runtime` struct that implements **every** `exo-caps` trait. Policy monomorphizes against this `R`. Where it can, it **reuses exomonad-core's proven services** (tmux injection, agent-launch builder) rather than reimplementing — `exo-runtime` depends on `exomonad-core`.

> Part of the v2 node-mode swarm (`exomonad experimental`). See `rust/CLAUDE.md`.

## Layout — one file per cap

`runtime.rs` owns **only** the `Runtime` struct + accessors, so cap leaves never collide. Every other module is one `impl <Cap> for Runtime`.

| File | Cap / role |
|------|-----------|
| `runtime` | The `Runtime` struct: birth identity (`node_path`, `branch`, `own_pane`) + ambient context (`working_dir`, `parent_inbox`, `run_id`, `tmux_session`). Identity baked in at construction — always present, no `Option`, no task-locals. |
| `git` | `impl Git` — `tokio::process` git. `merge` = `git merge --no-edit <branch>` (the local fold). |
| `bus` | `impl Bus` — **the genuinely-new piece.** Append a line to the target's jsonl inbox; resolve `Addressee`→`InboxPath` (Parent = papers pointer; child = fold `children.jsonl`); stamp the envelope; assert line ≤ `PIPE_BUF` (4096) and **never spill**; append + flush, no fsync. |
| `spawner` | `impl Spawner` — the recursion (birth + teardown). See below. |
| `tmux` | `impl Tmux` — delegates to exomonad-core `TmuxIpc::inject_input` (hardened buffer-paste). |
| `liveness` | `impl ChildLiveness` — the idle gate's read: any *direct* child still working? Combines the in-memory busy-bit map (mutated at birth in `spawner`, on child-deliver in `bus`, on `ChildIdle` in `exo-node`) with a tmux pane probe (`topology::live_panes`); a dead pane forces idle. Pure truth table split out + unit-tested. |
| `fs` `kv` `log` `process` | The remaining cap impls. |
| `node_config` | `write_node_agent_config` (Claude: `.mcp.json` + `.claude/settings.local.json` in the child's worktree, CWD-discovered) / `write_gemini_node_config` (Gemini: `settings.json` at a **per-pane** path `paths::gemini_settings_path`, env-var-discovered). Gemini's is per-pane, NOT worktree-local, because **inline** siblings share the parent's worktree — a worktree-local file would clobber each other's papers pointer → identity collision. |
| `session_boot` | `boot_root_session` — creates the detached `{session}` tmux window for the root, returns its pane. |


## Spawn: `birth` and record-first ordering (read before editing `spawner.rs`)

All three spawn ops (`spawn_worker`/`spawn_gemini`/`fork_wave`) fix their own `(role, agent_type, kind)` triple and funnel through one private `birth(BirthCore)` tail. The ordering is a **load-bearing race guard** — do not reorder:

1. (Worktree child only) `git worktree add` at `.exo/worktrees/{name}`.
2. `Tmux::new_window`/`new_pane` opens a **holding shell** (NOT the agent) → captures `%N`.
3. **Append `ChildRecord::Spawned{pane:%N}` to `children.jsonl`.** ← THE GUARD: the record precedes the *agent* launch, so a crash leaves at most a bare shell, never an untracked agent.
4. Write the child's `node.json` papers (`parent_inbox` = my own inbox).
5. Write per-runtime MCP config, then `Tmux::paste` the launch command (via exomonad-core's `build_agent_command` + `write_prompt_file` — prompt goes in a file, never inline) into the holding shell.

**Do not collapse steps 2+5** into a one-shot `new_pane(cwd, launch_cmd)` — that reopens the orphan window the two-phase split closes. Claude children get `CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS=1` so the Bus→Teams last hop can deliver native `<teammate-message>`s; Gemini children discover the sidecar via `GEMINI_CLI_SYSTEM_SETTINGS_PATH`.

### Child launch policy (`yolo` / `wrap_nix`) — inherited via papers

The two launch knobs are config, not literals: `yolo` (pass `--yolo` to Gemini children) and `wrap_nix` (wrap the launch in `nix develop` when the cwd has a `flake.nix`). They live on `NodePapers` (the `exo-caps` config seam) with behavior-preserving defaults (`DEFAULT_YOLO = true`, `DEFAULT_WRAP_NIX = false`), so a node with no config set launches children exactly as before. `birth` reads the spawning node's *own* papers (`own_launch_policy` — `{working_dir}/.exo/node.json`, or the run-namespaced `root.json` for the root), stamps the same policy onto each child's papers, and passes it to the launch builder. So policy set on one node flows down its whole subtree; an unreadable/older papers file (the root's defaults, a pre-field papers) falls back to the defaults. Writing non-default values into a node's papers (e.g. wiring `config.toml` → `root.json` at init) is the remaining seam to expose it end-to-end.

## The Runtime's stamp is the anti-spoof guarantee

`Bus::deliver` stamps `from = Agent(self.name())` from the runtime's own identity. Policy hands over only a `Message` (no `from`), so a tool **cannot** forge its sender. Same discipline as the spawn ledger: the runtime owns identity, policy never asserts it.

## Gaps / not-yet

- **`birth` itself is not unit-tested** (it needs live tmux+git). Only its helpers (ledger append/read, name resolution, inbox-path derivation) and `Bus` have automated tests; the converge integration test (`exo-node/tests/converge.rs`) covers the bus round-trip end-to-end.
