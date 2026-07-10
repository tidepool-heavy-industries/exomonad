# E2E Tests

Documentation and conventions for running and adding end-to-end tests.

```bash
# Classic (interactive — launches tmux session, you observe)
just e2e-messaging         # Teams inbox delivery pipeline
just e2e-hook-rewrite      # BeforeModel/AfterModel PII rewriting

# v2 node-mode (offline, non-interactive, asserts and exits)
bash tests/e2e/v2-loop/run.sh
```

## Classic vs v2

`tests/e2e/messaging/` and `tests/e2e/hook-rewrite/` (and any other scenario using
`exomonad init` / `spawn_gemini` / `file_pr` / a `testrunner.md` companion) are **Classic-only**
— they drive the central-server architecture and are kept, deprecated, for regression coverage.
They are interactive by design (you attach and observe).

`tests/e2e/v2-loop/` is the **v2 node-mode** template: it drives the `exo` binary directly
(`exo init` / `exo node`), runs fully offline (no real Claude, no network, no API keys — see
its own section below), and is meant to be run non-interactively (CI-shaped: asserts, exits
0/non-zero). New v2 scenarios should follow its pattern, not the Classic one — the two
architectures don't share a harness because they don't share tools (v2 has no `spawn_gemini`/
`file_pr`/Copilot to simulate).

### E2E Test Pattern (Classic)

All Classic E2E tests live in `tests/e2e/{name}/` and follow the same structure:

**Files:**
| File | Purpose |
|------|---------|
| `run.sh` | Setup script: creates temp repo, configures companions, runs `exomonad init` |
| `testrunner.md` | Test plan for the Claude testrunner companion (copied to `.exo/roles/devswarm/context/testrunner.md`) |
| `e2e-test.md` | Root TL rules for this test (copied to `.claude/rules/e2e-test.md`) |

**Structure of `run.sh`:**
1. **Preconditions** — Check `exomonad` binary, WASM plugins, `tmux`, `git`
2. **Temp environment** — `mktemp -d`, bare remote, working repo, `exomonad new`, symlink WASM
3. **Config** — Write `config.toml` with `yolo = true`, companions for the test scenario
4. **`exomonad init`** — Last line of the script. Creates tmux session, starts server, spawns companions, attaches.

**Companion roles:**
- **Test subject** — The agent being tested (e.g., Gemini with dev role for hook rewriting)
- **Testrunner** — Claude (haiku) companion with `testrunner` role. Observes results via bash (read-only), reports via `notify_parent`

**Key conventions:**
- `shell_command = "bash"` (not nix develop — temp env has no flake)
- `yolo = true` (skip interactive prompts)
- `export GITHUB_TOKEN="test-token-e2e"` (dummy token to avoid auth errors)
- Cleanup via `trap cleanup EXIT` (kills tmux session, removes temp dir)
- Testrunner uses only `notify_parent` MCP tool + read-only bash observation
- Root TL creates a team and idles

**Adding a new Classic E2E test:**
1. Create `tests/e2e/{name}/run.sh` following the pattern above
2. Create `testrunner.md` with the test plan (phases, assertions, report format)
3. Create `e2e-test.md` with root TL rules (usually: create team, idle)
4. Add `just e2e-{name}` recipe to `justfile`

## `tests/e2e/v2-loop/` — the v2 node-mode core loop

Boots a real v2 root node (`exo init`), spawns a real dev child (`spawn_dev`, over a live
`exo node` MCP sidecar), lets the child commit, merges the child's branch back (`merge`), and
asserts teardown — the target scenario: spawn → child commit → parent merge → teardown.

**Files:**
| File | Purpose |
|------|---------|
| `run.sh` | The whole scenario: scratch env, boot, drive, assert, cleanup. Runnable standalone. |
| `claude-shim.sh` | A fake `claude` binary (PATH-shadows the real one for the scratch tmux session) — reads its own role off the node's papers (via `--mcp-config`, never the task prompt) and does one deterministic thing per role: `dev` makes a specific-path commit; everything else (root) idles. |
| `mcp_call.py` | Drives one MCP tool call against a v2 node: spawns `exo node --papers <path>`, does the `initialize` → `tools/call` JSON-RPC handshake `rust/exo-node/src/outbound.rs` implements, prints the tool's `{text, data}`, exits. |

**Key technique — no real Claude, no LLM, fully offline:** the spawner launches
`claude <flags> '<task>'` in every pane; `claude-shim.sh` intercepts that (a scratch `PATH`
entry ahead of the real `claude`) and runs a deterministic sequence instead. Root's own
spawn/merge orchestration is driven directly by `run.sh` via `mcp_call.py` against root's
papers file, **not** by anything running inside root's pane — an MCP tool call is a fresh,
stateless `exo node --papers <path>` subprocess regardless of whether a conversational agent
happens to be live in that pane, so root's shim invocation doesn't need to do anything.

**Scope taken — the documented fallback:** the spec's target scenario has the child call
`submit_branch` itself (through its own sidecar) and asks the sidecar to auto-forward
`[READY]`. This scenario instead drives the **spawn + parent-side merge** halves directly
(`spawn_dev` then `merge`, both called the same way root's own sidecar would), skipping the
live `submit_branch` MCP round-trip through the child's own sidecar. This is exactly the
fallback scope the spec calls acceptable, and it matches production behavior in the common
case anyway: `review_enabled` defaults to **off**, so `submit_branch` would forward `[READY]`
straight to the parent with no reviewer spawn — no different in effect from the parent calling
`merge` directly once the child has committed. Follow-up: drive `submit_branch` itself through
a live child sidecar for full fidelity (needs a second `exo node` subprocess kept alive in the
child's worktree, reading the child's own inbox for the `[READY]`-equivalent bus message).

**tmux gotcha this scenario works around:** a brand-new tmux *session* does **not** inherit
the environment of the client process that created it when the tmux *server* was already
running (verified empirically) — it inherits the server's original startup environment
instead. `run.sh` uses `tmux set-environment` (which only reaches panes created *after* the
call) right after `exo init` returns, before spawning the dev child, so the child's pane gets
the scratch `PATH`/`HOME`/status-dir. Root's own pane predates that call and is never relied on
(see the technique above).

**Running:** `bash tests/e2e/v2-loop/run.sh` — offline, non-interactive, exits 0 on pass. No
`just` recipe yet (add `just e2e-v2-loop: bash tests/e2e/v2-loop/run.sh` to the `justfile` if
adopting the `just e2e-*` convention for v2 scenarios).
