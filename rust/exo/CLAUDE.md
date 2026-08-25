# exo — the v2 node-mode binary (CLI) + the domain (tools / roles / gates)

`exo` is the **standalone v2 node-mode binary** — it owns the whole node-mode CLI surface
(`exo init` / `exo node` / `exo hook`; classic `exomonad` is server-only). The lib half is the minimal **domain usage** of
[`exo-framework`](../exo-framework/CLAUDE.md): the genuinely domain-specific Bucket-C logic that
ports from the old Haskell DSL — the MCP tool set, the per-role roster, and the CC hook gates. This
is the "small usage" half of the framework/domain split (the Rust analog of Classic's Haskell-WASM
config DSL). Written **generic over the `exo-caps` traits** (no `dyn Caps`), so least-privilege is
compiler-checked and every tool is **unit-testable against mock caps with zero IO**.

The engine never depends on this crate's lib. The binary's bin-only `domain.rs` defines `ExoDomain`
(the [`Exomonad`](../exo-framework/CLAUDE.md) impl: `Caps = Runtime`, `Role = ExoRole`, `System =
ReviewSystem`, `Spawn = ExoSpawn`) and monomorphizes the engine once as `run_node::<ExoDomain>`;
that's the seam (the fn-pointer `RoleRegistry` is gone). See
[`docs/decisions/exo-framework-domain-split.md`](../../docs/decisions/exo-framework-domain-split.md)
and [`docs/decisions/exo-trait-refactor.md`](../../docs/decisions/exo-trait-refactor.md).

The CLI modules (`main.rs` / `init.rs` / `hook.rs` / `config.rs`) are **bin-only** — they link the
v2/shared seam (`exo-node`, `exo-runtime`, `exomonad-shared`) but never classic `exomonad-core`. The
lib (`lib.rs` + `tools/` + `gates.rs` + `roles.rs`) stays generic over the caps and links neither.

> Part of the v2 node-mode swarm (the `exo` binary). See `rust/CLAUDE.md`.

## Shape

| File | Contents |
|------|----------|
| `lib.rs` | Re-exports `role_def`, `ExoRole`, `ReviewSystem`/`handle_review_system` (the domain `System` + relocated gate: findings-based), `ExoSpawn` (the domain `Spawn`). Generic over `R`, depends only on `exo-framework` + `exo-caps` (+ `tracing`). |
| `review.rs` | The domain's inter-node behavior: `ReviewSystem` (`D::System`) + `handle_review_system` (decision derived from structured findings; IO-free via the `SystemCtx` seam — unit-tested against a mock context). **Now persists each round to a durable `ReviewLog` (`ReviewRound`) at `.exo/reviews/{safe-branch}.json`** using the `safe_branch` helper. Also `handle_review_tick` — a reviewer's wall-clock abandonment timeout (`REVIEW_ABANDON_TIMEOUT`, 30 min), called from `ExoDomain::handle_tick` by the sidecar's watchdog loop instead of a Stop hook. |
| `spawn.rs` | `ExoSpawn` (`D::Spawn`) implementing `SpawnSpec`, the role-fixing the per-op tools do; `render_spec_prompt` (moved from the runtime) + `write_acceptance` (the `.exo/acceptance.md` write via `Fs`, relocated out of birth). |
| `directives.rs` | `Directives` — the node's **standing directives**, loaded from its local untracked `.exo/directives/*.md`, injected into every child's spec, copied into worktree children, content-hashed onto their ledger rows. See [Standing directives](#standing-directives) below. |
| `domain.rs` | **Bin-only.** `ExoDomain` — the `Exomonad` impl that fixes `Caps = Runtime` and points `role_def`/`handle_system` at the lib. The one place that links `exo-runtime`. |
| `main.rs` | The CLI dispatcher (bin): clap `Cli` → `init` / `node` / `hook` / `listen` (the wake-channel client run under Claude Code's Monitor tool) / `doctor`. `node` is the composition root — `exo node --papers <path>` → `exo_node::bootstrap::<ExoDomain>(papers, cwd)` → `run_node::<ExoDomain>`. |
| `init.rs` | `exo init [--session <s>] [--recreate]` — bootstrap a node-mode ROOT (own tmux session, root papers, no server). Reuses `exo-runtime`/`exomonad-shared`. |
| `doctor.rs` | `exo doctor [--fix] [--include-unmerged]` — health-check + cleanup tool for worktrees. |
| `hook.rs` | `exo hook <event> --papers <path>` — handle a CC hook via the node's `exo` gate (SessionStart in-process; everything else routes to the sidecar hook socket, fail-open). |
| `config.rs` | Minimal node-mode init config read (`tmux_session`, `model`, the child-launch policy `yolo`/`wrap_nix`, and `[launch_profile.<role>]` tables flattened to `EXO_<ROLE>_*` for the reviewer-brain redirect) — classic `exomonad` owns the full `Config`. A missing `.exo/config.toml` defaults silently; a config file that exists but fails to read or parse is a loud error (`discover()` returns `anyhow::Result`) — a typo'd config must never be mistaken for an absent one. |
| `tools/` | One module per tool — a type + `Args` (derives `Deserialize + JsonSchema`) + `impl Tool<R>` (typed authoring trait; cap bounds in the impl header are the tool's least-privilege spec). The framework's `Adapter` handles JSON erasure; no per-tool adapter, no macro. Each ships mock-cap unit tests. |
| `gates.rs` | The concrete hook bodies: `pre_tool_use` (antipattern nudges), `session_start`. Functions generic over the caps they need. (There used to be a `stop` gate + per-role variants here — removed; see "The gates" below.) |
| `roles.rs` | `ExoRole` (the domain's `D::Role`, impl `RoleKind`) + `role_def(ExoRole)` — the hand-written table (the single place a role's tool list + hooks are named), resolved through `ExoDomain`'s `Exomonad::role_def`. `RoleKind::protocol` is overridden here to map each variant to its `protocol.rs` const. |
| `protocol.rs` | Per-role **decomposition-steering protocol** consts (`ROOT`/`TL`/`DEV`/`WORKER`/`REVIEWER`) — the prose the engine injects at session_start. The **source of truth** (ported from `.exo/roles/devswarm/context/*.md`, translated to v2 mechanics: local `merge` + `submit_branch`, no PRs/Copilot); an optional on-disk `.md` override wins during prompt-tuning. |
| `testing.rs` | `MockRuntime` — impls every cap, records calls, returns canned values. Every tool tests against this one shared mock. |

The `Tool<R>` trait, `RoleDef<R>`, the hook decision enums, and `PolicyCaps` are the framework
contract ([`exo-framework`](../exo-framework/CLAUDE.md)); this crate provides the concrete instances.

## The tools

| Tool | Caps | Roles | What it does |
|------|------|-------|--------------|
| `fork_wave` | `Spawner`+`Fs`+`Git` | root, tl | Fork N Claude TL children (own worktrees). Per-child opt-in `fork_session: bool` (default false) inherits the parent's context via `--resume --fork-session`; default-false launches fresh. Per-child `model` override (tier-capped, see below). Per-child `review: Option<bool>` override — `None` (default) inherits the spawner's own `review_enabled`, `Some(b)` stamps the child's papers with `b` and it inherits onward down that child's own subtree. Per-child `file_boundary: Vec<String>` (default empty = unrestricted) is **rendered into the child's own spec** under an `ALLOWED PATHS` section (distinct from the prose `boundary`/`ANTI-PATTERNS (DO NOT)` list — see [`boundary`](#fold-time-file-boundary) below) AND persisted parent-side so `merge` checks it against that child's actual diff before folding. **Refuses on a dirty worktree**, naming the offending `git status --porcelain` lines — children fork from the spawner's current commit, so uncommitted state would be invisible to them; a git error fails *closed*. `preview: true` renders every child's fully-assembled spec (directives injected, `birth_preamble` reproduced, `ALLOWED PATHS` included) and spawns **nothing** — no clean gate, no acceptance writes, no boundary writes — so a wave can be checked while the tree is still dirty. The tool's `data` also carries every spawned child's byte-exact rendered spec (`spawned: [{name, spec}, ...]`) — the spawner never composes a spec blind. **Spawn-time spec validation:** before any child is spawned, every child's non-empty `read_first` is checked against `Git::tracked_at_head` — a child forks from your COMMIT, so a path that exists on disk but isn't tracked at HEAD is invisible to it; any missing path refuses the WHOLE wave (all-or-nothing, naming the offending child + paths), before any worktree/boundary/acceptance write happens. A non-empty `read_first` that falls outside a non-empty `file_boundary` is a **warning only** (`note: read_first outside file_boundary …` appended to the result text) — reading outside your own edit scope is legitimate, so this never blocks. `preview: true` runs both checks too, rendering a missing-`read_first` refusal as a `WOULD REFUSE:` block instead of erroring. |
| `spawn_dev` | `Spawner`+`Fs`+`Git` | root, tl | Spawn a Sonnet Claude dev in its own worktree. Takes the same tier-capped `model` override, the same per-spawn `review: Option<bool>` override, and the same `file_boundary: Vec<String>` (see `fork_wave`, including the rendered `ALLOWED PATHS` section), and the same dirty-worktree refusal as `fork_wave`. The tool's `data` carries the child's byte-exact rendered spec (`data.spec`). **Spawn-time spec validation:** same `read_first`-tracked-at-HEAD refusal and `read_first`-outside-`file_boundary` warning as `fork_wave` (see there), run before the clean-worktree gate. |
| `spawn_worker` | `Spawner`+`Fs` | root, tl | Spawn an ephemeral Sonnet Claude worker (inline pane). Takes the `model` override, but is **not** clean-gated — an inline worker deliberately shares the parent's tree, and gets directives by text injection only (it already sees the parent's `.exo/directives/` on disk). No `file_boundary` — a worker never gets its own branch, so there's nothing for `merge` to check. |
| `dismiss_worker` | `Spawner` | root, tl | Dismiss an inline worker by name: unconditional parent-side `kill_pane` resolved via the children ledger. Matched to `spawn_worker`; the reliable teardown primitive for workers that never registered as a teammate. |
| `request_review` | `Fs`+`Bus` | root, tl | Mid-flight flip of a child's review gate to ON, for that child's NEXT `submit_branch`. Resolves `child` against the caller's own `.exo/children.jsonl` fold (same discipline as `broadcast`'s `read_own_children`); refuses on an unknown or terminal (reaped/died) child, naming the state. Reads the child's own birth papers at `.exo/worktrees/{child}/.exo/node.json` (relative to the CALLER's worktree — parent-authored infra `submit_branch` reads at call time), sets `review_enabled: true`, writes back — an unreadable or corrupt papers file is a loud error, never a silent default. **One-way:** there is no un-flip; review, once requested, stays on for that child's subtree exactly like the config-driven default. Idempotent — flipping an already-on gate is a no-op success, reported as such. Notifies the child over `Bus`. |
| `amend_boundary` | `Fs`+`Bus` | root, tl | Fix a wrong [file boundary](#fold-time-file-boundary) recorded for a child at spawn time — a full-replace of its `allowed` list. **Parent-side bookkeeping only:** reads/writes `.exo/boundaries/{child}.json` in the CALLER's own worktree, never reaches into the child's; `merge` reads the file fresh at fold time, so an amendment is honored with zero merge-side changes. Same child resolution/refusal as `request_review` (unknown or terminal child refused; Live and Submitted are both amendable — a submitted child is still waiting on this node's `merge`). `allowed` must be non-empty (an empty list would silently mean "touch nothing" — refuses and points at `merge`'s `boundary_override` for that rare real case instead). Amends an EXISTING boundary only: a child spawned without `file_boundary` has nothing to amend, and this tool refuses loudly rather than authoring enforcement that was never there at spawn — that's an authoring decision, not this tool's to make. The write is LOUD on failure (unlike `write_boundary`'s best-effort spawn-time write — an operator amending expects it to stick). Notifies the child over `Bus` with the new list; the tool output renders old → new. |
| `merge` | `Git`+`Spawner`+`Process`+`Fs` | root, tl | **The local fold:** if the child has a persisted [`FileBoundary`](#fold-time-file-boundary) (from `file_boundary` at spawn time), its actual diff (`Git::commits_between(merge_base, branch)`) is checked against it **before** `git merge` runs; a violation refuses with the offending files named (`boundary_override: true` merges anyway, noted in the output). A child with no persisted boundary merges unrestricted, byte-identical to before this mechanism existed. Then `git merge <child-branch>`, followed by best-effort teardown (`kill_pane` + `reclaim_worktree`) of the child. When **both** teardown calls return `SpawnError::UnknownChild`, the note becomes an explicit `merged non-child ref; no teardown performed (succession escape hatch: …)` instead of the generic best-effort string — `merge` accepts any local ref so a live ancestor can fold a dead TL's orphaned descendant, and on that path there simply is no ledger child of yours to reclaim (the boundary check is skipped the same way when `merge_base` resolves to `None` — unrelated histories). Every other outcome (both ok, partial, mixed) renders exactly as before. **Optional `gate`:** a whitespace-split verification command run after the merge, before teardown — on failure the merge stays committed (NOT rolled back) and teardown is skipped, leaving the child alive to fix its work; on success, its output tail (`gate_output_tail`, ~1KiB cap) rides in `data.gate.output_tail` even though the rendered text just says `(gate ok)` — the parent can see gate warnings without re-running it. **Optional `gate_timeout_ms`:** bounds `gate`'s runtime via `Process::run_with_timeout`, which kills the gate's whole process group (`killpg`, not just the direct child) on expiry rather than leaking it; a timeout is rendered exactly like a gate failure (committed, teardown skipped) but clearly labeled TIMED OUT, with `data.gate.exit = "timeout"`. Omitting `gate_timeout_ms` runs `gate` via plain `Process::run` with no timeout, byte-identical to before this field existed. |
| `submit_branch` | `Git`+`Process`+`Spawner`+`Fs`+`Bus` | tl, dev | **Request review** — if reviewers are enabled (`review_enabled` in `.exo/config.toml`, off by default; read from the node's own papers). Runs the ordered precondition checks (committed → **needs_rebase** → `.exo/checks/pre-merge/*` scripts); the rebase gate blocks + prompts `git rebase <parent>` when the branch is behind its parent's REAL git branch (`NodePapers.parent_branch`, birth-stamped by the spawner from ITS OWN current branch — not a dot-derived tree-address coordinate, so the gate fires at every depth, including a direct child of root; fails open only on the root itself, or unreadable/corrupt/pre-field papers). Then spawns a **reviewer** off this branch (fork-point `git diff` base via `Git::merge_base`) and returns "stop & wait". It does NOT deliver `[READY]` itself except via the skip path — only the sidecar does, on an approve-verdict (the structural gate). **Continuity:** reads the latest `ReviewLog` and appends unresolved Error findings from the prior round to the reviewer task. Explicit escape hatch regardless of config: `dangerously_skip_reviewer: true`. **Structured receipts:** an optional typed `receipts` block (`commit_tested`, `verify_commands_run`, `metrics: Vec<LabeledValue>`, `deviations`) rendered compactly into the parent-bound `[READY]` text and carried in full in the tool's `data`. `commit_tested` drives the **transfer proof** — see [Receipts and the transfer proof](#receipts-and-the-transfer-proof). **Full receipts reach the parent:** whenever `receipts` is passed, the FULL untruncated JSON is also written to `.exo/receipts-submitted.json` in the submitter's own worktree (best-effort, both the skip and reviewer-spawn paths — the write happens at submit time regardless of which path is taken afterward); the skip-path `[READY]` text names the parent-side path it will land at (`full receipts: .exo/receipts/{safe-branch}.json`), which the parent's sidecar populates by copying those bytes when the `Lifecycle::Submitted` event arrives (see `exo-node/CLAUDE.md`). The skip-path response also appends the sender-side **wake note** when the parent's listener isn't confirmed live (same `messaging::wake_note` as the messaging tools). |
| `verdict` | `Bus`+`Kv` | reviewer | A reviewer's one output → a `System(Reviewed)` message to its parent: `summary` + structured `findings` {`file`, `line`, `severity`, `body`, `suggestion`?}. Triggers reviewer teardown (handled in `exo-node`). |
| `notify_parent` | `Bus` | tl, dev, worker, reviewer | Status/failure update to `Addressee::Parent` (NOT the done-signal). Optional `reply_to` — see [Reply threading](#reply-threading). The response carries a **wake note** (`messaging::wake_note` over `Bus::wake_status`) when the recipient's `exo listen` monitor isn't confirmed live: ⚠ for a fresh not-listening snapshot, a soft "status unknown" for a stale/absent one — the message is queued durably either way. |
| `send_message` | `Bus` | root, tl | Deliver to a child by name (`to: <child>`) — **tree-edges only**; inline vs worktree is transparent. Optional `reply_to` — see [Reply threading](#reply-threading). Same sender-side wake note as `notify_parent` (a just-spawned child that hasn't armed its monitor yet shows as unknown/not-listening — expected, the message queues and drains on arm). |
| `broadcast` | `Bus` | root, tl | Flat fan-out: the same `text` delivered to **every LIVE direct child**, one `Bus::deliver` per child over the same path `send_message` uses — no role filtering, no addressing. Reads this node's own `.exo/children.jsonl` directly via `Fs` (a supertrait of `Bus`) and folds it with `exo_caps::fold_children`; a child counts as live only at exactly `ChildState::Live` — a `Submitted` child (still running, but waiting on this node's `merge`) and any terminal (`Reaped`/`Died`) child are both skipped. `text` must be non-empty. No live children is not an error — returns `"no live children"`. Returns a per-child `delivered`/`error: …` result (plus the same wake-channel note `send_message` gives when a recipient hasn't armed its listener) in both the rendered text and `data.results`. |
| `tree` | `Topology`+`Fs` | root, tl | Read-only: the caller's subtree (recursive ledger fold) + parent + per-node liveness, effective `model`, and a `(label)` for any launch-profiled node (e.g. a Kimi reviewer). **Compact by default:** shows Live + Submitted + Died and hides routine `Reaped` tombstones behind a `(k reaped hidden — pass all:true)` count; `{"all": true}` shows everything. A Submitted node renders `submitted @ <sha8>, awaiting merge` (the pending-merge queue) and keeps its liveness bracket — it's still running. A **tombstoned** node gets no `[alive]`/`[dead]` bracket, no busy bit, and no status-file lookup at all: those are pane-keyed and tmux recycles pane ids, so for a corpse the honest answer is to show nothing rather than a possibly-wrong something. **Directives audit:** a non-terminal child spawned with directives (a recorded `directives_hash`, folded through `Child`/`TreeNode`) is judged against the caller's own current bundle (loaded once via `directives::load_directives`) — `directives:ok` on a match, `directives:stale(<hash8>)` on a mismatch, or the bare `directives:<hash8>` when the caller has no bundle of its own to compare against (informational) or its own bundle failed to load (degrades with a one-line warning, never an error). A child spawned without directives gets no bit at all; terminal nodes never render one. **Wake bit:** any node with a fresh status snapshot renders `wake:listen` (its `exo listen` monitor is armed) or `wake:-` (messages to it are queuing until it arms/re-arms). **Seen bit:** every live (non-terminal) node renders `seen <age> ago` (e.g. `seen 4s ago`, `seen 32m ago`) from its status snapshot's `ts` field — how long since its sidecar last published one (the periodic publisher writes one roughly every 5s while the node is alive), so a parent can tell "thinking" from "wedged for an hour" without attaching to the pane. A node with no readable snapshot renders `seen: -`; a tombstoned node renders neither (no status-file lookup at all — see above). |

### Spawn idempotency

`birth` (the runtime's spawn path, shared by `fork_wave`/`spawn_dev`/`spawn_worker`) refuses a
duplicate `name` — including one belonging to a previously **reaped** child — before any resource
is created: no pane, no worktree, no ledger append. A spawn under a name that already resolves to
*any* fold state other than "never spawned" is rejected up front, not discovered mid-flight. This
means a spawn error for a name you *thought* was free is ambiguous from the caller's side alone —
it could be a genuine collision with a still-live sibling, or a stale belief about a name that was
already torn down. **Check `tree` before retrying** rather than blind-respawning: `tree` (pass
`all: true` to see reaped tombstones too) tells you which case you're in. Respawning under a
*different* name is always safe; retrying the same name blind is not.

## Standing directives

A node's **standing directives** are its persistent instructions to everything it spawns: plain
`.md` files in `.exo/directives/`, **local and untracked** (the repo's `.git/info/exclude` covers
`.exo/*`). They are deliberately not git-tracked — they are per-node local state, not repo content,
so they never ride a merge and never leak sideways between subtrees. A human or a parent propagates
one by ordinary message; adopting it is a file write.

`directives.rs` is the whole implementation. Every spawn path does three things with the loaded
bundle (`load_directives` is called **once per tool invocation**, not once per child):

1. **Injects** it as text into the child's spec (`Directives::apply` appends a
   `STANDING DIRECTIVES (inherited from your spawner — follow these):` section, one `## {file}`
   block per file). This is what actually makes the child obey, and it is the only mechanism that
   works for an inline worker with no worktree of its own. It reaches the **reviewer** too —
   `submit_branch` applies it to the review task, since a directive like "reject any new `unwrap()`
   in library code" is worthless if the one child judging the diff never sees it.
2. **Copies** it into a *worktree* child's own `.exo/directives/` (`copy_directives`). Untracked
   files do not materialize through `git worktree add`, so without this a mid-tree TL would inherit
   its parent's directives in its prompt but have nothing to pass further down. Best-effort: a
   failure warns and continues, because the text injection has already landed — only the child's
   ability to re-propagate is lost.
3. **Records** `Directives::hash()` onto the child's spec, which the birth path stamps into its
   `ChildRecord::Spawned.directives_hash` — so "which directives was this node born under" stays
   answerable after the fact. The hash is sha256 over the filename-sorted `(name, content)` pairs
   with each field NUL-terminated (the terminators make the encoding injective, so `[("ab","c")]`
   and `[("a","bc")]` cannot collide), and is `None` for an empty bundle: no directives is the
   *absence* of a hash, not the hash of nothing.

`load_directives` is **loud**. Only a `NotFound` on the directory itself maps to `Ok(empty)` — that
is the ordinary "this node has no directives" case. An unreadable file or non-UTF-8 content is a
hard error, because the failure it guards against is a node that *has* directives quietly spawning
a whole subtree that never received them.

### The model tier cap

All three spawn tools take an optional per-spawn `model`. It is validated against the **spawner's
own** role (`own_role` reads `.exo/node.json`) before anything is built:

- **Root** — shape only: a single bare token matching `[A-Za-z0-9._-]+`. Root is the human's own
  interactive session and may legitimately name any tier or full model id; the check exists to
  catch typos and shell metacharacters, not to restrict choice.
- **Tl** — must be in `TL_MODEL_ALLOWLIST` (`opus` / `sonnet` / `haiku`); anything else is rejected
  with an error that *names the allowlist*. Without a cap a spawned TL could seed a whole subtree
  on a costlier tier, or on a name the launcher does not understand — which otherwise fails late
  and confusingly at pane-launch time rather than at the tool call.
- **Dev / Worker / Reviewer** — unreachable (they have no spawn tools), but any override is
  rejected defensively.

`model: None` skips validation entirely and leaves behavior byte-identical to no override. An
unknown name is never passed through: tools require well-formed args.

Papers resolution fails **conservatively**: a `NotFound` on `.exo/node.json` means `Root` (the
root's papers live outside the cwd under `~/.claude/exo`, so their absence in the cwd *is* the root
signature), while corrupt or untyped papers warn and assume `Tl`. A spuriously-capped TL fails loud
and actionably; a spuriously-uncapped one silently burns tokens on the wrong tier.

A role redirected by a **launch profile** ignores the override — that profile's proxy serves exactly
one model, so overriding it would 404.

## Fold-time file boundary

A spawn spec has two distinct fields, rendered as two distinct sections, and they must never be
confused: `boundary` is prose the child reads — a `DO NOT` list rendered under `ANTI-PATTERNS (DO
NOT):` — while `file_boundary` is the allowed-paths list, rendered under a separate `ALLOWED PATHS`
section right after it. Spec authors used to put allowed paths into `boundary` (there was nowhere
else to put them — a child never saw its own `file_boundary` at all, only the parent that spawned
it did), which renders as a forbidden list and reads backwards: children halted, reading their own
work-paths as things they must NOT touch. `render_spec_prompt` now renders both, so the child sees
exactly where it's allowed to write.

`file_boundary` isn't just prose, though — `boundary.rs` (`FileBoundary`) makes it a mechanism
too: `fork_wave`/`spawn_dev` optionally persist the **same allowed-paths list** parent-side at
spawn time, and `merge` reads it back and verifies the child's actual diff against it — refusing
the fold if it's violated — **before** `ctx.merge()` ever runs. A `submit_branch` reviewer still
structurally *cannot* check this — it never sees the spawn-spec's boundary list at all, only the
diff — so the mechanical check at `merge` time is the only enforcement, and the rendered `ALLOWED
PATHS` section in the child's own spec is what lets the child avoid violating it in the first
place.

- **Persisted, not injected.** `write_boundary` writes `FileBoundary { allowed: Vec<String> }` to
  `.exo/boundaries/{child}.json`, **relative to the spawning node's own worktree** (the sidecar's
  cwd) — not the child's. The same node is the one that later calls `merge` from that same cwd, so
  this is parent-local bookkeeping, symmetric with the child ledger (`.exo/children.jsonl`), never
  materialized into the child's own worktree. Best-effort on write (mirrors
  [`write_acceptance`](#standing-directives) — a write failure only costs the fold-time check, never
  blocks the spawn).
- **Absence ≠ violation.** `read_boundary` returns `Ok(None)` when no file exists — the ordinary
  case for a child spawned with `file_boundary` omitted/empty, or one predating this mechanism —
  and `merge` treats `None` as "unrestricted, exactly as before". Only a file that exists but fails
  to parse is a loud error; a missing file is never conflated with an empty (i.e. "touch nothing")
  boundary.
- **Matching is exact-or-directory-prefix, dep-free** (`boundary::matches` — no glob crate): an
  entry matches a changed file if it equals it exactly, or is a directory prefix (the file path
  starts with the entry **+ `/`**). The separator requirement is load-bearing — `"src/lib"` must
  NOT match `"src/librs"`, only `"src/lib/…"` or `"src/lib"` itself.
- **Checked against the real diff, not the spec's claim.** `merge` resolves `Git::merge_base` for
  the branch, then unions the files touched across `Git::commits_between(base, branch)`. `merge_base
  == None` (unrelated histories — the succession escape-hatch case, folding an orphaned
  descendant's branch back into a live ancestor) skips the check with a note in the output, the same
  fail-open posture teardown already has for that path.
- **Override, don't silently allow.** A violation refuses the merge with a `CapError::invalid`
  naming every offending file and pointing at `boundary_override: true` (a new `merge` arg,
  default false) to fold anyway; the override still lands, but the output text says
  `(boundary OVERRIDDEN: k violations)` and the tool's `data` carries the violation list, so an
  override is never silent either.
- **Success is visible too.** A clean check appends `(boundary ok: N files)` to `merge`'s output —
  so a TL reading its own tool-call history can see the check ran, not just that it didn't fire.

## exo doctor

`exo doctor` is a health-check and cleanup tool for node-mode workspaces. It audits the `.exo/worktrees/` directory and identifies stale (merged) worktrees.

- **Dry-run (default)**: `exo doctor` reports merged and unmerged worktrees (and what the acknowledge pass WOULD do) but changes nothing.
- **Reclaim**: `exo doctor --fix` reclaims merged worktrees and deletes their associated branches.
- **Force**: `exo doctor --fix --include-unmerged` reclaims even unmerged worktrees (dangerous).
- **Acknowledge (part of `--fix`)**: doctor is the acknowledgment path for `Died` tombstones — it records `Reaped` for every child it reclaims AND for every `Died` child with no worktree left on disk (nothing to reclaim ⇒ fold to routine history). The ledger fold's later-record-wins rule makes `Died → Reaped` the designed transition; afterwards the default `tree` view hides them (visible under `all: true`).

Worktrees are considered reclaimable if their HEAD is an ancestor of the current branch's HEAD. The current worktree (repo root) is never removed.

The actual removal (nested-worktree walk, kill each nested child's recorded tmux pane, then
`git worktree remove --force` innermost-first) is **not** doctor's own logic — it's
`exo_runtime::Runtime::reclaim_worktree_tree`, the same code path `Spawner::reclaim_worktree` uses
at merge-time (see `exo-runtime/CLAUDE.md` § "Reclaim ordering"). `doctor.rs` constructs a minimal
root-rooted `Runtime` (a placeholder identity — doctor is a foreground CLI, not a spawned node) and
calls that method once per top-level reclaimable worktree (sorted shallowest-first; a worktree
already swallowed by an enclosing one's nested-walk is skipped, not re-removed). Branch deletion
(`git branch -D`) stays doctor-specific and runs after a successful reclaim, for every worktree
(root or nested) that was actually removed.

Every tool implements `Tool::description()`; `exo-node`'s `tools/list` emits it, so the toolset is
self-documenting — an agent learns the local-merge loop (commit → `submit_branch` → parent `merge`,
no PR/remote) from the tools it has. `submit_branch`'s preconditions are an **ordered, extensible
fn-pointer list** (`tools/submit.rs`) mirroring the role hook fn-pointers — currently `committed`
(clean tree), `needs_rebase` (branch not behind its parent's REAL git branch, read off the node's
own `.exo/node.json` `parent_branch` — prompts `git rebase <parent>`, keeps the parent's fold
conflict-free by resolving in the child's own context), and `pre_merge_checks` (project
`.exo/checks/pre-merge/*` scripts). Any check failing surfaces as a tool error the agent acts on,
before either the review-spawn or the skip-forward path.

### Run-artifact GC

Two kinds of node-mode debris never get cleaned up by anything else, so doctor is the GC path for
both, runs in the same dry-run/`--fix` pass as the acknowledgment pass above (dry-run reports, never
touches; `--fix` deletes and prints freed totals):

- **Home-dir run state** — `~/.claude/exo/{inboxes,status,papers}/{run_id}/`. `exo-node/src/inbound.rs`
  documents this honestly: nothing deletes a run's directories once it ends. A run id is classified
  **dead** iff it is not the current `EXOMONAD_SWARM_RUN_ID` AND its liveness mtime is older than
  `STALE_RUN_THRESHOLD` (6 hours) — the sidecar's status publisher writes a fresh file under
  `status/{run_id}/` every 5s while any node in the run is alive (`Runtime::status_snapshot`'s
  periodic caller), so hours of silence means the run ended, not that it's idle between turns. The
  liveness mtime is the newest mtime among the run's `status/` files when that dir exists; a run with
  no `status/` dir at all (dead before ever heartbeating) falls back to the newest mtime anywhere
  under its `inboxes`/`papers` dirs. The current run is **never** classified dead, regardless of
  mtime. `--fix` removes every dir that exists for a dead run id (`std::fs::remove_dir_all`,
  per-dir error ⇒ `eprintln!` + continue) and prints the freed byte total.
- **Repo-local dispatch spill files** — `.exo/tmp/inbox-{pid}-*.md` (written by `exo-node`'s
  `dispatch::prepare_listen_payload` whenever a message is too large to deliver inline over the
  wake channel). A spill file is
  dead iff its `{pid}` is not a live process (`/proc/{pid}` absent). The pid is parsed defensively
  from the exact `inbox-{pid}-{id}.md` shape — anything that doesn't match (including a non-numeric
  pid) is skipped, never deleted. `/tmp`-style `exomonad_buf` scratch files are a separate mechanism
  and out of scope here.

Both passes are pure-classify-then-act: `run_is_dead`/`spill_pid_from_name` are unit-tested with no
IO, and `classify_dead_runs`/`classify_dead_spill_files` are home-root-parameterized so the
integration test builds a fake `~/.claude/exo` layout under a tempdir rather than touching the real
home.

## Roles

`role_def(kind)` returns a `RoleDef<R> { tools, pre_tool_use, session_start }`; `ExoDomain::role_def`
resolves through it (the domain's `Exomonad` impl), replacing the deleted `RoleRegistry`. Hooks compose by pointing several roles at the same fn.

Every role is a Claude instance; the **model** varies per role via `ExoRole::model()` (the `RoleKind::model` seam): `Some("sonnet")` for dev/worker/reviewer leaves, `Some("opus")` for a spawned tl, `None` (inherit the launcher's default) for root only. Every *spawned* node (everything but root) gets an explicit cap — never `None` — because "inherit the launcher's default" means whatever model tier the human's own top-level session happens to be set to, which is the human's choice for their own interactive use, not a choice made for subagent work (e.g. a human running a cheap/fast model for chat must not have that silently propagate onto a spawned TL's decomposition work). The model flows `RoleKind::model()` → `BirthCore.model` → `ClaudeSpawnFlags.model` → `build_agent_command`'s `--model`.

There is no per-role "stop gate" column anymore — Claude Code's `Stop` hook is no longer wired at
all (see "The gates" below for why). What each role needs from convergence/liveness now comes from
explicit tool calls (`submit_branch`, `verdict`) and the watchdog loop's wall-clock checks, not a
turn-boundary hook.

| Role | agent | tools |
|------|-------|-------|
| **Root** | Claude (inherits the launcher's default — the human's own session) | fork_wave, spawn_dev, spawn_worker, merge, send_message, broadcast, tree, request_review, amend_boundary |
| **Tl** | Claude (Opus) | spawns, merge, notify_parent, send_message, broadcast, submit_branch, tree, request_review, amend_boundary |
| **Dev** | Claude (Sonnet) | notify_parent, submit_branch |
| **Worker** | Claude (Sonnet) | notify_parent |
| **Reviewer** | Claude (Sonnet, or a launch-profile brain) | verdict, notify_parent |

The **reviewer** and ephemeral in-pane **worker** roles carry a **launch profile** (`ExoRole::launch_profile_env_prefix` → `Some("EXO_REVIEWER")` / `Some("EXO_WORKER")`): their Claude can be redirected to a non-default Anthropic-compatible endpoint/model (e.g. Kimi via a local `claude-code-proxy`) — still a Claude process, so Teams/hooks/MCP are unchanged (the old Gemini-worker slot, now Kimi). Configure per-role in `.exo/config.toml` (the convenient path) — the **named-brain shorthand** is the common case:
```toml
[launch_profile]
reviewer = "kimi"          # built-in named brain → proxy endpoint + model + label
worker   = "kimi"          # no auth_token needed: the proxy holds the OAuth
```
`"kimi"` is the one built-in brain today (`config.rs::named_brain` — the **only place a vendor is named**; the runtime/seam stays backend-agnostic). For a custom/unknown backend, the **full-table form** still works and overrides the shorthand:
```toml
[launch_profile.reviewer]
base_url = "http://localhost:18765"
model = "kimi-for-coding"
auth_token = "sk-…"        # OPTIONAL — omit for a local proxy; a real key can also live in the shell env (env wins)
label = "kimi"             # tags the window + tree
```
`config.rs` resolves each profile (`Named` shorthand via `named_brain`, an unknown name is a loud skip) and flattens to `EXO_<ROLE_UPPER>_*`; `init.rs` embeds them in the root launch (a matching shell `EXO_*` overrides), the tree propagates, and `exo-runtime` resolves them (see its CLAUDE.md — the profile **activates on `BASE_URL`**, the token is optional). Omit a role ⇒ it stays the default Sonnet. Adding another role/backend = one arm in `launch_profile_env_prefix` (+ optionally a `named_brain` entry).

## The review gate (how `submit_branch` → `merge` is gated)

**Reviewers are opt-in, off by default** — `review_enabled` in `.exo/config.toml` (inherited down
the tree onto every node's papers exactly like `yolo`/`wrap_nix`; unset ⇒
`NodePapers::DEFAULT_REVIEW_ENABLED = false`). Reviewers aren't a fully-cooked feature yet (see the
abandonment-timeout and nested-teardown history in this file), so a project turns them on
deliberately rather than getting them by surprise. The default is inherited tree-wide, but `fork_wave`
and `spawn_dev` each take a per-child `review: Option<bool>` override so a TL can turn the gate ON
for one subtree doing subtle cross-cutting work and OFF for another doing mechanical leaf work,
rather than the choice being all-or-nothing for the whole session. `submit_branch` reads its own
`review_enabled` (`.exo/node.json`, via `Fs`) at call time; when it's off (or the agent explicitly passes
`dangerously_skip_reviewer: true`), it forwards `[READY]` straight to the parent, flagged as
unreviewed. The body is **compact flag form**, not paragraphs — a parent merging a wave reads these
back to back, so the state has to be scannable in one line:

```
[READY] branch `X` @ {sha} — review: SKIPPED-BY-AGENT (dangerously_skip_reviewer; inspect the diff yourself, be more suspicious than usual)
[READY] branch `X` @ {sha}
note: {the submitter's note}
{the receipts block, when receipts were passed}
```

The two cases still read differently by *why* — the loud "be more suspicious" framing appears only
when the agent itself opted out of a normally-on gate. The plain config-off case carries **no
`— review: …` suffix at all**: reviewers are off by default, so that's the common case, and a
"disabled (config)" tag on every single `[READY]` a project ever produces is pure noise, not
signal — a parent reading a wave back to back gains nothing from being told the default is still
the default. The `summary` line (`[READY] {branch}` / `[READY skipped] {branch}`) and the typed
`Lifecycle::Submitted { branch, sha, reviewed }` kind are unchanged.

When reviewers ARE enabled: a node commits, then calls `submit_branch`. It runs the checks, then
spawns a **reviewer** (a full Sonnet Claude in its own worktree branched off the under-review code)
handed the diff + `.exo/acceptance.md` + (when the submitter passed any) a **SUBMITTER RECEIPTS**
section — the same `render_receipts_summary` block rendered on the parent-bound `[READY]`, so
there is one renderer, not two. Its task prompt is explicit that review is **read-only** —
judge the diff, don't re-run the build/test suite — because a reviewer has a 30-minute wall-clock
abandonment timeout (`REVIEW_ABANDON_TIMEOUT`) and a cold build routinely blows well past that,
burning the whole round for nothing (see the tidepool forensics in "The gates" below). The task
also carries two prompt-level lenses beyond plain correctness — **SCOPE** (check the diff against
any ALLOWED PATHS the acceptance criteria name; an undeclared out-of-scope file is an Error, a
declared-and-justified one is at most a Warning) and **DUPLICATION** (read the CLAUDE.md of every
touched top-level directory; an undeclared reimplementation of an existing mechanism is an Error)
— and, when receipts were passed, an instruction to audit the `deviations` field against the diff
(an undeclared deviation is a finding; a declared one is context). These are prompting only —
no new mechanical scope/dup computation exists anywhere in this path; the recorded file boundary
can be wrong, and judging that is the parent's call, not the reviewer's.
**Cross-round continuity:** `submit_branch` reads the latest `.exo/reviews/{safe-branch}.json` and
appends any unresolved Error findings from the prior round to the reviewer's task string.
The reviewer calls `verdict`, which rides the bus as a `System` message to the submitter's
**sidecar**:
- **Reviewed** (no Error-severity findings) & sha==HEAD → the sidecar escalates `[READY]` to the parent — *no LLM turn*.

Both `[READY]` deliveries — this approve-escalation and `submit_branch`'s skip/no-review path — ride
as a **typed `Lifecycle::Submitted { branch, sha, reviewed }`** (`reviewed: true` here, `false` on the
skip path) rather than a plain `Chat`. The prose body is unchanged; the type is what lets the parent's
sidecar append a durable `ChildRecord::Submitted` to its own ledger before re-showing that prose, so
the pending-merge queue survives the parent's context window instead of living only in its scrollback.
- **Reviewed** (with Error-severity findings) → findings are rendered and delivered into the submitter's LLM to address, then re-submit (new sha → fresh reviewer). **The verdict handler persists the round to the log.**
- **Aborted** (the reviewer never produced a verdict — see `handle_review_tick` below) → the
  submitter is told explicitly NOT to spawn another reviewer (a second one is likely to hit the same
  wall) and to re-submit with `dangerously_skip_reviewer: true` instead.

When reviewers are enabled, `submit_branch` never delivers `[READY]` itself except through the
skip path, so the gate is **structural** — the LLM has no other tool that fabricates approval. The
reviewer is torn down (best-effort) as soon as the `verdict` (or the abandonment timeout) is processed.

**Severity calibration.** The reviewer's own steering protocol (`protocol::REVIEWER`) frames
`error` around one question: would the parent be right to REFUSE this fold? If the reviewer would
merge it itself, it is not an error — and when unsure between `error` and `warning`, it picks
`warning`. A false block costs the submitter a full round-trip; a missed nit costs nothing. This
is a calibration change, not a new check: it exists because the reviewer's kill-condition is a
false-positive Error block, not a missed nit.

## Receipts and the transfer proof

`receipts.rs` — the typed half of a `[READY]`. A submit used to be prose ("did the thing") that a
parent could only take on faith; `Receipts { commit_tested, verify_commands_run, metrics:
Vec<LabeledValue>, deviations }` makes the claim checkable. Every field is `#[serde(default)]` —
receipts are an upgrade to the submit path, not a new mandatory ceremony. Fields are **typed, never
free-form JSON**: the tool schema has to inline cleanly (`schema_json` inlines subschemas), and a
`Value` field would be an unbounded blob smuggled through a bounded channel.

The load-bearing part is the **transfer proof**. A node verifies at one commit and submits at
another — a rebase, a follow-up fix, "one more small thing" all land in that gap, and the parent
could not previously see it at all. `commit_tested` closes it via `Git::commits_between`:

| `TransferProof` | When | What the parent is told |
|-----------------|------|-------------------------|
| `AtHead` | `commit_tested` prefix-matches HEAD (≥7 chars) | `tested@HEAD {sha}` — the strongest receipt available |
| `Moved` | commits landed in `tested..HEAD` | how many, the files they touched, and whether any **overlap the diff being merged** (`Some(empty)` = the real, reassuring "none overlap"; `None` = no diff base resolved, so the question is unanswerable and says so) |
| `Unverifiable` | the tested sha doesn't resolve / git errored | `treat as untested transfer` — loud, in both the message text and the tool output |

`Unverifiable` is a **variant, not a flag**, precisely so the case cannot be silently dropped into a
reassuring blank; the renderer has to handle it.

`render_receipts_summary` is pure, total, and **deliberately truncating** — per-string
(`MAX_STRING_RENDER_BYTES`), per-list (verify commands / metrics / deviations / file lists), each
clipped on a char boundary. Every truncation leaves a **visible marker** (`…`, `(+N more)`): a
reader must always be able to tell "that's all there was" from "there was more". Size is guarded at
both ends, and both are LOUD `CapError::invalid` failures rather than a silent trim — any single
receipt string over `MAX_FIELD_BYTES` is rejected **before the preconditions run and before any
delivery** (on both paths, including the skip path where receipts wouldn't otherwise render), and a
rendered block over `MAX_RENDERED_BYTES` is rejected before it reaches `MessageBody::new` (which caps
at 4 KiB and *errors* on overflow — the budget leaves room for the `[READY]` line and the note).

Receipts ride the **message text + `ToolOutput` data only**. They are deliberately NOT in
`Lifecycle::Submitted`: `exo-caps` sits below this crate and cannot name a domain type. Full
untruncated receipts go in the tool's `data`; only the message text is budgeted.

**Not yet:** receipts render on the skip/no-review path only. On the reviewer-enabled path the
`[READY]` is escalated by the sidecar on an approve-verdict, which never sees the submitter's
receipts — validation still runs, but the block is not carried. Closing that means routing receipts
through the review round, not a tweak here.

## Reply threading

`Message.reply_to` (an `Option<String>` pointing at another message's envelope `id`) has been
rendered as `re:` in the recipient's delivery header for a while, but nothing could populate it —
both messaging tools hardcoded `None`. `notify_parent` and `send_message` now take an optional
`reply_to`, so an agent that receives three messages and answers the second can say which one it is
answering. Carried verbatim; omitted ⇒ `None` and byte-identical prior behavior.

**Ids are reference-only, never a dedup key.** The inbound cursor advances only after a successful
last-hop delivery, so redelivery is at-least-once *by design* and a redelivered line arrives with its
ORIGINAL id — an "already seen" check would swallow exactly the retry the protocol exists to
guarantee. Nothing here validates, generates, or looks up the referenced id.

## The gates

- **`pre_tool_use`** — default-**ALLOW** antipattern *nudge* (NOT a security gate). Currently one rule: deny `git add .` / `git add -A` (stage by path). Can `Deny` with guidance or `Modify` to rewrite.
- **`session_start`** — identity bootstrap (the node-identity context AND the wake-channel arming instruction are appended by `exo-node`'s engine-side SessionStart arm — `hook.rs::listen_instruction`, the exact `Monitor { command: "exo listen --papers <abs>" }` call; the domain gate itself still returns `default()`). The role's **steering protocol** (`RoleKind::protocol`, mapped to a `protocol.rs` const, override-or-const) is delivered via the launch-time `--append-system-prompt` flag at spawn.

There used to be a third gate, `stop` (Claude Code's `Stop` event), with per-role variants
(`stop_allow`/`stop_notify`/`stop_dev`/`stop_reviewer`) — a TL/dev dirty-worktree exit-block and a
reviewer verdict-or-abort check. **It was removed entirely** (not neutered to `Allow` — a node's CC
settings no longer register `Stop` at all, so it's never invoked). Root cause, found live in a
production swarm: `Stop` fires on **every turn-end**, including a node legitimately yielding to wait
on a backgrounded async task (e.g. a reviewer polling a `cargo build`). It cannot distinguish
"genuinely done" from "paused" — confirmed against Claude Code's own docs, which offer no signal that
can (`SessionEnd` is the only turn-boundary-independent event, but it can't gate/block and has
undocumented gaps around hard kills). Every decision built on `Stop` was provably wrong some of the
time: a reviewer got killed ~1 second into a build wait, three submit rounds in a row, on the same
branch, before ever producing a verdict; a TL/dev got nagged "commit first" mid-async-wait; and the
`ChildIdle` busy-bit it fed produced false "subtree idle" reports that propagated up the tree.

What replaced it — each protection moved to a signal that's actually true regardless of turn
boundaries, not a hook:
- Reviewer "done" → the `verdict` tool (unchanged) — it was always the real signal; `Stop` was only
  ever consulted for the *negative* case.
- Reviewer "abandoned" → `review.rs`'s `handle_review_tick`, a wall-clock timeout
  (`REVIEW_ABANDON_TIMEOUT`, 30 min) run by `exo-node`'s watchdog loop (`Exomonad::handle_tick`),
  checked against real elapsed time, not a turn count. Delivers the same `ReviewAborted` the old
  `stop_reviewer` sent; the parent-side handling (`handle_review_system`) is unchanged.
- "Uncommitted work before converging" → already independently enforced by `submit_branch`'s own
  precondition check (`tools/submit.rs`) at the moment it actually matters (tool-call time, not
  turn-boundary time) — no Stop-time backstop needed.
- "Is my subtree still working" (`ChildLiveness`) → collapsed to pure pane-existence
  (`Tmux::list_panes`), dropping the busy-bit entirely. Coarser than the old claim, but the old claim
  was false; this one isn't. Its only remaining consumer is the cooperative-shutdown `Defer`
  response's cosmetic wording — the actual clear-to-reap gate was always `Topology`'s recursive pane
  walk, unaffected.
- Cooperative-shutdown reap-on-idle (`try_reap`) lost its `Stop`-triggered check point; the watchdog
  loop now calls it unconditionally every tick instead (it's idempotent and independently gated on
  `shutdown_pending` + subtree-clear, so this is a strict improvement, not a new heuristic).

## Gaps / not-yet

- **Reviewers:** now config-gated (`review_enabled`, off by default) instead of always-on. Still missing: a two-way colleague back-channel (submitter→reviewer reply) needs `send_message` on dev.
- `pre_tool_use` is intentionally minimal (one nudge); classic exomonad's richer antipattern set + PII rewrite are not ported.
- **Authoring-DSL Phase A LANDED** — typed `Tool` + `ErasedTool` flip: 9 hand-adapters deleted, roster uses `tool(X)`. Phase B (gate/observer stop pipelines, #20426 structural) is moot now that Stop-hook gating is gone; see [`docs/decisions/exo-authoring-dsl.md`](../../docs/decisions/exo-authoring-dsl.md).
