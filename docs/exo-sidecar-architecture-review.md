# Exo MCP Sidecar Architecture Review

**Status:** Review / triage input, 2026-08-08. Not an accepted decision record.

## Purpose

This is a read-only architectural scan of the new `exo` surface, centered on the per-agent MCP
sidecar. It follows the live paths across `exo`, `exo-framework`, `exo-node`, `exo-caps`, and
`exo-runtime` rather than judging modules in isolation:

- MCP initialize/list/call → typed tool adapter → capability implementation
- spawn tool → `ExoSpawn` → runtime spawn → node manifest/config → sidecar bootstrap
- bus append/spill → inbox watch/cursor → internal routing → tmux delivery
- hook CLI → socket client/server → gate
- submit → reviewer spawn → verdict → parent handling → teardown
- cooperative/forced shutdown → cascade → reap

The aim is not generic minimalism. ExoMonad's Haskell vocabulary is part of its model and voice.
The hylomorphism over context windows and worktrees is real: spawn/scaffold is the unfold; merge and
integration are the fold. Terms such as **hylo**, **unfold**, **fold**, and **convergence** compress
actual structure and should remain.

The target is accidental complexity: false invariants, duplicated paths, abstractions without a
present axis of variation, temporary bridges that look permanent, and terminology that merely
renames an operation without compressing a concept.

## How to triage this review

Active development changes how incomplete code should be interpreted. Do not collapse every rough
edge into “remove it.” Classify each item as one of:

- **BUG** — a claimed invariant is currently false or a live path can behave incorrectly.
- **FINISH** — the destination is known and justified, but connecting implementation is missing.
- **SIMPLIFY** — the behavior is useful, but its realization has unnecessary machinery or duplication.
- **DECIDE** — the architecture depends on a future requirement that should be named before more
  framework is built around it.

“Active development” is a reason to classify unfinished scaffolding accurately, not a blanket
defense of it. A temporary bridge is reasonable when its destination, removal condition, and owner
are explicit. Otherwise it tends to fossilize.

## Executive assessment

The useful core is straightforward:

1. typed MCP tools over explicit IO interfaces;
2. one sidecar per agent;
3. a durable tree-edge inbox with tmux as the final delivery hop;
4. worktree-backed spawn and local merge as unfold/fold.

Most of the baroque character is concentrated in four places:

- bespoke protocol and durability machinery whose guarantees are weaker than claimed;
- shutdown state inferred from intent rather than observed process state;
- an unfinished reviewer gate that drives much of the supposedly general engine seam;
- multi-domain generality when **role** may be the real axis of variation.

## Findings

### 1. BUG — `PIPE_BUF` does not prove regular-file append atomicity

`exo-runtime/src/bus.rs` spills entries larger than 4096 bytes because it treats `PIPE_BUF` as an
atomic-write guarantee for the JSONL inbox. `PIPE_BUF` is the guarantee for pipes/FIFOs, not regular
files. Tokio's `write_all` also promises completion, not that the buffer becomes exactly one
`write(2)` call.

The claim-check mechanism therefore adds spill directories, pointer entries, resolution, retained
artifacts, and tests without establishing its stated multi-writer invariant.

Possible honest implementations:

- one inbox per sender, eliminating concurrent writers;
- an advisory lock around each append;
- a recipient-owned Unix socket, retaining files only for durability;
- a small recipient-owned append service.

Malformed-tail recovery remains useful if JSONL stays. The `PIPE_BUF` proof does not.

### 2. BUG — `ChildExited` is emitted before the child exits

`exo-node/src/inbound.rs::try_reap` sends `Lifecycle::ChildExited`, sleeps for the grace period, and
then attempts to kill its own pane. The parent treats receipt as an authoritative gone-set and may
filter the child out even while topology still reports its pane alive.

If the pane kill is delayed or fails, an ancestor can conclude its subtree is clear and begin its
own reap while the child remains alive.

Process existence should be observational truth. If a pre-kill signal is useful, call it `Exiting`.
Reserve `Exited` for an observer that has confirmed pane death. Better still, let the parent use its
existing pane probe rather than maintain a second authoritative set.

### 3. BUG — a failed inbox delivery can wait forever for another write

On a routing failure, `process_inbox` leaves the cursor unchanged and returns, which preserves the
entry. But `watch` calls it again only after another filesystem notification. If no later message is
written, the failed entry is never retried.

The watchdog already supplies a clock. The inbox loop can select between notification wakes and a
bounded retry timer. The `notify` callback's bounded-channel `blocking_send` can also be replaced by
a capacity-one nonblocking signal, `Notify`, or dirty bit; wakeups only need coalescing semantics.

### 4. BUG / FINISH — “graceful when idle” has no idle signal

After removal of the unreliable Claude `Stop` hook, cooperative shutdown still tells an agent it
will be reaped “when you go idle.” In reality the watchdog calls `try_reap` every 60 seconds, and
`try_reap` checks only shutdown-pending state and child topology. A leaf can therefore be killed on
the next tick whether or not it finished wrapping up.

Either finish a truthful graceful protocol—an explicit `shutdown_ready` tool/event is the clearest
option—or describe and implement it as timeout-based termination with notice. Do not infer semantic
idleness from a wall clock.

### 5. BUG / SIMPLIFY — the “rmcp” server is a partial hand-written JSON-RPC server

`exo-node/src/outbound.rs` is described as rmcp/stdio but parses raw `serde_json::Value` and shapes
responses manually. Current edge behavior includes:

- malformed JSON is dropped instead of receiving JSON-RPC parse error `-32700`;
- `jsonrpc` is not validated;
- a missing/non-string method becomes an empty method name;
- malformed or absent arguments often become `{}`;
- tool execution errors become JSON-RPC internal errors rather than MCP tool results with `isError`;
- `ToolOutput.data` is converted into a second pretty-printed text block instead of structured content.

Either adopt a maintained MCP implementation or explicitly own and test a rigorous limited server.
Do not call a bespoke subset “rmcp” if rmcp is not actually providing the wire behavior.

### 6. SIMPLIFY / FINISH — hook invocation bootstraps too much, twice

The `exo hook` command first performs full sidecar bootstrap merely to initialize logging. The hook
path then either bootstraps again for `SessionStart` or rereads papers to locate the hook socket.
Bootstrap installs a parent-death signal, reads ambient process state, creates inbox directories,
constructs a runtime, and emits a diagnostic; it is not a cheap identity parser.

Split out a small, pure `NodeIdentity`/`HookAddress` read. Logging metadata and socket location need
only that. Full `NodeContext` assembly belongs to the long-lived sidecar.

PreToolUse verdict shaping is also duplicated between the one-shot hook handler and hook-socket
server. Make it one pure formatter shared by both paths. Keeping one-shot `SessionStart` to survive
the cold-start socket race is a reasonable explicit bridge.

### 7. BUG / CLEANUP — retired Agent Teams behavior is still enabled

The new delivery architecture says Agent Teams delivery was removed and tmux paste is the sole last
hop. The spawner nevertheless sets `CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS=1` for worktree children,
with a comment saying it enables the removed Bus→Teams path. Runtime comments also retain teamout
and team-isolation language.

Remove the environment mutation unless a currently exercised feature has a separately demonstrated
dependency on it. If `fork_session` needs some Teams-related behavior, name and scope that dependency
instead of globally enabling a retired transport.

### 8. BUG / FINISH — domain code bypasses the capability boundary

`submit_branch` declares a `Process`/`Fs`-based policy surface but directly uses
`std::fs::read_dir` for `.exo/checks/pre-merge`. This means mocks do not control all IO, relative-path
behavior depends on process CWD, and the claimed compiler-enforced IO wall is not true in practice.

Add the required directory operation to `Fs`, or deliberately weaken the architectural claim:
capabilities are test seams and design conventions, not an enforced security boundary. The current
halfway statement is the least useful one.

### 9. DECIDE — “domain” may be speculative; role may be the actual variation

The framework currently pays for a domain-generic engine through:

- `Exomonad` with four associated types;
- `PolicyCaps`, the all-capability bound union;
- erased `RoleRecord` JSON in node manifests and typed reconstruction at bootstrap;
- `DomainPayload(String)` and another deserialize step inside a single-process composition;
- a bespoke `SystemCtx` facade and `SystemOutcome` lifecycle return;
- boxed futures and hook function-pointer tables.

But there is one binary domain, one concrete runtime, and one domain-specific internal message
family: review. The meaningful differences visible today are roles: root, TL, dev, worker, reviewer.
Those roles vary tool sets, model, protocol, launch profile, spawn kind, and periodic behavior.

Before extending “multi-domain” machinery, answer:

> What is the second domain, and how does it differ in a way that is not already a role?

If the intended second/third “domain” is really another agent archetype with a different tool set or
behavior, model it as a role. `RoleDef` is already the natural product-level extension point.

A distinct domain is justified only when another composition supplies a meaningfully different
closed world: different role type, spawn intent, internal protocol, and policy table that should run
on the same engine. Until such a consumer exists, a concrete `ExoSurface`/role registry would keep
the useful typed-tool abstraction without erase/retype machinery.

This is a **DECIDE**, not an automatic deletion request. If a real second domain is imminent, name it
in the decision record and use it as the acceptance test for every generic seam.

### 10. FINISH / DECIDE — the optional reviewer feature dominates the core seam

Review is off by default and documented as unfinished, yet it motivates or exercises a large share
of the general architecture:

- reviewer role, spawn, worktree, pane, and teardown;
- `DomainSystem`, `DomainPayload`, `SystemCtx`, and `SystemOutcome`;
- verdict KV flags and watchdog domain ticks;
- review-log read/modify/write and cross-round continuity;
- abandonment timeout and dangerous bypass behavior.

This may be deliberate scaffolding. Judge it against a named final form:

- Will review become a reliable default gate?
- Must a reviewer remain a full worktree-bearing node?
- Is cross-round review-log continuity a product requirement?
- Should timeout recovery retry/recover, or permanently advise bypass?
- Does the submitter need a two-way colleague channel?

If yes, finish those properties and let review justify the machinery. If not, isolate it behind a
narrow optional `ReviewGate::submit` service rather than making the general sidecar ontology bend
around it. The current abandonment policy—“do not retry; dangerously skip”—is especially important:
it makes a large structural gate degrade to bypass at the point it fails.

### 11. SIMPLIFY — child and branch identity are redundantly exposed

`merge` accepts a branch plus an optional child name, then guesses the child from the last branch
segment. Branch sanitization can destroy the original name, which is why the optional override
exists. The child ledger already stores the authoritative relationship.

Prefer one stable child handle/name. The runtime can resolve that child's recorded branch and
teardown resources. Do not ask the LLM to preserve two correlated strings when one authority already
exists.

### 12. SIMPLIFY / FINISH — identity and ambient paths are duplicated

`NodeContext` and `Runtime` both carry pieces of identity and routing state: pane, parent inbox,
run ID, path, branch, kind, and role split across the two. Operational code then rereads `$HOME`
with inconsistent behavior: bootstrap fails loudly, while other paths warn or silently fall back to
`.`.

Resolve a `NodeIdentity` plus `RuntimePaths` once during bootstrap and carry it through. No live
operational path should have to rediscover home, inbox, papers, status, or socket roots from ambient
environment.

## Terminology guidance

Keep terminology that compresses a real concept; cut terminology that merely renames a struct or
step.

### Keep

- **hylomorphism / hylo** — the whole recursive computation
- **unfold** — scaffold, decompose, and spawn
- **fold** — merge, integrate, and surface upward
- **convergence** — the process by which child work becomes parent state
- **sidecar**, **node**, **role**, **worktree**, **inbox**, **capability** — each currently marks a
  useful engineering distinction

### Reconsider

| Current term | Plainer alternative | Why |
|---|---|---|
| papers | node manifest / node config | “Papers” adds flavor but forces translation at every IO edge. Keep only if that flavor is intentionally worth it. |
| birth / birth core / birth finish | spawn / spawn plan / complete spawn | The hylo's unfold is meaningful; local helper phases do not all need metaphorical names. |
| roster | tool registry | Both are understandable; use one consistently. |
| policy | tools and hooks | “Policy” is useful only where a real rule/decision is being made. |
| domain system | internal event | Today it primarily means reviewer events. |
| ingestion entry | inbox entry | The entry is already inside an inbox abstraction. |
| lifecycle | child/shutdown event | Prefer the actual event family where possible. |
| synthetic persona | system sender | The implementation distinction need not become product vocabulary. |
| brain | backend/model profile | Charming in UI copy; less precise in config/runtime architecture. |
| N1/N2a/N2b/N4/N5 | module or operation names | Wave coordinates are useful during a build plan, but become archaeology once the code is the map. |

The line is semantic compression. **Fold** means more than `git merge`: it includes recursive
integration and upward accumulation. **Birth finish** mostly means “the latter half of spawn.”

## Recommended order

1. Fix shutdown truth: do not emit `Exited` before observed exit.
2. Replace the `PIPE_BUF` claim with a real multi-writer design.
3. Make inbox retry independent of future filesystem writes.
4. Make graceful shutdown explicit or honestly timeout-based.
5. Replace or rigorously finish the hand-written MCP transport.
6. Delete the retired Teams mutation and stale path.
7. Split identity parsing from sidecar bootstrap and unify hook shaping.
8. Restore the filesystem capability boundary.
9. Make child identity authoritative in merge/spawn APIs.
10. Consolidate node identity and runtime paths.
11. Write the reviewer feature's intended final state and finish or isolate it.
12. Decide whether `domain` has a concrete second consumer or whether `role` is the real extension axis.

## Meta-level design tests

Use these questions when reviewing new sidecar machinery:

### Is the abstraction paying rent now?

- What are its two current implementations or consumers?
- If there is only one, what named near-term consumer sets its shape?
- Would deleting the abstraction lose a meaningful invariant, or only future flexibility?

### Is the state observed or inferred?

- Pane existence is observable; “idle,” “finished,” and “exited soon” are interpretations.
- Prefer explicit protocol messages for semantic state and OS probes for process state.
- Never promote intent into an authoritative fact before the effect succeeds.

### Does the mechanism prove its claim?

- Write the exact guarantee beside the primitive that supplies it.
- Distinguish process-crash durability, machine-crash durability, append atomicity, and retry liveness.
- Tests that encode an assumption do not validate the operating-system guarantee behind it.

### Is this scaffolding or fossilized transition code?

For every temporary dual path, document:

- the target steady state;
- why the bridge exists;
- what event allows its removal;
- which path is authoritative meanwhile.

### Is terminology compressing thought?

Project language can and should be opinionated. Preserve terms that reveal the recursive model.
Avoid requiring a glossary to understand ordinary IO, serialization, or helper-function phases.

### Is failure behavior consistent with feature importance?

If a feature requires several crates and protocols but degrades immediately to bypass on failure,
either its recovery path is unfinished or its architectural weight is too high. Critical gates need
critical-grade recovery; optional hints should remain structurally small.

## Closing view

The hylo is not the baroque part. It is the simplifying idea: context windows unfold into isolated
worktrees and fold back through integration. The review should protect that model by making the
mechanics beneath it more literal. Spawn should mean spawn, exit should mean observed exit, an inbox
should have a defensible delivery guarantee, and a role should remain the unit of variation unless a
genuinely different domain arrives.
