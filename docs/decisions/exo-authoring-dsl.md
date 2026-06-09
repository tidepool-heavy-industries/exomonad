# exo authoring DSL: typed tools + hook pipelines (T3.7)

**Status:** accepted 2026-06-09 (design; reviewed adversarially, mechanics compile-proven in a
scratch crate — see [Review provenance](#review-provenance)). Implementation pending.
Builds on [exo-framework-domain-split](exo-framework-domain-split.md) and
[exo-trait-refactor](exo-trait-refactor.md); resolves the deferred "builder/trait reshape of
`RoleDef<R>`" follow-on those docs banked, and closes `exo/CLAUDE.md`'s "authoring-DSL polish"
gap bullet.

## Problem

The north star: `exo` should read like an xmonad config — a minimal typed-DSL usage crate over
a big reusable framework (the Rust analog of Classic's Haskell-WASM split). Three kinds of
ceremony/opacity stand between today's `exo` and that:

1. **Nine hand-written ~20-line erased adapters** (`impl Tool<R> for X` in every `tools/*.rs`),
   verified pure ceremony: `name`/`description`/`schema` echo statics, `call` is always
   `ok_json(Self::run(ctx, parse(j)?).await?)`.
2. **Monolithic per-role hook fns** (`stop` / `stop_allow` / `stop_notify` / `stop_reviewer`)
   that interleave orthogonal concerns and each hand-roll an error policy. They are *literally*
   compositions: `stop` = clean-gate then idle-announce; `stop_notify` = the announce alone;
   `stop_allow` = the empty composition.
3. **An opaque policy table**: `RoleDef` stores bare fn-pointers, so the roster *names* each
   role's policy but doesn't *show* it — you must chase the fn to learn that Dev never blocks
   at stop.

## The design sentence

> **A `RoleDef` is pure data: a tool roster, two hook pipelines, an observer list, and a
> session_start fn. The framework owns the folds and the JSON erasure; the domain contributes
> typed tools and typed stages.**

---

## The authoring surface — what `exo` reads like

### A tool file (`tools/tree.rs` shape)

```rust
pub struct Tree;

#[derive(Deserialize, JsonSchema)]
pub struct TreeArgs { /* doc comments become schema descriptions, as today */ }

#[async_trait]
impl<R: Topology + Fs + Send + Sync> Tool<R> for Tree {
    const NAME: &'static str = "tree";
    const DESCRIPTION: &'static str = "Show your subtree: children, liveness, parent.";
    type Args = TreeArgs;

    async fn run(ctx: &R, args: TreeArgs) -> CapResult<ToolOutput> {
        // exactly today's inherent `run` body — unchanged
    }
}
```

The impl header's cap bounds remain the tool's least-privilege spec (unchanged doctrine). The
hand adapter below it is **deleted**. The existing inherent `run` fns already have this exact
signature (receiverless, `CapResult<ToolOutput>`), so the 20 typed test call sites
(`Tree::run(&mock, args)`) compile unchanged (trait import aside; 3 messaging tests that call
the *erased* `tool.call(...)` on the concrete value need a one-line rewrite each).

### The gates file (`gates.rs` shape) — gates and observers, not monoliths

```rust
/// PreToolUse rule: deny `git add .` / `git add -A` (stage by path). Default-allow nudge.
pub fn deny_git_add_all<'a, R: Send + Sync>(_ctx: &'a R, input: &'a HookInput)
    -> BoxFuture<'a, CapResult<HookDecision>> { /* today's body, Ok-wrapped */ }

/// Stop GATE — may Block: hold exit while the worktree is dirty (a parent merges the
/// *branch* off disk; uncommitted work is invisible to that merge). Errors propagate;
/// the fold fails open.
pub fn require_clean_tree<'a, R: Git + Send + Sync>(ctx: &'a R)
    -> BoxFuture<'a, CapResult<StopDecision>> { /* is_clean? → Allow/Block */ }

/// Stop OBSERVER — cannot block (the type returns `()`): when the subtree is quiescent,
/// deliver ChildIdle to the parent. Owns its errors (log + swallow), as today.
pub fn announce_idle<'a, R: Bus + ChildLiveness + Send + Sync>(ctx: &'a R)
    -> BoxFuture<'a, ()> { /* today's stop_notify body */ }

/// Stop OBSERVER (reviewer): no `verdict_produced` flag → deliver ReviewAborted.
/// Bias-LOUD: a kv Err counts as no-verdict (a spurious re-submit beats a silent stall).
pub fn abort_if_no_verdict<'a, R: Bus + Kv + Send + Sync>(ctx: &'a R)
    -> BoxFuture<'a, ()> { /* today's stop_reviewer body */ }
```

Each stage's cap bound narrows to what *it* touches (`require_clean_tree`: `Git` only;
`announce_idle`: `Bus + ChildLiveness` only) — per-stage least-privilege, same doctrine as
tools. Today's monolithic `stop` demanded the union.

### The roster (`roles.rs` shape) — the fully legible policy table

```rust
pub fn role_def<R: PolicyCaps>(kind: ExoRole) -> RoleDef<R> {
    match kind {
        // Root is the human-facing top: no parent, folds children locally.
        ExoRole::Root => RoleDef {
            tools: vec![tool(ForkWave), tool(SpawnGemini), tool(SpawnWorker),
                        tool(Merge), tool(SendMessage), tool(Tree)],
            pre_tool_use: vec![deny_git_add_all],
            stop: vec![],              // never gate the human's session
            stop_observers: vec![],    // no parent to signal
            session_start,
        },
        ExoRole::Tl => RoleDef {
            tools: vec![/* … */],
            pre_tool_use: vec![deny_git_add_all],
            stop: vec![require_clean_tree],
            stop_observers: vec![announce_idle],
            session_start,
        },
        ExoRole::Dev => RoleDef {
            tools: vec![tool(NotifyParent), tool(SubmitBranch)],
            pre_tool_use: vec![deny_git_add_all],
            stop: vec![],              // STRUCTURAL #20426: a Gemini role has NO stop gates
            stop_observers: vec![announce_idle],
            session_start,
        },
        ExoRole::Worker => RoleDef { /* …, stop: vec![], stop_observers: vec![announce_idle], … */ },
        ExoRole::Reviewer => RoleDef {
            tools: vec![tool(Verdict), tool(NotifyParent)],
            pre_tool_use: vec![deny_git_add_all],
            stop: vec![],
            stop_observers: vec![abort_if_no_verdict],
            session_start,
        },
    }
}
```

Every role's complete policy is *visible data* at the single table. `vec![]` is a meaningful
policy statement, not an omission — for Gemini roles it IS the safety invariant: "never block
a Gemini at stop" (gemini-cli #20426) upgrades from body discipline to a **total structural
assertion** (`rd.stop.is_empty()` for every role with `agent_type() == Gemini`), plus the
behavioral test. (Caveat for the legibility claim: if stages ever go phase-conditional over
`Kv`, the table shows stage names, not phase-dependent behavior — legibility is of the
*composition*, not the bodies.)

---

## The framework contract (`exo-framework`)

### `tool.rs` — typed authoring surface + erased runtime surface

```rust
/// Typed authoring surface — what domain tools implement. One impl per tool, no adapter.
#[async_trait]
pub trait Tool<R: Send + Sync>: Send + Sync {
    const NAME: &'static str;
    const DESCRIPTION: &'static str;
    type Args: DeserializeOwned + JsonSchema + Send;
    async fn run(ctx: &R, args: Self::Args) -> CapResult<ToolOutput>;
}

/// Object-safe runtime surface — what RoleDef stores and the engine dispatches.
/// Open for direct impls: runtime-named or stateful tools implement this directly
/// (`run` is receiverless, so instance state is unreachable through `Tool` — by design).
/// (Today's `Tool` trait, renamed; methods unchanged.)
#[async_trait]
pub trait ErasedTool<R: Send + Sync>: Send + Sync {
    fn name(&self) -> &str;
    fn description(&self) -> &str;
    fn schema(&self) -> Value;
    async fn call(&self, ctx: &R, args: Value) -> CapResult<Value>;
}

/// The ONE generic adapter (replaces nine hand-written ones).
struct Adapter<T>(PhantomData<T>);

#[async_trait]
impl<R: Send + Sync, T: Tool<R>> ErasedTool<R> for Adapter<T> {
    fn name(&self) -> &str { T::NAME }
    fn description(&self) -> &str { T::DESCRIPTION }
    fn schema(&self) -> Value {
        // schema_for!'s own expansion (0.8.22), so wire-identical by construction
        schema_json(schemars::gen::SchemaGenerator::default().into_root_schema_for::<T::Args>())
    }
    async fn call(&self, ctx: &R, args: Value) -> CapResult<Value> {
        ok_json(T::run(ctx, parse(args)?).await?)
    }
}

/// Roster constructor: `vec![tool(ForkWave), …]`. The value is a type witness
/// (`run` is receiverless) — any fields on it are dead.
pub fn tool<R: Send + Sync, T: Tool<R> + 'static>(_witness: T) -> Box<dyn ErasedTool<R>> {
    Box::new(Adapter::<T>(PhantomData))
}
```

**An Adapter wrapper, NOT a blanket impl**: `impl<R, T: Tool<R>> ErasedTool<R> for T` is an
E0119 compile-blocker against the four existing direct impls (`exo-node/src/outbound.rs:205,263`
tests, `exo-framework/tests/seam.rs:230` EchoTool, `exo/tests/seam_proof.rs:108` SubmitAudit)
and a one-way door sealing `ErasedTool` against runtime-named tools — silently narrowing the
`seam_proof.rs` third-party-extensibility guarantee. The wrapper gets the same nine-adapter
deletion, keeps both traits open, and is reversible. `parse`/`ok_json`/`schema_json` stay
public — direct `ErasedTool` implementors still use them.

### `roles.rs` — RoleDef as pure data + framework-owned folds

```rust
/// Pipeline stages are fallible async fns over the concrete runtime `R`, stored as plain
/// fn-pointers (greppable table, fn-pointer equality testable). The fold owns gate/rule
/// error policy; observers are infallible BY TYPE and own their errors internally.
pub type PreToolRule<R>  = for<'a> fn(&'a R, &'a HookInput) -> BoxFuture<'a, CapResult<HookDecision>>;
pub type StopGate<R>     = for<'a> fn(&'a R) -> BoxFuture<'a, CapResult<StopDecision>>;
pub type StopObserver<R> = for<'a> fn(&'a R) -> BoxFuture<'a, ()>;
pub type SessionStartFn<R> = for<'a> fn(&'a R) -> BoxFuture<'a, SessionStartOutput>; // unchanged

pub struct RoleDef<R: Send + Sync> {
    pub tools: Vec<Box<dyn ErasedTool<R>>>,
    pub pre_tool_use: Vec<PreToolRule<R>>,
    /// Gates: may Block (short-circuit). A Gemini role's gate list MUST stay empty (#20426).
    pub stop: Vec<StopGate<R>>,
    /// Observers: run only when the gates allowed exit; cannot block (return `()`).
    pub stop_observers: Vec<StopObserver<R>>,
    pub session_start: SessionStartFn<R>,
}

impl<R: Send + Sync> RoleDef<R> {
    pub async fn run_pre_tool_use(&self, ctx: &R, input: &HookInput) -> HookDecision { /* fold */ }
    pub async fn run_stop(&self, ctx: &R) -> StopDecision { /* gate fold, then observers on Allow */ }
}
```

`session_start` deliberately stays a plain fn (see Rejected: pipeline-ised session_start).

### Fold semantics (pinned)

- **pre_tool_use**: thread a working `HookInput` through the rules in order. `Allow` →
  continue. `Deny` → **short-circuit**, return it. `Modify { input }` → replace the working
  input's `tool_input` with the rewritten `Value` (tool_name unchanged), mark modified,
  continue — **subsequent rules see the rewritten input**. End: if any rule modified, return
  `Modify { input: working.tool_input }`, else `Allow`. Empty → `Allow`.
- **stop**: run gates in order. `Block` → **short-circuit**, return it (observers do NOT run —
  a dirty TL does not announce idle; today's monolith semantics, now fold-defined rather than
  order-dependent). All gates `Allow` (or none) → run every observer in order, return `Allow`.
- **Error doctrine, scoped per slot**:
  - *Gates and pre-rules* return `CapResult`; the fold logs an `Err` loud
    (`tracing::error!`, naming slot + stage index + role) and **continues** — fail-open,
    centralizing today's hand-rolled "a hook must never wedge an agent" matches in one
    documented place. Note: in the one-shot `exo hook` process "loud" is only as loud as the
    wired subscriber.
  - *Observers* return `()` — they **cannot** delegate error policy upward, so the bias-loud
    requirement on `abort_if_no_verdict` (a kv `Err` must count as no-verdict, or the
    review-gate stalls silently) is enforced by the type, not by a convention a future
    `?`-simplification could break.
  - *Forward note for the classic-antipattern port*: fold fail-open bakes in "all pre-rules
    are nudges". A future fail-closed rule (PII rewrite, security) erroring under fail-open
    would leak unscrubbed input — such a rule must catch its own errors and return `Deny`
    (or the fold grows per-rule policy then). Recorded now so it isn't discovered in an
    incident. Related: the engine currently shapes a Claude PreToolUse `Deny` as a
    `continue:true` + systemMessage *nudge* (`exo-node/src/hook.rs:114-117`,
    `hooksock/server.rs:146-148`) — fine for nudges, fatal for a rule that believes `Deny`
    denies; revisit alongside that port.
- **One deliberate behavior delta**: today a git error in `stop` allows exit *without* the
  idle announcement; under the fold, the gate's error fails open and the observers still run.
  The idle signal becomes robust to a git hiccup — an improvement, recorded so it's a decision
  rather than an accident.

### The decomposition map (exact — verified against `gates.rs`)

| Today (monolith) | Becomes |
|---|---|
| `stop_allow` (root) | `stop: vec![]`, `stop_observers: vec![]` |
| `stop` (tl) | `stop: vec![require_clean_tree]`, `stop_observers: vec![announce_idle]` |
| `stop_notify` (dev, worker) | `stop: vec![]`, `stop_observers: vec![announce_idle]` |
| `stop_reviewer` (reviewer) | `stop: vec![]`, `stop_observers: vec![abort_if_no_verdict]` |
| `pre_tool_use` (all) | `pre_tool_use: vec![deny_git_add_all]` |
| `session_start` (all, no-op) | unchanged plain fn |

**#20426 enforcement, all layers**: (1) structural — Gemini roles' `stop` is empty, asserted
totally in the roster test; (2) behavioral — Gemini-role `run_stop` returns `Allow` against a
dirty + failing-deliver mock; (3) engine net — the hooksock verdict shaping
(`hooksock/server.rs::stop_verdict`) never emits a Gemini Stop deny. Known gap in (3): the
in-process Stop arm in `exo-node/src/hook.rs:128-137` emits raw `block` with no shaping —
latent today (the CLI routes Stop through the socket, `exo/src/hook.rs:48-51`), and layer (1)
now covers every path regardless; still, give that arm the shaping or document it socket-only
during implementation.

---

## Review provenance

Two independent adversarial reviews (2026-06-09), conclusions folded in above:

- **Mechanics**: the full contract was transcribed into a scratch crate against the workspace's
  exact dep versions (async-trait 0.1.89, schemars 0.8.22) — compiled as written, 12/12 tests
  green, zero fixes: receiverless `async fn` + assoc consts/types under async_trait;
  fn-item→fn-pointer coercion inside `vec!` inside a generic roster fn; the vec-element
  equality idiom (`rd.stop[0] as usize == gate::<Mock> as *const () as usize`); Adapter schema
  output **byte-identical** to `schema_for!` (which in 0.8.22 expands to exactly the function
  form used); Deny short-circuit / Modify threading / Block-suppresses-observers / Err-skipped
  fold behaviors.
- **Design**: forced three amendments over the first draft — the gates/observers split
  (was: one `Vec<StopGate>` where observers could type-theoretically Block and #20426 was only
  a mock-bounded property test), the per-slot error doctrine (was: one fail-open convention
  with a doc-comment override — exactly the class of undocumented heuristic the repo forbids),
  and `session_start` staying a plain fn (was: a third pipeline with monoid semantics and zero
  contributors).

## Rejected alternatives (recorded so they aren't relitigated)

- **Blanket impl** `impl<R, T: Tool<R>> ErasedTool<R> for T` — E0119 against the 4 direct
  impls; seals the runtime trait; one-way door.
- **One stop vec for gates and observers** — types that lie (`StopGate` naming things that
  must never Block), the #20426 check decays to a mock-bounded property test, and bias-loud
  error handling becomes convention. The split costs one field and buys structural enforcement
  of the design's own hardest invariant. Lost generality — an observer that fires even on
  Block — has zero users; recoverable with an enum later if ever needed.
- **Pipeline-ised session_start** — uniformity with zero contributors and invented `\n\n`
  monoid semantics; worse, the likely first contributor (role-protocol injection, currently
  engine-side) is per-role-parameterized, which capture-free fn-pointers can't express without
  five near-duplicate fns. Convert when a real contributor exists — mechanical, since the
  rejected `Default` means every role literal already names the field.
- **`Default` / record-update on RoleDef** — an omitted hook field silently becoming
  empty-pipeline is a silent policy change; exhaustive literals are load-bearing for a policy
  table (and `vec![]` must be a visible, deliberate statement — it now carries #20426).
- **`tools![…]` / `rules![…]` macros** — `vec![tool(X), …]` / `vec![stage, …]` are already
  shorter than the macro invocations; no macro earns its keep (consistent with the
  no-per-tool-macro rule).
- **Per-role ZST trait** (`trait Role { fn tools(); … }`) — the banked spike's "trait+assoc
  wins iff phases are added" is deflated: v2 hooks already receive cap-bearing `&R` and do
  async IO, so phases are a domain pattern over `Kv` (as `verdict_produced` demonstrates),
  not an engine feature. serde + papers want the role as an enum value; the match table stays.
- **`Box<dyn Fn>` stage slots** — loses fn-pointer equality testing, `Copy`, and greppability;
  closures would smuggle config out of the visible table. Known cost accepted: fn-pointer
  storage also forecloses runtime stage *combinators* (e.g. `only_if_quiescent(observer)`) —
  conditions live inside stage bodies.
- **Named-stage pairs** (`(&'static str, StopGate<R>)`) for fold logging — table noise on
  every row; slot + index + role in the fold's error log is enough to find the stage in a
  1-2 element pipeline.
- **A single generic `Pipeline<S>`** over the slots — the folds have genuinely different
  semantics (threading vs short-circuit-then-observers); concrete aliases + two fold methods
  beat one abstraction with behavior knobs.

## Implementation map (two phases, two commits)

**Phase A — the Tool flip (wire-identical):**
- `exo-framework/src/tool.rs`: rename `Tool` → `ErasedTool`; add typed `Tool`, `Adapter`,
  `tool()`; rewrite the module-header doctrine (typed authoring trait + one generic adapter;
  NO per-tool macro; direct `ErasedTool` for runtime-named tools).
- The 9 adapters (`exo/src/tools/`: merge.rs:69, spawn.rs:67/145/263, tree.rs:179,
  submit.rs:305, verdict.rs:83, messaging.rs:42/134): delete adapter, convert inherent `run`
  + statics into `impl Tool<R>`. Bodies unchanged; 3 messaging tests calling the erased
  `call` on the concrete value (messaging.rs:165/190/218) get one-line rewrites.
- Roster arms `Box::new(X)` → `tool(X)`. Rename at the 4 direct-impl sites + `dyn Tool`
  mentions (outbound.rs:56/105/219/278, framework roles.rs:26, seam_proof.rs:177);
  seam_proof's extensibility doc-comment updates to name both surfaces.

**Phase B — hook pipelines:**
- `exo-framework/src/roles.rs`: aliases + `RoleDef` fields + the two fold methods; add
  `tracing` to `exo-framework/Cargo.toml` (verified absent).
- `exo/src/gates.rs`: decompose per the map; gates return `CapResult`, observers `()`;
  delete the monoliths; port gate tests to roster-level pipeline tests
  (`role_def::<MockRuntime>(Tl).run_stop(&ctx)`); add the structural Gemini test + keep the
  behavioral one; assert the flagged git-error delta as intended.
- Engine invocations → fold methods: `exo-node/src/hook.rs:110,129` and
  `hooksock/server.rs:142,175` (`session_start` call sites unchanged). Give hook.rs's
  in-process Stop arm the agent-type shaping or document it socket-only.
- RoleDef literals to vec-ify: exo-node/src/hook.rs tests (:325/:352/:386),
  `exo-node/src/test_support.rs:76-83`, `exo-node/tests/common/mod.rs:112-117`,
  `exo-framework/tests/seam.rs:276-287` (+ its hook invocations :327-341),
  `exo/tests/seam_proof.rs:182-187` (+ scalar fn-pointer asserts :307-309 → vec-element form).
- Flag rides along: the hooksock Gemini path **drops `Modify`** (server.rs:162-165 returns a
  bare allow, discarding the rewrite) — with Modify first-class in the fold, fix or loudly
  document before the classic antipattern port lands Modify-producing rules.

**Docs:** `exo/CLAUDE.md` (Shape rows, "The gates" section, Roles table, drop the
authoring-DSL gap bullet), `exo-framework/CLAUDE.md` (modules table, least-privilege section),
module headers in framework + domain, `rust/CLAUDE.md` one-liners, month-plan T3.7.

## Verification

- `cargo check --workspace` && `cargo test -p exo -p exo-framework -p exo-node`.
- Phase A: `tools/list` byte-identical before/after (statics + same schemars derivation;
  proven byte-identical in the scratch crate).
- Phase B: every existing gate behavior test green re-targeted at pipelines; the structural
  #20426 test (`stop.is_empty()` for every Gemini role) + the behavioral one (dirty +
  failing-deliver mock → `Allow`); `role_tool_matrix` unchanged.
