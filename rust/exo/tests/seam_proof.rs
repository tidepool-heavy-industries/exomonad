//! **The acceptance proof for the `Exomonad` trait refactor.**
//!
//! This file defines a SECOND, entirely independent domain (`ProofDomain`) — distinct from the
//! shipping `exo::ExoDomain` — using **only** the engine's public API (`exo-caps` / `exo-framework`
//! / `exo-node` / `exo-runtime`). It exercises all four extensibility seams the refactor closed:
//!
//! 1. **a new role / backend mapping** (`ProofRole`, incl. a brand-new `Auditor` archetype absent
//!    from `exo` that maps to a **different backend** (Shoal) than its other roles),
//! 2. **a novel inter-node System payload** (`ProofSystem::AuditComplete`),
//! 3. **a novel inter-node tool** (`SubmitAudit`, which emits that payload via `deliver_domain`;
//!    implements [`ErasedTool`] directly — the open extensibility surface for runtime-named or
//!    stateful tools, alongside the typed [`Tool`] authoring surface the 9 domain tools use),
//! 4. **a domain spawn intent** (`ProofSpawn`).
//!
//! The proof is that this compiles and links `exo-node` (`run_node::<ProofDomain>` typechecks) with
//! **ZERO edits** to `exo-framework` / `exo-caps` / `exo-node` / `exo-runtime`. Adding a whole new
//! domain is a downstream-only change — exactly the property the refactor set out to guarantee.

use async_trait::async_trait;
use exo_caps::{
    deliver_domain, Addressee, AgentName, AgentType, Branch, Bus, CapResult, ChildKind, Message,
    Persona, RoleKind, SpawnSpec,
};
use exo_framework::{
    ok_json, parse, schema_json, BoxFuture, ErasedTool, Exomonad, RoleDef, SystemCtx,
    SystemOutcome, Tool, ToolOutput,
};
use exo_runtime::Runtime;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

// ── seam #1: a brand-new role set with its own role→backend mapping ──────────────────────────

/// A role set that is NOT `exo::ExoRole`: it has a novel `Auditor` archetype mapped to the **Shoal**
/// backend (vs its Claude-backed siblings) — proving a domain owns both the role enum (leak #1) and
/// the role→backend mapping (leak #2).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
enum ProofRole {
    Overseer,
    /// A brand-new archetype with no analogue in `exo`, mapped to a non-Claude backend.
    Auditor,
    /// A reviewer mapped to the Claude backend.
    ClaudeReviewer,
}

impl RoleKind for ProofRole {
    fn all() -> &'static [Self] {
        &[
            ProofRole::Overseer,
            ProofRole::Auditor,
            ProofRole::ClaudeReviewer,
        ]
    }
    fn agent_type(&self) -> AgentType {
        match self {
            // The overseer + the Claude reviewer run on Claude; the auditor on Shoal.
            ProofRole::Overseer | ProofRole::ClaudeReviewer => AgentType::Claude,
            ProofRole::Auditor => AgentType::Shoal,
        }
    }
    fn role_str(&self) -> &'static str {
        match self {
            ProofRole::Overseer => "overseer",
            ProofRole::Auditor => "auditor",
            ProofRole::ClaudeReviewer => "claude-reviewer",
        }
    }
}

// ── seam #2: a novel inter-node System payload ───────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
enum ProofSystem {
    /// A signal no other domain has: an audit finished with a score.
    AuditComplete { score: u32, note: String },
}

// ── seam #3: a novel inter-node tool emitting that payload ───────────────────────────────────

#[derive(Debug, Deserialize, JsonSchema)]
struct SubmitAuditArgs {
    score: u32,
    note: String,
}

struct SubmitAudit;

impl SubmitAudit {
    async fn run<C: Bus>(ctx: &C, args: SubmitAuditArgs) -> CapResult<ToolOutput> {
        // The free `deliver_domain` helper keeps least-privilege: this tool names `ProofSystem`
        // but needs only `C: Bus` — not `C: Bus<ProofSystem>`.
        deliver_domain(
            ctx,
            Addressee::Parent,
            "[audit]",
            "audit complete",
            &ProofSystem::AuditComplete {
                score: args.score,
                note: args.note,
            },
        )
        .await?;
        Ok(ToolOutput::text("audit submitted"))
    }
}

#[async_trait]
impl<R: Bus + Send + Sync> ErasedTool<R> for SubmitAudit {
    fn name(&self) -> &str {
        "submit_audit"
    }
    fn description(&self) -> &str {
        "Emit an AuditComplete system signal to the parent."
    }
    fn schema(&self) -> serde_json::Value {
        schema_json::<SubmitAuditArgs>()
    }
    async fn call(&self, ctx: &R, j: serde_json::Value) -> CapResult<serde_json::Value> {
        ok_json(Self::run(ctx, parse(j)?).await?)
    }
}

// ── seam #4: a domain spawn intent ───────────────────────────────────────────────────────────

struct ProofSpawn {
    role: ProofRole,
}

impl SpawnSpec for ProofSpawn {
    type Role = ProofRole;
    fn role(&self) -> ProofRole {
        self.role
    }
    fn child_kind(&self) -> ChildKind {
        ChildKind::Worktree
    }
    fn name(&self) -> Option<AgentName> {
        None
    }
    fn name_prefix(&self) -> &str {
        "audit"
    }
    fn fork_session(&self) -> bool {
        false
    }
    fn into_task(self) -> String {
        "audit the branch".into()
    }
}

// ── the domain: ties the four seams together ─────────────────────────────────────────────────

// Trivial hooks (the engine only needs the fn-pointer shapes).
fn pre<'a>(
    _: &'a Runtime,
    _: &'a exo_framework::HookInput,
) -> BoxFuture<'a, exo_framework::HookDecision> {
    Box::pin(async { exo_framework::HookDecision::Allow })
}
fn stop(_: &Runtime) -> BoxFuture<'_, exo_framework::StopDecision> {
    Box::pin(async { exo_framework::StopDecision::Allow })
}
fn session(_: &Runtime) -> BoxFuture<'_, exo_framework::SessionStartOutput> {
    Box::pin(async { exo_framework::SessionStartOutput::default() })
}

struct ProofDomain;

impl Exomonad for ProofDomain {
    type Caps = Runtime;
    type Role = ProofRole;
    type System = ProofSystem;
    type Spawn = ProofSpawn;

    fn role_def(role: ProofRole) -> RoleDef<Runtime> {
        // Every role gets the novel tool — direct construction in a match (struct-first).
        let tools: Vec<Box<dyn ErasedTool<Runtime>>> = match role {
            ProofRole::Overseer | ProofRole::Auditor | ProofRole::ClaudeReviewer => {
                vec![Box::new(SubmitAudit)]
            }
        };
        RoleDef {
            tools,
            pre_tool_use: pre,
            stop,
            session_start: session,
        }
    }

    fn handle_system<'a, C: SystemCtx>(
        ctx: &'a C,
        _from: &'a Persona,
        system: &'a ProofSystem,
    ) -> BoxFuture<'a, CapResult<SystemOutcome>> {
        Box::pin(async move {
            match system {
                ProofSystem::AuditComplete { score, note } => {
                    // A domain reacts to its own novel signal through the engine's SystemCtx seam.
                    ctx.deliver_to_self(
                        "auditor",
                        "[audit]",
                        &format!("audit score {score}: {note}"),
                    )
                    .await?;
                    Ok(SystemOutcome::Done)
                }
            }
        })
    }
}

// ── the proof itself ─────────────────────────────────────────────────────────────────────────

/// THE acceptance assertion: the engine's generic entrypoints accept `ProofDomain`. If this
/// type-checks, a brand-new domain links `exo-node` with zero edits to the engine crates.
#[test]
fn engine_accepts_a_brand_new_domain() {
    // Naming the monomorphized engine fns forces the `D: Exomonad<Caps = Runtime>` bound check.
    let _bootstrap = exo_node::bootstrap::<ProofDomain>;
    let _run = exo_node::run_node::<ProofDomain>;

    // The domain's role table resolves and includes the novel tool.
    for role in ProofRole::all() {
        let rd = ProofDomain::role_def(*role);
        assert!(rd.tools.iter().any(|t| t.name() == "submit_audit"));
    }
    // The role→backend mapping is the domain's: a Claude reviewer, a new Shoal-backed archetype.
    assert_eq!(ProofRole::ClaudeReviewer.agent_type(), AgentType::Claude);
    assert_eq!(ProofRole::Auditor.agent_type(), AgentType::Shoal);
    assert_eq!(ProofRole::all().len(), 3);
}

/// The novel System payload round-trips through the erased wire (`deliver_domain` → raw JSON →
/// `D::System`), the exact path the inbound loop's Domain arm walks.
#[test]
fn novel_system_round_trips_through_the_erased_wire() {
    let sys = ProofSystem::AuditComplete {
        score: 9,
        note: "looks good".into(),
    };
    let raw = serde_json::value::to_raw_value(&sys).unwrap();
    let back: ProofSystem = serde_json::from_str(raw.get()).unwrap();
    assert_eq!(sys, back);
}

/// The domain's `handle_system` runs through a mock `SystemCtx` — proving the relocated gate seam
/// works for an arbitrary domain, not just `exo`'s review gate.
#[tokio::test]
async fn handle_system_runs_through_the_seam() {
    use std::sync::Mutex;

    struct MockCtx {
        branch: Branch,
        delivered: Mutex<Vec<String>>,
    }
    #[async_trait]
    impl SystemCtx for MockCtx {
        fn own_branch(&self) -> &Branch {
            &self.branch
        }
        async fn head_sha(&self) -> CapResult<String> {
            Ok("deadbeef".into())
        }
        async fn deliver_parent(&self, _msg: Message) -> CapResult<()> {
            Ok(())
        }
        async fn deliver_to_self(&self, _from: &str, _summary: &str, text: &str) -> CapResult<()> {
            self.delivered.lock().unwrap().push(text.to_string());
            Ok(())
        }
        async fn read_reviews(&self, _path: &std::path::Path) -> CapResult<Option<Vec<u8>>> {
            Ok(None)
        }
        async fn persist_reviews(&self, _path: &std::path::Path, _bytes: &[u8]) -> CapResult<()> {
            Ok(())
        }
    }

    let ctx = MockCtx {
        branch: Branch::new("proof.audit-0".into()).unwrap(),
        delivered: Mutex::new(vec![]),
    };
    let from = Persona::Agent(AgentName::new("audit-0".into()).unwrap());
    let sys = ProofSystem::AuditComplete {
        score: 7,
        note: "ok".into(),
    };
    let outcome = ProofDomain::handle_system(&ctx, &from, &sys).await.unwrap();
    assert_eq!(outcome, SystemOutcome::Done);
    assert!(ctx.delivered.lock().unwrap()[0].contains("audit score 7"));

    // A spawn intent for the new archetype is constructible + readable by the engine's seam.
    let spec = ProofSpawn {
        role: ProofRole::Auditor,
    };
    assert_eq!(spec.role(), ProofRole::Auditor);
    assert_eq!(spec.child_kind(), ChildKind::Worktree);
}

#[test]
fn auditor_role_def_is_wired() {
    let rd = ProofDomain::role_def(ProofRole::Auditor);
    assert!(!rd.tools.is_empty());
    assert!(rd.tools.iter().any(|t| t.name() == "submit_audit"));

    // Verify hook pointers are wired
    assert_eq!(rd.pre_tool_use as usize, pre as *const () as usize);
    assert_eq!(rd.stop as usize, stop as *const () as usize);
    assert_eq!(rd.session_start as usize, session as *const () as usize);
}

#[test]
fn proof_spawn_properties_round_trip() {
    let spec = ProofSpawn {
        role: ProofRole::Auditor,
    };
    assert_eq!(spec.role(), ProofRole::Auditor);
    assert_eq!(spec.child_kind(), ChildKind::Worktree);
    assert_eq!(spec.name(), None);
    assert_eq!(spec.name_prefix(), "audit");
    assert!(!spec.fork_session());
    assert_eq!(spec.into_task(), "audit the branch");
}
