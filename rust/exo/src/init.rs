//! `exo init` — bootstrap a node-mode ROOT: its own tmux session, root papers, NO central server.
//!
//! Reuses the v2/shared seam only (`exo-runtime`, `exo-caps`, `exomonad-shared`) — never classic
//! `exomonad-core`.

use anyhow::Result;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

/// Per-node runtime artifacts (`.mcp.json`, `.claude/settings.local.json`, and the whole `.exo/`
/// runtime tree — `node.json`, `settings.json`, `children.jsonl`, `tmp/`, `worktrees/`, `logs/`)
/// are written into each node's worktree at spawn. They must not dirty the tree — a dirty worktree
/// blocks `fork_wave`'s clean-state precondition and trips the `stop` clean-gate (a node can't
/// cleanly exit). Add them to the repo's **shared** `.git/info/exclude` (in the common dir, so it
/// covers the root and every worktree child, isn't committed, and doesn't itself dirty anything).
/// `.exo/*` + negations mirror the `exomonad new` `.gitignore`, keeping tracked config/roles/lib/
/// rules visible. Idempotent.
fn ensure_git_excludes(cwd: &Path) -> Result<()> {
    let out = Command::new("git")
        .arg("-C")
        .arg(cwd)
        .args(["rev-parse", "--git-common-dir"])
        .output()?;
    if !out.status.success() {
        eprintln!("[exo] not a git repo (or git missing); skipping .git/info/exclude setup");
        return Ok(());
    }
    let common = String::from_utf8_lossy(&out.stdout).trim().to_string();
    let common_dir = if Path::new(&common).is_absolute() {
        PathBuf::from(common)
    } else {
        cwd.join(common)
    };
    let info_dir = common_dir.join("info");
    std::fs::create_dir_all(&info_dir)?;
    let exclude = info_dir.join("exclude");
    let existing = std::fs::read_to_string(&exclude).unwrap_or_default();

    let marker = "# exomonad node-mode per-agent runtime artifacts (keep worktrees clean)";
    let patterns = [
        ".mcp.json",
        ".claude/settings.local.json",
        ".exo/*",
        "!.exo/config.toml",
        "!.exo/roles/",
        "!.exo/lib/",
        "!.exo/rules/",
    ];
    let missing: Vec<&str> = patterns
        .iter()
        .copied()
        .filter(|p| !existing.lines().any(|l| l.trim() == *p))
        .collect();
    if missing.is_empty() {
        return Ok(());
    }

    let mut content = existing;
    if !content.is_empty() && !content.ends_with('\n') {
        content.push('\n');
    }
    if !content.contains(marker) {
        content.push_str(marker);
        content.push('\n');
    }
    for p in missing {
        content.push_str(p);
        content.push('\n');
    }
    std::fs::write(&exclude, content)?;
    Ok(())
}

/// The user-level systemd slice `swarm-run` launches its argv inside. Hardcoded (not a config
/// knob) — one shared slice for the whole swarm, no per-agent scopes; see `rust/CLAUDE.md` § the
/// v2 UX campaign's cgroup-confinement design note.
const CONFINE_SLICE: &str = "swarm.slice";

/// Outcome of [`confine_server`]: the socket to boot the root session on (`None` ⇒ default tmux
/// socket, today's behavior) and whether cgroup membership was actually confirmed.
struct ConfineOutcome {
    socket: Option<String>,
    confirmed: bool,
}

fn wrapper_on_path(wrapper: &str) -> bool {
    Command::new("which")
        .arg(wrapper)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

/// The confined tmux server's own pid, via a plain (no `-t`) `display-message` — targeting no
/// pane, tmux answers with the SERVER's own pid rather than a pane's.
fn tmux_server_pid(socket: &str) -> Option<u32> {
    let out = Command::new("tmux")
        .args(["-L", socket, "display-message", "-p", "#{pid}"])
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    String::from_utf8_lossy(&out.stdout).trim().parse().ok()
}

/// Pure classification: does this `/proc/{pid}/cgroup` content place the process in `slice`?
/// Split out for unit testing without touching `/proc`.
fn cgroup_content_in_slice(cgroup_content: &str, slice: &str) -> bool {
    cgroup_content.contains(slice)
}

fn cgroup_pid_in_slice(pid: u32, slice: &str) -> bool {
    std::fs::read_to_string(format!("/proc/{pid}/cgroup"))
        .map(|content| cgroup_content_in_slice(&content, slice))
        .unwrap_or(false)
}

/// Ensure the swarm's tmux SERVER for `socket` is running inside `wrapper`'s cgroup slice, so
/// every pane it ever forks structurally inherits the slice — panes are forked by the tmux
/// SERVER, not the client, so wrapping `exo init` itself or individual pane commands confines
/// nothing. Fails OPEN on every precondition miss (missing wrapper, wrapped start failure,
/// unconfirmable cgroup): loud warning on stderr + `tracing::warn!`, then proceeds exactly as
/// `exo init` did before `confine` existed.
fn confine_server(wrapper: &str, socket: &str) -> ConfineOutcome {
    if !wrapper_on_path(wrapper) {
        eprintln!(
            "\n\
             ⚠️  exo init: confine=true but `{wrapper}` was not found on PATH.\n\
             ⚠️  Proceeding UNCONFINED — spawned panes will NOT be cgroup-isolated.\n\
             ⚠️  Expose `{wrapper}` on PATH (the {CONFINE_SLICE} wrapper) to enable confinement.\n"
        );
        tracing::warn!(
            wrapper,
            "confine=true but wrapper not found on PATH; proceeding unconfined"
        );
        return ConfineOutcome {
            socket: None,
            confirmed: false,
        };
    }

    // `tmux_server_pid` (a plain `display-message`, no `-t`), not `has-session` — `has-session`
    // without `-t` resolves against a "current session" and fails with "no current target" even
    // when the server IS up but has zero sessions (exactly the state right after the
    // `exit-empty off` bootstrap below, and a real reachable state whenever a confined server
    // outlives a killed session, since `exit-empty` is deliberately off).
    let already_running = tmux_server_pid(socket).is_some();
    if already_running {
        tracing::info!(
            socket,
            "confine: reusing already-running tmux server on socket"
        );
    } else {
        tracing::info!(
            socket,
            wrapper,
            "confine: starting tmux server on dedicated socket via wrapper"
        );
        // `set-option -g exit-empty off` first: a session-less `start-server` alone exits
        // immediately (tmux's default `exit-empty` tears down a server with zero sessions), so
        // without this the wrapped server would die before the root session is ever created on
        // it by the (unconfined) client call in `boot_root_session` below.
        let started = Command::new(wrapper)
            .args([
                "tmux",
                "-L",
                socket,
                "set-option",
                "-g",
                "exit-empty",
                "off",
                ";",
                "start-server",
            ])
            .status();
        match started {
            Ok(s) if s.success() => {}
            Ok(s) => {
                eprintln!(
                    "\n⚠️  exo init: `{wrapper} tmux -L {socket} start-server` exited {s}.\n\
                     ⚠️  Proceeding UNCONFINED.\n"
                );
                tracing::warn!(
                    ?s,
                    "confine: wrapped start-server failed; proceeding unconfined"
                );
                return ConfineOutcome {
                    socket: None,
                    confirmed: false,
                };
            }
            Err(e) => {
                eprintln!("\n⚠️  exo init: failed to exec `{wrapper}`: {e}.\n⚠️  Proceeding UNCONFINED.\n");
                tracing::warn!(error = %e, "confine: failed to exec wrapper; proceeding unconfined");
                return ConfineOutcome {
                    socket: None,
                    confirmed: false,
                };
            }
        }
    }

    let confirmed = tmux_server_pid(socket)
        .map(|pid| cgroup_pid_in_slice(pid, CONFINE_SLICE))
        .unwrap_or(false);

    if confirmed {
        println!("exo init: CONFINED — tmux server on socket {socket:?} is in {CONFINE_SLICE}.");
    } else if already_running {
        eprintln!(
            "\n⚠️  exo init: a tmux server is already running on socket {socket:?} but it is NOT \
             in {CONFINE_SLICE}.\n\
             ⚠️  Proceeding UNCONFINED on the existing server (not killing it).\n"
        );
        tracing::warn!(
            socket,
            "confine: pre-existing server on socket not in slice; proceeding unconfined on it"
        );
    } else {
        eprintln!(
            "\n⚠️  exo init: started a tmux server on socket {socket:?} via {wrapper} but could \
             not confirm it is in {CONFINE_SLICE}.\n\
             ⚠️  Proceeding UNCONFINED.\n"
        );
        tracing::warn!(
            socket,
            "confine: could not verify slice membership after wrapped start; proceeding unconfined"
        );
    }

    ConfineOutcome {
        socket: Some(socket.to_string()),
        confirmed,
    }
}

#[allow(clippy::too_many_arguments)]
pub async fn run(
    tmux_session: &str,
    model: Option<&str>,
    yolo: bool,
    wrap_nix: bool,
    review_enabled: bool,
    profile_env: &[(String, String)],
    confine: bool,
    confine_wrapper: &str,
    confine_socket: &str,
    session: Option<String>,
    recreate: bool,
    backend: crate::config::Backend,
    codex_env: &[(String, String)],
) -> Result<()> {
    let session = session.unwrap_or_else(|| format!("{tmux_session}-exp"));
    let run_id = uuid::Uuid::new_v4().to_string();
    let cwd = std::env::current_dir()?;

    if backend == crate::config::Backend::Codex {
        let queue_help = Command::new("codex")
            .args(["queue", "--help"])
            .output()
            .map_err(|e| {
                anyhow::anyhow!("Codex backend requires `codex` on PATH with queue support: {e}")
            })?;
        let queue_text = String::from_utf8_lossy(&queue_help.stdout);
        if !queue_help.status.success()
            || !queue_text.contains("--thread")
            || !queue_text.contains("--message")
        {
            anyhow::bail!(
                "installed Codex CLI lacks the required `codex queue --thread --message` \
                 surface (Codex 0.149+); update Codex or run `exo init --backend claude`"
            );
        }
    }

    // Keep per-node runtime artifacts from dirtying worktrees (root + all spawned children).
    ensure_git_excludes(&cwd)?;

    let confine_outcome = if confine {
        confine_server(confine_wrapper, confine_socket)
    } else {
        ConfineOutcome {
            socket: None,
            confirmed: false,
        }
    };
    let socket = confine_outcome.socket.as_deref();

    let root_pane = exo_runtime::boot_root_session(&session, &cwd, recreate, socket).await?;

    let mut set_run_id_args: Vec<&str> = Vec::new();
    if let Some(s) = socket {
        set_run_id_args.extend(["-L", s]);
    }
    set_run_id_args.extend([
        "set-environment",
        "-t",
        &session,
        "EXOMONAD_SWARM_RUN_ID",
        &run_id,
    ]);
    let set_run_id = Command::new("tmux").args(&set_run_id_args).status()?;
    if !set_run_id.success() {
        anyhow::bail!("Failed to set EXOMONAD_SWARM_RUN_ID in tmux session");
    }

    let mut set_session_args: Vec<&str> = Vec::new();
    if let Some(s) = socket {
        set_session_args.extend(["-L", s]);
    }
    set_session_args.extend([
        "set-environment",
        "-t",
        &session,
        "EXOMONAD_TMUX_SESSION",
        &session,
    ]);
    let set_session = Command::new("tmux").args(&set_session_args).status()?;
    if !set_session.success() {
        anyhow::bail!("Failed to set EXOMONAD_TMUX_SESSION in tmux session");
    }

    for (key, value) in codex_env {
        let mut args: Vec<&str> = Vec::new();
        if let Some(s) = socket {
            args.extend(["-L", s]);
        }
        args.extend(["set-environment", "-t", &session, key, value]);
        let status = Command::new("tmux").args(args).status()?;
        if !status.success() {
            anyhow::bail!("failed to set {key} in tmux session");
        }
    }

    // Only stamped when confinement was actually verified. Set on the SESSION (not just the root's
    // own launch env below) so it reaches every descendant pane too — `boot_root_session`'s holding
    // shell predates this call and needs it embedded directly (see `env_vars` below), but every
    // `Tmux::new_window`/`new_pane` spawned tree-wide happens well after this, and a tmux pane's
    // initial process environment is seeded from the session environment at creation time (verified
    // live: `set-environment` + a later `new-window` sees the var via plain `printenv`) — the same
    // mechanism `EXOMONAD_SWARM_RUN_ID`/`EXOMONAD_TMUX_SESSION` above already rely on. `tree.rs`'s
    // per-node self-check reads this via `std::env::var`, so every spawned TL/dev/worker/reviewer
    // must see it, not just root.
    if confine_outcome.confirmed {
        let mut set_confined_args: Vec<&str> = Vec::new();
        if let Some(s) = socket {
            set_confined_args.extend(["-L", s]);
        }
        set_confined_args.extend(["set-environment", "-t", &session, "EXO_CONFINED", "1"]);
        let set_confined = Command::new("tmux").args(&set_confined_args).status()?;
        if !set_confined.success() {
            anyhow::bail!("Failed to set EXO_CONFINED in tmux session");
        }
    }

    // Stamp the configured child-launch policy onto the root's papers. `birth` reads a node's own
    // papers (`own_launch_policy`) and inherits the policy onto every child, so setting it on the
    // root flows it down the whole tree. Defaults (config unset) keep launches byte-identical.
    // The root's role is the domain's `ExoRole::Root` (recorded erased in papers).
    let mut papers = exo_caps::NodePapers::root(root_pane.clone(), exo::ExoRole::Root)?;
    papers.agent_type = backend.agent_type();
    papers.yolo = yolo;
    papers.wrap_nix = wrap_nix;
    papers.review_enabled = review_enabled;
    let papers_path = cwd.join(format!(".exo/node/{run_id}/root.json"));
    if backend == crate::config::Backend::Codex {
        papers.codex = Some(exo_caps::CodexNode {
            binding: cwd.join(".exo/codex-root-thread.json"),
        });
    }
    if let Some(parent) = papers_path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(&papers_path, serde_json::to_vec_pretty(&papers)?)?;

    // Root config goes to private files (siblings of root.json under `.exo/node/{run}/`), pointed at
    // via `--settings`/`--mcp-config` below — never the cwd's `.mcp.json`/`.claude/settings.local.json`.
    let (settings_path, mcp_config_path) = exo_caps::paths::node_config_paths(&papers_path);
    if backend == crate::config::Backend::Claude {
        exo_runtime::write_node_agent_config(&settings_path, &mcp_config_path, &papers_path)
            .await?;
    }

    // Migration: remove stale exomonad-written cwd config from the pre-private-config era. CC still
    // MERGES the cwd's `.claude/settings.local.json` over our `--settings`, so a leftover (e.g. a
    // worker-clobbered) one would fire dead hooks. Only OUR generated content is touched.
    migrate_strip_legacy_cwd_config(&cwd);

    // Embed the boot env directly in the launch command. The holding-shell pane was created
    // by `boot_root_session` BEFORE the `tmux set-environment` calls above, so it never picked
    // up the session vars — `claude` (and the `exo node` sidecar it spawns) would
    // otherwise start without EXOMONAD_SWARM_RUN_ID and fail bootstrap. The session-env calls
    // above still serve descendant panes spawned later.
    // Carry any per-role launch-profile vars into the ROOT launch (the root pane predates the
    // session-env, so they're embedded inline like the boot vars above). `birth_finish` re-copies
    // them onto every descendant; only a profiled role's own launch translates them to `ANTHROPIC_*`.
    // Source = `.exo/config.toml` (`[launch_profile.<role>]`, the `profile_env` param), overlaid by
    // any matching `EXO_*` already in the shell (so a secret key can stay out of the file). Absent ⇒
    // empty ⇒ launch byte-identical.
    const PROFILE_SUFFIXES: [&str; 4] = ["_BASE_URL", "_MODEL", "_AUTH_TOKEN", "_LABEL"];
    let mut env_vars: std::collections::HashMap<String, String> =
        profile_env.iter().cloned().collect();
    env_vars.extend(
        std::env::vars().filter(|(k, _)| {
            k.starts_with("EXO_") && PROFILE_SUFFIXES.iter().any(|s| k.ends_with(s))
        }),
    );
    env_vars.extend(codex_env.iter().cloned());
    env_vars.insert("EXOMONAD_SWARM_RUN_ID".into(), run_id.clone());
    env_vars.insert("EXOMONAD_TMUX_SESSION".into(), session.clone());
    // Only stamped when confinement was actually verified — descendants inherit it (`birth_finish`
    // re-copies the full launch env), and `tree` uses its presence to decide whether to self-check
    // `/proc/self/cgroup` at all (absent ⇒ this host never asked for confinement ⇒ no noise).
    if confine_outcome.confirmed {
        env_vars.insert("EXO_CONFINED".into(), "1".into());
    }
    // On `--recreate` (e.g. restarting after a binary update), continue the prior root
    // conversation so the restart doesn't discard the human's context — `claude --continue`
    // resumes the most recent conversation in this cwd. A fresh `init` has nothing to continue.
    // The root has no positional prompt (interactive launch), so --mcp-config never abuts a
    // prompt argument — but we use ClaudeInvocation for uniformity and structural safety.
    let launch = if backend == crate::config::Backend::Codex {
        let codex = papers.codex.as_ref().expect("Codex papers set");
        // Root is the top-level TL for Codex model policy. An explicit legacy `model` setting
        // still wins; otherwise use the same configured/default model and effort as spawned TLs.
        let codex_model = model.map(str::to_owned).or_else(|| {
            codex_env
                .iter()
                .find(|(key, _)| key == "EXO_CODEX_TL_MODEL")
                .map(|(_, value)| value.clone())
        });
        let effort = codex_env
            .iter()
            .find(|(key, _)| key == "EXO_CODEX_TL_REASONING_EFFORT")
            .map(|(_, value)| value.clone());
        let identity = format!(
            "You are exomonad node 'root' (role: root) on the current branch. Parent: none.\n\n{}",
            exo::protocol::ROOT
        );

        // Only bindings captured by the stock-TUI integration are resume-safe. Older app-server
        // bindings name server-side threads that may have no local rollout, which is exactly the
        // `no rollout found` failure this path replaces.
        let resume_thread = if recreate {
            match exo_runtime::codex::read_binding(&codex.binding).await {
                Ok(binding) if binding.v == exo_runtime::codex::CodexBinding::VERSION => {
                    Some(binding.thread_id)
                }
                Ok(binding) => {
                    tracing::info!(
                        version = binding.v,
                        "ignoring legacy Codex app-server binding; starting a stock TUI"
                    );
                    None
                }
                Err(error) => {
                    tracing::warn!(%error, "Codex root binding unavailable; starting a fresh TUI");
                    None
                }
            }
        } else {
            None
        };
        if resume_thread.is_none() {
            match tokio::fs::remove_file(&codex.binding).await {
                Ok(()) => {}
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                Err(error) => return Err(error.into()),
            }
        }
        let mode = match resume_thread.as_deref() {
            Some(thread_id) => exo_runtime::codex::LaunchMode::Resume(thread_id),
            None => exo_runtime::codex::LaunchMode::Fresh,
        };
        exo_runtime::codex::tui_command(
            mode,
            &cwd,
            &papers_path,
            codex_model.as_deref(),
            effort.as_deref(),
            &identity,
            None,
            &env_vars,
        )
    } else {
        exomonad_shared::services::agent_control::launch::ClaudeInvocation {
            agent_type: exomonad_shared::services::agent_control::AgentType::Claude,
            cwd: cwd.clone(),
            permission_mode: None, // root always uses --dangerously-skip-permissions
            allowed_tools: vec![],
            disallowed_tools: vec![],
            settings_path: Some(settings_path.to_string_lossy().into_owned()),
            mcp_config_path: Some(mcp_config_path.to_string_lossy().into_owned()),
            append_system_prompt: None, // root has no role-steering prompt
            model: model.map(|m| m.to_string()),
            prompt_file: None, // interactive launch — no positional prompt
            fork_session_id: None,
            env_vars,
            yolo: false,
            wrap_nix,
            resume: recreate,
        }
        .render()
    };

    exomonad_shared::services::tmux_ipc::TmuxIpc::new_with_socket(
        &session,
        socket.map(str::to_string),
    )
    .inject_input(root_pane.as_str(), &launch)
    .await?;

    let attach_cmd = match socket {
        Some(s) => format!("tmux -L {s} attach -t {session}"),
        None => format!("tmux attach -t {session}"),
    };
    println!(
        "Root node up in tmux session '{session}'. Attaching (detach: Ctrl-b d; reattach: {attach_cmd})..."
    );

    exomonad_shared::services::tmux_ipc::TmuxIpc::attach_session(&session, socket).await
}

/// Best-effort removal of stale exomonad-written cwd config from the pre-private-config era, so it
/// doesn't merge into a node's launch. Touches ONLY our generated content: a user-authored
/// `settings.local.json` (no `_exomonad_generated` marker) or a `.mcp.json` carrying the user's own
/// servers is preserved. Failures are logged, never fatal.
fn migrate_strip_legacy_cwd_config(cwd: &Path) {
    // `.claude/settings.local.json`: delete iff it carries our generated marker (CC MERGES it over
    // our `--settings`, so a stale one would fire dead hooks).
    let settings = cwd.join(".claude/settings.local.json");
    let is_ours = std::fs::read(&settings)
        .ok()
        .and_then(|b| serde_json::from_slice::<serde_json::Value>(&b).ok())
        .and_then(|v| v.get("_exomonad_generated").and_then(|m| m.as_bool()))
        == Some(true);
    if is_ours {
        if let Err(e) = std::fs::remove_file(&settings) {
            eprintln!(
                "exo init: could not remove legacy {}: {e}",
                settings.display()
            );
        }
    }

    // `.mcp.json` that is ONLY our exomonad server (what the old code truncated it to) is obsolete —
    // delete it. A file the user added other servers to is left alone (our `--mcp-config` exomonad
    // server takes command-line precedence over a stale project one).
    let mcp = cwd.join(".mcp.json");
    let only_exomonad = std::fs::read(&mcp)
        .ok()
        .and_then(|b| serde_json::from_slice::<serde_json::Value>(&b).ok())
        .and_then(|v| {
            v.get("mcpServers")
                .and_then(|s| s.as_object())
                .map(|o| o.len() == 1 && o.contains_key("exomonad"))
        })
        == Some(true);
    if only_exomonad {
        if let Err(e) = std::fs::remove_file(&mcp) {
            eprintln!("exo init: could not remove legacy {}: {e}", mcp.display());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cgroup_content_matches_slice_present() {
        let content =
            "0::/user.slice/user-1000.slice/user@1000.service/swarm.slice/run-p1-i2.scope\n";
        assert!(cgroup_content_in_slice(content, "swarm.slice"));
    }

    #[test]
    fn cgroup_content_no_match_when_slice_absent() {
        let content = "0::/user.slice/user-1000.slice/user@1000.service/app.slice/foo.scope\n";
        assert!(!cgroup_content_in_slice(content, "swarm.slice"));
    }

    #[test]
    fn cgroup_content_no_match_on_empty() {
        assert!(!cgroup_content_in_slice("", "swarm.slice"));
    }
}
