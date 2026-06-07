//! `exo init` — bootstrap a node-mode ROOT: its own tmux session, root papers, NO central server.
//!
//! Reuses the v2/shared seam only (`exo-runtime`, `exo-caps`, `exomonad-shared`) — never classic
//! `exomonad-core`.

use anyhow::Result;
use std::path::{Path, PathBuf};
use std::process::Command;

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

pub async fn run(
    tmux_session: &str,
    model: Option<&str>,
    yolo: bool,
    wrap_nix: bool,
    session: Option<String>,
    recreate: bool,
) -> Result<()> {
    let session = session.unwrap_or_else(|| format!("{tmux_session}-exp"));
    let run_id = uuid::Uuid::new_v4().to_string();
    let cwd = std::env::current_dir()?;

    // Keep per-node runtime artifacts from dirtying worktrees (root + all spawned children).
    ensure_git_excludes(&cwd)?;

    let root_pane = exo_runtime::boot_root_session(&session, &cwd, recreate).await?;

    let set_run_id = Command::new("tmux")
        .args([
            "set-environment",
            "-t",
            &session,
            "EXOMONAD_SWARM_RUN_ID",
            &run_id,
        ])
        .status()?;
    if !set_run_id.success() {
        anyhow::bail!("Failed to set EXOMONAD_SWARM_RUN_ID in tmux session");
    }

    let set_session = Command::new("tmux")
        .args([
            "set-environment",
            "-t",
            &session,
            "EXOMONAD_TMUX_SESSION",
            &session,
        ])
        .status()?;
    if !set_session.success() {
        anyhow::bail!("Failed to set EXOMONAD_TMUX_SESSION in tmux session");
    }

    // Stamp the configured child-launch policy onto the root's papers. `birth` reads a node's own
    // papers (`own_launch_policy`) and inherits the policy onto every child, so setting it on the
    // root flows it down the whole tree. Defaults (config unset) keep launches byte-identical.
    let mut papers = exo_caps::NodePapers::root(root_pane.clone());
    papers.yolo = yolo;
    papers.wrap_nix = wrap_nix;
    let papers_path = cwd.join(format!(".exo/node/{run_id}/root.json"));
    if let Some(parent) = papers_path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(&papers_path, serde_json::to_vec_pretty(&papers)?)?;

    exo_runtime::write_node_agent_config(&cwd, &papers_path).await?;

    let model_flag = model.map(|m| format!(" --model {m}")).unwrap_or_default();
    // Embed the boot env directly in the launch command. The holding-shell pane was created
    // by `boot_root_session` BEFORE the `tmux set-environment` calls above, so it never picked
    // up the session vars — `claude` (and the `exo node` sidecar it spawns) would
    // otherwise start without EXOMONAD_SWARM_RUN_ID and fail bootstrap. The session-env calls
    // above still serve descendant panes spawned later.
    // `CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS=1` enables Teams (TeamCreate + the Teams inbox),
    // which is the Bus's last hop into a running CC session — without it, the root can't lead
    // a team and child messages fall back to raw tmux paste.
    // The launch string is pasted into a shell, so shell-escape the interpolated values.
    // `sanitize_session_name` only maps `.`→`_`, so a session name with shell metacharacters
    // would otherwise break the launch. (run_id is a UUID, but escape it too for uniformity.)
    let run_id_esc = shell_escape::escape(run_id.clone().into());
    let session_esc = shell_escape::escape(session.clone().into());
    // On `--recreate` (e.g. restarting after a binary update), continue the prior root
    // conversation so the restart doesn't discard the human's context — `claude --continue`
    // resumes the most recent conversation in this cwd. A fresh `init` has nothing to continue.
    let continue_flag = if recreate { " --continue" } else { "" };
    let launch = format!(
        "EXOMONAD_SWARM_RUN_ID={run_id_esc} EXOMONAD_TMUX_SESSION={session_esc} CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS=1 claude{continue_flag} --dangerously-skip-permissions{model_flag}"
    );

    exomonad_shared::services::tmux_ipc::TmuxIpc::new(&session)
        .inject_input(root_pane.as_str(), &launch)
        .await?;

    println!("Root node up in tmux session '{session}'. Attaching (detach: Ctrl-b d; reattach: tmux attach -t {session})...");

    // Attach the user into the root session, matching production `init`.
    exomonad_shared::services::tmux_ipc::TmuxIpc::attach_session(&session, None).await
}
