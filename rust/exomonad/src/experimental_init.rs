use crate::config::Config;
use anyhow::Result;
use std::process::Command;

pub async fn run(config: &Config, session: Option<String>, recreate: bool) -> Result<()> {
    let session = session.unwrap_or_else(|| format!("{}-exp", config.tmux_session));
    let run_id = uuid::Uuid::new_v4().to_string();
    let cwd = std::env::current_dir()?;

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

    let papers = exo_caps::NodePapers::root(root_pane.clone());
    let papers_path = cwd.join(format!(".exo/node/{run_id}/root.json"));
    if let Some(parent) = papers_path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(&papers_path, serde_json::to_vec_pretty(&papers)?)?;

    exo_runtime::write_node_agent_config(&cwd, &papers_path).await?;

    let model_flag = config
        .model
        .as_deref()
        .map(|m| format!(" --model {m}"))
        .unwrap_or_default();
    // Embed the boot env directly in the launch command. The holding-shell pane was created
    // by `boot_root_session` BEFORE the `tmux set-environment` calls above, so it never picked
    // up the session vars — `claude` (and the `experimental node` sidecar it spawns) would
    // otherwise start without EXOMONAD_SWARM_RUN_ID and fail bootstrap. The session-env calls
    // above still serve descendant panes spawned later.
    let launch = format!(
        "EXOMONAD_SWARM_RUN_ID={run_id} EXOMONAD_TMUX_SESSION={session} claude --dangerously-skip-permissions{model_flag}"
    );

    exomonad_core::services::tmux_ipc::TmuxIpc::new(&session)
        .inject_input(root_pane.as_str(), &launch)
        .await?;

    println!("Root node up in tmux session '{session}'. Attaching (detach: Ctrl-b d; reattach: tmux attach -t {session})...");

    // Attach the user into the root session, matching production `init`.
    exomonad_core::services::tmux_ipc::TmuxIpc::attach_session(&session, None).await
}
