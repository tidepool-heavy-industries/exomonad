//! Per-channel delivery primitives for the capability-driven delivery pipeline.
//!
//! Each `try_*_channel` attempts delivery via a single transport ONCE and reports
//! an honest `ChannelOutcome`. The executor (`execute_plan`) walks a
//! `Vec<DeliveryChannel>` produced by `delivery_plan` and stops at the first
//! non-`Failed` outcome. Retry + fallback are expressed as the plan list itself,
//! not hidden inside these helpers.

use crate::services::delivery::{
    ChannelOutcome, DeliveryChannel, DeliveryResult, FailureReason, VerifyOutcome,
};
use crate::services::{HasAcpRegistry, HasProjectDir, HasTeamRegistry, HasTmuxIpc};
use agent_client_protocol::{Agent, PromptRequest};
use claude_teams_bridge::TeamInfo;
use tokio::sync::oneshot;
use tracing::{info, instrument, warn};

#[instrument(skip_all, fields(team = %team_info.team_name, inbox = %team_info.inbox_name))]
pub async fn try_teams_channel(
    team_info: &TeamInfo,
    from: &crate::domain::AgentName,
    message: &str,
    summary: &str,
) -> Result<ChannelOutcome, String> {
    // Mirror the existing Teams inbox write (no retry here — executor handles retry/fallback via plan list).
    let timestamp = match claude_teams_bridge::write_to_inbox(
        &team_info.team_name,
        &team_info.inbox_name,
        from.as_str(),
        message,
        summary,
    ) {
        Ok(ts) => ts,
        Err(e) => return Ok(ChannelOutcome::Failed(format!("teams write: {e}"))),
    };

    tracing::info!(
        otel.name = "message.delivery",
        agent_id = %from,
        method = "teams_inbox",
        outcome = "success",
        detail = format!("{}/{}", team_info.team_name, team_info.inbox_name),
        "[event] message.delivery"
    );

    let (tx, rx) = oneshot::channel::<VerifyOutcome>();
    let team_name = team_info.team_name.clone();
    let inbox_name = team_info.inbox_name.clone();
    tokio::spawn(async move {
        let policy = crate::services::resilience::RetryPolicy::new(
            3,
            crate::services::resilience::Backoff::Fixed(std::time::Duration::from_secs(10)),
        );
        let verified = crate::services::resilience::retry(&policy, || {
            let is_read = claude_teams_bridge::is_message_read(&team_name, &inbox_name, &timestamp);
            async move {
                if is_read {
                    Ok(())
                } else {
                    anyhow::bail!("message not yet read")
                }
            }
        })
        .await;
        let outcome = if verified.is_ok() {
            VerifyOutcome::Confirmed
        } else {
            VerifyOutcome::VerificationFailed(format!(
                "teams inbox {}/{} not read within verification window",
                team_name, inbox_name
            ))
        };
        let _ = tx.send(outcome);
    });

    Ok(ChannelOutcome::Queued(rx))
}

#[instrument(skip_all, fields(agent_key = %agent_key))]
pub async fn try_acp_channel<C: HasAcpRegistry>(
    ctx: &C,
    agent_key: &str,
    from: &crate::domain::AgentName,
    message: &str,
) -> Result<ChannelOutcome, String> {
    let Some(conn) = ctx.acp_registry().get(agent_key).await else {
        return Ok(ChannelOutcome::Failed(
            "no ACP connection registered".into(),
        ));
    };
    match conn
        .conn
        .prompt(PromptRequest::new(
            conn.session_id.clone(),
            vec![message.into()],
        ))
        .await
    {
        Ok(_) => {
            info!(agent = %agent_key, "Delivered message via ACP prompt");
            tracing::info!(
                otel.name = "message.delivery",
                agent_id = %from,
                recipient = %agent_key,
                method = "acp",
                outcome = "success",
                "[event] message.delivery"
            );
            Ok(ChannelOutcome::Confirmed)
        }
        Err(e) => {
            warn!(agent = %agent_key, error = ?e, "ACP prompt failed");
            tracing::info!(
                otel.name = "message.delivery",
                agent_id = %from,
                recipient = %agent_key,
                method = "acp",
                outcome = "failed",
                detail = ?e,
                "[event] message.delivery"
            );
            if super::delivery::is_acp_connection_error(&e) {
                ctx.acp_registry().remove(agent_key).await;
            }
            Ok(ChannelOutcome::Failed(format!("acp prompt: {e:?}")))
        }
    }
}

#[instrument(skip_all, fields(socket = %socket_path.display()))]
pub async fn try_uds_channel(
    socket_path: &std::path::Path,
    from: &crate::domain::AgentName,
    message: &str,
    summary: &str,
) -> Result<ChannelOutcome, String> {
    if !socket_path.exists() {
        return Ok(ChannelOutcome::Failed(format!(
            "UDS socket not present: {}",
            socket_path.display()
        )));
    }
    match super::delivery::deliver_via_uds(socket_path, from.as_str(), message, summary).await {
        Ok(()) => {
            tracing::info!(
                otel.name = "message.delivery",
                agent_id = %from,
                method = "unix_socket",
                outcome = "success",
                detail = %socket_path.to_string_lossy(),
                "[event] message.delivery"
            );
            Ok(ChannelOutcome::Confirmed)
        }
        Err(e) => {
            tracing::info!(
                otel.name = "message.delivery",
                agent_id = %from,
                method = "unix_socket",
                outcome = "failed",
                detail = %e,
                "[event] message.delivery"
            );
            Ok(ChannelOutcome::Failed(format!("uds: {e}")))
        }
    }
}

#[instrument(skip_all, fields(target = %tmux_target))]
pub async fn try_tmux_channel<C: HasTmuxIpc>(
    ctx: &C,
    tmux_target: &str,
    message: &str,
    working_dir: &std::path::Path,
) -> Result<ChannelOutcome, String> {
    match crate::services::tmux_events::inject_input(
        ctx.tmux_ipc(),
        tmux_target,
        message,
        working_dir,
    )
    .await
    {
        Ok(()) => {
            tracing::info!(
                otel.name = "message.delivery",
                method = "tmux",
                outcome = "success",
                detail = %tmux_target,
                "[event] message.delivery"
            );
            Ok(ChannelOutcome::Confirmed)
        }
        Err(e) => {
            warn!(target = %tmux_target, error = %e, "tmux inject_input failed");
            tracing::info!(
                otel.name = "message.delivery",
                method = "tmux",
                outcome = "failed",
                detail = %e,
                "[event] message.delivery"
            );
            Ok(ChannelOutcome::Failed(format!("tmux: {e}")))
        }
    }
}

/// Context carrying everything `execute_plan` may need to try any channel.
/// `team_info` and `uds_socket` are optional; if missing, attempts via those
/// channels report `Failed`.
pub struct PlanContext<'a> {
    pub agent_key: &'a str,
    pub tmux_target: &'a str,
    pub from: &'a crate::domain::AgentName,
    pub message: &'a str,
    pub summary: &'a str,
    pub team_info: Option<&'a TeamInfo>,
    pub uds_socket: Option<&'a std::path::Path>,
    pub tmux_working_dir: &'a std::path::Path,
}

pub async fn execute_plan<C>(
    plan: Vec<DeliveryChannel>,
    ctx: &C,
    pctx: &PlanContext<'_>,
) -> DeliveryResult
where
    C: HasAcpRegistry + HasTmuxIpc + HasTeamRegistry + HasProjectDir,
{
    if plan.is_empty() {
        // RecipientMeta is not available here, so use the conservative default
        // rather than fabricating metadata for Undeliverable.
        return DeliveryResult::Failed(FailureReason::AllChannelsExhausted);
    }

    for channel in plan {
        let outcome_res = match channel {
            DeliveryChannel::Teams => match pctx.team_info {
                Some(info) => try_teams_channel(info, pctx.from, pctx.message, pctx.summary).await,
                None => Ok(ChannelOutcome::Failed("no team_info available".into())),
            },
            DeliveryChannel::Acp => {
                try_acp_channel(ctx, pctx.agent_key, pctx.from, pctx.message).await
            }
            DeliveryChannel::Uds => match pctx.uds_socket {
                Some(path) => try_uds_channel(path, pctx.from, pctx.message, pctx.summary).await,
                None => Ok(ChannelOutcome::Failed("no uds socket available".into())),
            },
            DeliveryChannel::Tmux => {
                try_tmux_channel(ctx, pctx.tmux_target, pctx.message, pctx.tmux_working_dir).await
            }
        };
        match outcome_res {
            Ok(ChannelOutcome::Confirmed) => return DeliveryResult::Confirmed(channel),
            Ok(ChannelOutcome::Queued(rx)) => return DeliveryResult::QueuedUnverified(channel, rx),
            Ok(ChannelOutcome::Failed(reason)) => {
                tracing::debug!(channel = ?channel, reason = %reason, "channel attempt failed; continuing plan");
                continue;
            }
            Err(e) => {
                tracing::warn!(channel = ?channel, error = %e, "channel attempt errored; continuing plan");
                continue;
            }
        }
    }
    DeliveryResult::Failed(FailureReason::AllChannelsExhausted)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::AgentName;
    use crate::services::Services;
    use std::sync::Arc;

    #[tokio::test]
    async fn test_execute_plan_exhausted() {
        let services = Services::test();

        let from = AgentName::from("sender");
        let pctx = PlanContext {
            agent_key: "agent",
            tmux_target: "target",
            from: &from,
            message: "msg",
            summary: "sum",
            team_info: None,
            uds_socket: None,
            tmux_working_dir: std::path::Path::new("."),
        };

        let plan = vec![DeliveryChannel::Teams, DeliveryChannel::Uds];
        let result = execute_plan(plan, &services, &pctx).await;

        match result {
            DeliveryResult::Failed(FailureReason::AllChannelsExhausted) => {}
            _ => panic!("Expected AllChannelsExhausted, got {:?}", result),
        }
    }

    #[tokio::test]
    async fn test_execute_plan_empty() {
        let services = Services::test();

        let from = AgentName::from("sender");
        let pctx = PlanContext {
            agent_key: "agent",
            tmux_target: "target",
            from: &from,
            message: "msg",
            summary: "sum",
            team_info: None,
            uds_socket: None,
            tmux_working_dir: std::path::Path::new("."),
        };

        let plan = vec![];
        let result = execute_plan(plan, &services, &pctx).await;

        match result {
            DeliveryResult::Failed(FailureReason::AllChannelsExhausted) => {}
            _ => panic!("Expected AllChannelsExhausted, got {:?}", result),
        }
    }

    #[tokio::test]
    async fn test_execute_plan_confirmed_tmux() {
        if !crate::services::tmux_ipc::IsolatedTmux::is_available().await {
            return;
        }
        let isolated = crate::services::tmux_ipc::IsolatedTmux::new()
            .await
            .expect("tmux unavailable");
        let services = Services::test_with_tmux(Arc::new(isolated.ipc.clone()));

        let window_id = isolated
            .ipc
            .new_window(
                "test-window",
                std::path::Path::new("/tmp"),
                "/bin/sh",
                "sleep 100",
            )
            .await
            .unwrap();

        let from = AgentName::from("sender");
        let pctx = PlanContext {
            agent_key: "agent",
            tmux_target: window_id.as_str(),
            from: &from,
            message: "msg",
            summary: "sum",
            team_info: None,
            uds_socket: None,
            tmux_working_dir: std::path::Path::new("."),
        };

        let plan = vec![DeliveryChannel::Tmux];
        let result = execute_plan(plan, &services, &pctx).await;

        match result {
            DeliveryResult::Confirmed(DeliveryChannel::Tmux) => {}
            _ => panic!("Expected Confirmed(Tmux), got {:?}", result),
        }
    }

    #[tokio::test]
    async fn test_try_tmux_channel_confirmed() {
        if !crate::services::tmux_ipc::IsolatedTmux::is_available().await {
            return;
        }
        let isolated = crate::services::tmux_ipc::IsolatedTmux::new()
            .await
            .expect("tmux unavailable");
        let services = Services::test_with_tmux(Arc::new(isolated.ipc.clone()));

        let window_id = isolated
            .ipc
            .new_window(
                "test-window-2",
                std::path::Path::new("/tmp"),
                "/bin/sh",
                "sleep 100",
            )
            .await
            .unwrap();

        let result = try_tmux_channel(
            &services,
            window_id.as_str(),
            "msg",
            std::path::Path::new("."),
        )
        .await;

        match result {
            Ok(ChannelOutcome::Confirmed) => {}
            _ => panic!("Expected Confirmed, got {:?}", result),
        }
    }
}
