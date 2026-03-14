use crate::effects::{dispatch_process_effect, EffectContext, EffectError, EffectResult, ProcessEffects};
use async_trait::async_trait;
use exomonad_proto::effects::process::*;
use std::time::Duration;

pub struct ProcessHandler;

impl ProcessHandler {
    pub fn new() -> Self {
        Self
    }
}

crate::impl_pass_through_handler!(ProcessHandler, "process", dispatch_process_effect);

#[async_trait]
impl ProcessEffects for ProcessHandler {
    async fn run(&self, req: RunRequest, ctx: &EffectContext) -> EffectResult<RunResponse> {
        if req.working_dir.is_empty() {
            return Err(EffectError::invalid_input("working_dir is required"));
        }

        let agent_root = crate::services::agent_control::resolve_agent_working_dir(ctx);
        let resolved_dir = if std::path::Path::new(&req.working_dir).is_absolute() {
            std::path::PathBuf::from(&req.working_dir)
        } else {
            agent_root.join(&req.working_dir)
        };

        tracing::info!(
            command = %req.command,
            args = ?req.args,
            dir = %resolved_dir.display(),
            "[Process] run starting"
        );

        let mut cmd = tokio::process::Command::new(&req.command);
        cmd.args(&req.args).current_dir(&resolved_dir);

        if !req.env.is_empty() {
            cmd.envs(&req.env);
        }

        let output_result = if req.timeout_ms > 0 {
            match tokio::time::timeout(Duration::from_millis(req.timeout_ms), cmd.output()).await {
                Ok(res) => res,
                Err(_) => {
                    return Err(EffectError::Timeout {
                        message: format!(
                            "process.run timed out after {}ms: {} {}",
                            req.timeout_ms,
                            req.command,
                            req.args.join(" ")
                        ),
                    });
                }
            }
        } else {
            cmd.output().await
        };

        match output_result {
            Ok(output) => {
                let exit_code = output.status.code().unwrap_or(-1);
                let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
                let stderr = String::from_utf8_lossy(&output.stderr).into_owned();

                tracing::info!(exit_code, "[Process] run completed");

                Ok(RunResponse {
                    exit_code,
                    stdout,
                    stderr,
                })
            }
            Err(e) => Err(EffectError::custom(
                "process_error",
                format!("Failed to execute: {}", e),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::{AgentName, BirthBranch};
    use crate::effects::EffectContext;

    fn test_ctx() -> EffectContext {
        EffectContext {
            agent_name: AgentName::from("test"),
            birth_branch: BirthBranch::from("main"),
        }
    }

    #[tokio::test]
    async fn test_run_echo() {
        let handler = ProcessHandler::new();
        let ctx = test_ctx();

        let req = RunRequest {
            command: "echo".into(),
            args: vec!["hello".into()],
            working_dir: ".".into(),
            env: std::collections::HashMap::new(),
            timeout_ms: 1000,
        };

        let resp = handler.run(req, &ctx).await.unwrap();
        assert_eq!(resp.exit_code, 0);
        assert!(resp.stdout.contains("hello"));
    }

    #[tokio::test]
    async fn test_run_empty_working_dir_rejected() {
        let handler = ProcessHandler::new();
        let ctx = test_ctx();

        let req = RunRequest {
            command: "echo".into(),
            args: vec!["hello".into()],
            working_dir: "".into(),
            env: std::collections::HashMap::new(),
            timeout_ms: 1000,
        };

        let err = handler.run(req, &ctx).await.unwrap_err();
        assert!(matches!(err, EffectError::InvalidInput { .. }));
    }
}
