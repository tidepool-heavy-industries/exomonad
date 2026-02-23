//! Template effect handler for the `template.*` namespace.
//!
//! Renders worker prompts by loading profiles, context files, and verify templates.

use crate::effects::{dispatch_template_effect, EffectContext, EffectResult, TemplateEffects};
use async_trait::async_trait;
use exomonad_proto::effects::template::*;
use std::path::PathBuf;
use tracing::{info, warn};

/// Template effect handler.
pub struct TemplateHandler {
    project_dir: PathBuf,
}

impl TemplateHandler {
    /// Create a new template handler.
    pub fn new(project_dir: PathBuf) -> Self {
        Self { project_dir }
    }
}

crate::impl_pass_through_handler!(TemplateHandler, "template", dispatch_template_effect);

#[async_trait]
impl TemplateEffects for TemplateHandler {
    async fn render_worker_prompt(
        &self,
        req: RenderWorkerPromptRequest,
        _ctx: &EffectContext,
    ) -> EffectResult<RenderWorkerPromptResponse> {
        info!(task = %req.task, "Rendering worker prompt");
        let mut boundary = req.boundary.clone();
        let mut verify = req.verify.clone();

        // 1. Load profiles from .exo/templates/profiles/{name}.md
        for profile in req.profiles {
            let path = self
                .project_dir
                .join(".exo/templates/profiles")
                .join(format!("{}.md", profile));
            match tokio::fs::read_to_string(&path).await {
                Ok(content) => {
                    // Prepend non-empty lines to boundary list
                    let lines: Vec<String> = content
                        .lines()
                        .map(|l| l.trim().to_string())
                        .filter(|l| !l.is_empty())
                        .collect();
                    for line in lines.into_iter().rev() {
                        boundary.insert(0, line);
                    }
                }
                Err(e) => {
                    warn!(profile = %profile, path = %path.display(), error = %e, "Failed to load profile")
                }
            }
        }

        // 2. Load context files
        let mut context_parts = Vec::new();
        if !req.context.is_empty() {
            context_parts.push(req.context.clone());
        }

        for file_ref in req.context_files {
            let (path_str, range) = parse_file_ref(&file_ref);
            let path = self.project_dir.join(path_str);
            match tokio::fs::read_to_string(&path).await {
                Ok(content) => {
                    let sliced = apply_line_range(&content, range);
                    let mut part = format!("File: `{}`", path_str);
                    if let Some((start, end)) = range {
                        part.push_str(&format!(" (lines {}-{})", start, end));
                    }
                    part.push_str("\n```\n");
                    part.push_str(&sliced);
                    if !sliced.ends_with('\n') {
                        part.push('\n');
                    }
                    part.push_str("```");
                    context_parts.push(part);
                }
                Err(e) => {
                    warn!(file = %file_ref, path = %path.display(), error = %e, "Failed to load context file")
                }
            }
        }

        // 3. Load verify templates from .exo/templates/verify/{name}.sh
        for vt in req.verify_templates {
            let path = self
                .project_dir
                .join(".exo/templates/verify")
                .join(format!("{}.sh", vt));
            match tokio::fs::read_to_string(&path).await {
                Ok(content) => {
                    // Prepend non-empty lines to verify list
                    let lines: Vec<String> = content
                        .lines()
                        .map(|l| l.trim().to_string())
                        .filter(|l| !l.is_empty())
                        .collect();
                    for line in lines.into_iter().rev() {
                        verify.insert(0, line);
                    }
                }
                Err(e) => {
                    warn!(template = %vt, path = %path.display(), error = %e, "Failed to load verify template")
                }
            }
        }

        // 4. Render markdown prompt
        let mut rendered = String::new();

        if !req.task.is_empty() {
            rendered.push_str("## TASK\n");
            rendered.push_str(&req.task);
            rendered.push_str("\n\n");
        }

        if !boundary.is_empty() {
            rendered.push_str("## BOUNDARY\n");
            for line in boundary {
                rendered.push_str(&format!("- {}\n", line));
            }
            rendered.push('\n');
        }

        if !req.read_first.is_empty() {
            rendered.push_str("## READ FIRST\n");
            for path in req.read_first {
                rendered.push_str(&format!("- `{}`\n", path));
            }
            rendered.push('\n');
        }

        if !req.steps.is_empty() {
            rendered.push_str("## STEPS\n");
            for (i, step) in req.steps.into_iter().enumerate() {
                rendered.push_str(&format!("{}. {}\n", i + 1, step));
            }
            rendered.push('\n');
        }

        if !context_parts.is_empty() {
            rendered.push_str("## CONTEXT\n");
            for part in context_parts {
                rendered.push_str(&part);
                rendered.push('\n');
            }
            rendered.push('\n');
        }

        if !verify.is_empty() {
            rendered.push_str("## VERIFY\n");
            for cmd in verify {
                rendered.push_str(&format!("- `{}`\n", cmd));
            }
            rendered.push('\n');
        }

        if !req.done_criteria.is_empty() {
            rendered.push_str("## DONE CRITERIA\n");
            for criterion in req.done_criteria {
                rendered.push_str(&format!("- {}\n", criterion));
            }
            rendered.push('\n');
        }

        Ok(RenderWorkerPromptResponse {
            rendered: rendered.trim().to_string(),
        })
    }
}

fn parse_file_ref(file_ref: &str) -> (&str, Option<(usize, usize)>) {
    // "path/to/file.rs:10-20" → ("path/to/file.rs", Some((10, 20)))
    // "path/to/file.rs" → ("path/to/file.rs", None)
    if let Some((path, range)) = file_ref.rsplit_once(':') {
        if let Some((start, end)) = range.split_once('-') {
            if let (Ok(s), Ok(e)) = (start.parse::<usize>(), end.parse::<usize>()) {
                return (path, Some((s, e)));
            }
        }
    }
    (file_ref, None)
}

fn apply_line_range(content: &str, range: Option<(usize, usize)>) -> String {
    match range {
        Some((start, end)) => content
            .lines()
            .skip(start.saturating_sub(1))
            .take(end.saturating_sub(start.saturating_sub(1)).max(0))
            .collect::<Vec<_>>()
            .join("\n"),
        None => content.to_string(),
    }
}
