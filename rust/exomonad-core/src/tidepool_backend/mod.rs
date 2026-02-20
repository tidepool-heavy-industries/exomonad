use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Result;
use async_trait::async_trait;
use serde_json::Value as JsonValue;
use tracing::debug;

use tidepool_bridge::FromCore;
use tidepool_effect::dispatch::DispatchEffect;
use tidepool_eval::value::Value;

use crate::effects::EffectContext;
use crate::mcp::tools::MCPCallOutput;
use crate::mcp::ToolDefinition;
use crate::runtime_backend::RuntimeBackend;

pub mod bridge_types;
pub mod handlers;
pub mod tool_defs;

use bridge_types::*;
use handlers::*;
use tool_defs::*;

#[cfg(test)]
mod tests;

pub struct TidepoolBackend {
    /// Compiled Haskell tool expressions + data constructor tables.
    pub tools: HashMap<String, CompiledTool>,

    /// Agent control service for spawn operations.
    pub agent_control: Arc<crate::services::agent_control::AgentControlService>,

    /// JJ workspace service for file_pr and merge_pr.
    pub jj: Arc<crate::services::jj_workspace::JjWorkspaceService>,

    /// Question registry for answer_question (bridges oneshot channels).
    pub question_registry: Arc<crate::services::questions::QuestionRegistry>,

    /// Project directory for inbox file paths.
    pub project_dir: std::path::PathBuf,

    /// Zellij session name for popup and other UI services.
    pub zellij_session: Option<String>,

    /// Agent identity context, threaded as user data to effect handlers.
    pub ctx: EffectContext,
}

impl TidepoolBackend {
    pub fn new(zellij_session: Option<String>, ctx: EffectContext) -> Self {
        let mut tools = HashMap::new();

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/PopupEffect.hs::popupTool");
        tools.insert(
            "popup".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/FilePREffect.hs::filePRTool");
        tools.insert(
            "file_pr".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/MergePREffect.hs::mergePRTool");
        tools.insert(
            "merge_pr".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/NotifyEffect.hs::notifyTool");
        tools.insert(
            "notify_parent".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/SpawnEffect.hs::spawnSubtreeTool");
        tools.insert(
            "spawn_subtree".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/SpawnLeafEffect.hs::spawnLeafTool");
        tools.insert(
            "spawn_leaf_subtree".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/SpawnWorkersEffect.hs::spawnWorkersTool");
        tools.insert(
            "spawn_workers".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/NoteEffect.hs::noteTool");
        tools.insert(
            "note".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/AnswerEffect.hs::answerTool");
        tools.insert(
            "answer_question".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let (expr, table) =
            tidepool_macro::haskell_expr!("haskell/MessagesEffect.hs::messagesTool");
        tools.insert(
            "get_agent_messages".to_string(),
            CompiledTool {
                expr,
                table: Arc::new(table),
            },
        );

        let working_dir = crate::services::agent_control::resolve_agent_working_dir(&ctx);
        let jj = Arc::new(crate::services::jj_workspace::JjWorkspaceService::new(
            working_dir.clone(),
        ));

        let agent_control = Arc::new(
            crate::services::agent_control::AgentControlService::new(
                working_dir.clone(),
                None,
                jj.clone(),
            )
            .with_birth_branch(ctx.birth_branch.clone())
            .with_zellij_session(zellij_session.clone().unwrap_or_default()),
        );

        let question_registry = Arc::new(crate::services::questions::QuestionRegistry::new());

        Self {
            tools,
            agent_control,
            jj,
            question_registry,
            project_dir: working_dir,
            zellij_session,
            ctx,
        }
    }

    /// Run a compiled Haskell tool through the EffectMachine with the given handlers.
    pub fn run_effect<H: DispatchEffect<EffectContext>>(
        &self,
        tool_name: &str,
        handlers: &mut H,
    ) -> Result<Value, anyhow::Error> {
        let tool = self
            .tools
            .get(tool_name)
            .ok_or_else(|| anyhow::anyhow!("No compiled tool: {}", tool_name))?;
        let mut heap = tidepool_eval::heap::VecHeap::new();
        let mut machine = tidepool_effect::EffectMachine::new(&tool.table, &mut heap)
            .map_err(|e| anyhow::anyhow!("EffectMachine init: {}", e))?;
        machine
            .run_with_user(&tool.expr, handlers, &self.ctx)
            .map_err(|e| anyhow::anyhow!("EffectMachine run: {}", e))
    }

    pub fn run_and_decode<R: FromCore>(
        &self,
        tool_name: &str,
        handlers: &mut impl DispatchEffect<EffectContext>,
        result_to_json: impl FnOnce(R) -> serde_json::Value,
    ) -> Result<MCPCallOutput> {
        match self.run_effect(tool_name, handlers) {
            Ok(result_value) => {
                let table = &self.tools[tool_name].table;
                let result = R::from_value(&result_value, table)
                    .map_err(|e| anyhow::anyhow!("Bridge decode error: {}", e))?;
                Ok(MCPCallOutput {
                    success: true,
                    result: Some(result_to_json(result)),
                    error: None,
                })
            }
            Err(e) => {
                tracing::error!(tool = %tool_name, error = %e, "Tidepool tool failed");
                Ok(MCPCallOutput {
                    success: false,
                    result: None,
                    error: Some(e.to_string()),
                })
            }
        }
    }

    async fn call_popup(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: PopupArgs = serde_json::from_value(args)?;
        let title = args.title.unwrap_or_default();

        let components = if let Some(panes) = args.panes {
            let mut wizard = serde_json::Map::new();
            wizard.insert("panes".to_string(), serde_json::to_value(panes)?);
            if let Some(s) = args.start {
                wizard.insert("start".to_string(), serde_json::Value::String(s));
            }
            serde_json::to_string(&JsonValue::Object(wizard)).unwrap_or_default()
        } else if let Some(elements) = args.elements {
            serde_json::to_string(&elements).unwrap_or_default()
        } else {
            "[]".to_string()
        };

        debug!(
            title = %title,
            components_len = components.len(),
            "Tidepool popup call via EffectMachine"
        );

        // HList order must match Haskell Eff '[PopupInput', Identity, PopupOp]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(ToolInput { title, components }),
            },
            IdentityHandler,
            PopupOpHandler {
                zellij_session: self.zellij_session.clone(),
            },
        ];

        self.run_and_decode("popup", &mut handlers, |response: PopupResponse| {
            let values: JsonValue = serde_json::from_str(&response.values)
                .unwrap_or(JsonValue::Object(serde_json::Map::new()));
            serde_json::json!({
                "button": response.button,
                "values": values,
            })
        })
    }

    async fn call_file_pr(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: FilePRArgs = serde_json::from_value(args)?;
        debug!(title = %args.title, "Tidepool file_pr call via EffectMachine");

        // HList order must match Haskell Eff '[FilePRInput', Identity, FilePROp]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(FilePRToolInput {
                    title: args.title,
                    body: args.body,
                    base_branch: args.base_branch.unwrap_or_default(),
                }),
            },
            IdentityHandler,
            FilePROpHandler {
                jj: self.jj.clone(),
            },
        ];

        self.run_and_decode("file_pr", &mut handlers, |result: FilePRToolResult| {
            serde_json::json!({
                "pr_url": result.pr_url,
                "pr_number": result.pr_number.parse::<u64>().unwrap_or(0),
                "head_branch": result.head_branch,
                "base_branch": result.result_base,
                "created": result.created == "true",
            })
        })
    }

    async fn call_merge_pr(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: MergePRArgs = serde_json::from_value(args)?;
        debug!(
            pr_number = args.pr_number,
            strategy = %args.strategy.as_deref().unwrap_or("squash"),
            "Tidepool merge_pr call via EffectMachine"
        );

        // HList order must match Haskell Eff '[MergePRInput', Identity, MergePROp]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(MergePRToolInput {
                    pr_number: args.pr_number.to_string(),
                    strategy: args.strategy.unwrap_or_else(|| "squash".to_string()),
                }),
            },
            IdentityHandler,
            MergePROpHandler {
                jj: self.jj.clone(),
            },
        ];

        self.run_and_decode("merge_pr", &mut handlers, |result: MergePRToolResult| {
            serde_json::json!({
                "success": result.success == "true",
                "message": result.message,
                "jj_fetched": result.jj_fetched == "true",
            })
        })
    }

    async fn call_notify_parent(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: NotifyParentArgs = serde_json::from_value(args)?;
        debug!(status = %args.status, "Tidepool notify_parent call via EffectMachine");

        // HList order must match Haskell Eff '[NotifyInput', Identity, FormatOp, NotifyOp]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(NotifyToolInput {
                    status: args.status,
                    message: args.message,
                }),
            },
            IdentityHandler,
            FormatOpHandler,
            NotifyOpHandler,
        ];

        self.run_and_decode(
            "notify_parent",
            &mut handlers,
            |result: NotifyToolResult| serde_json::json!({"ack": result.ack}),
        )
    }

    async fn call_spawn_subtree(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: SpawnSubtreeArgs = serde_json::from_value(args)?;
        debug!(branch = %args.branch_name, "Tidepool spawn_subtree call via EffectMachine");

        // HList order must match Haskell Eff '[SpawnSubtreeInput', SpawnSubtreeOp]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(SpawnSubtreeToolInput {
                    task: args.task,
                    branch_name: args.branch_name,
                }),
            },
            SpawnSubtreeOpHandler {
                agent_control: self.agent_control.clone(),
            },
        ];

        self.run_and_decode(
            "spawn_subtree",
            &mut handlers,
            |result: SpawnSubtreeToolResult| {
                serde_json::json!({
                    "tab_name": result.tab_name,
                    "branch_name": result.branch_name,
                })
            },
        )
    }

    async fn call_spawn_leaf(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: SpawnLeafArgs = serde_json::from_value(args)?;
        debug!(branch = %args.branch_name, "Tidepool spawn_leaf_subtree call via EffectMachine");

        // HList order must match Haskell Eff '[SpawnLeafInput', SpawnLeafOp]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(SpawnLeafToolInput {
                    task: args.task,
                    branch_name: args.branch_name,
                }),
            },
            SpawnLeafOpHandler {
                agent_control: self.agent_control.clone(),
            },
        ];

        self.run_and_decode(
            "spawn_leaf_subtree",
            &mut handlers,
            |result: SpawnLeafToolResult| {
                serde_json::json!({
                    "tab_name": result.tab_name,
                    "branch_name": result.branch_name,
                })
            },
        )
    }

    async fn call_spawn_workers(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: SpawnWorkersArgs = serde_json::from_value(args)?;
        debug!(count = args.specs.len(), "Tidepool spawn_workers call via EffectMachine");

        // Pre-render specs into WorkerSpecBridge (name + prompt)
        let worker_specs: Vec<WorkerSpecBridge> = args.specs
            .iter()
            .map(|spec| {
                let prompt = if let Some(p) = &spec.prompt {
                    p.clone()
                } else {
                    let mut parts = vec![format!("Task: {}", spec.task)];
                    if let Some(steps) = &spec.steps {
                        parts.push("Steps:".to_string());
                        for (i, s) in steps.iter().enumerate() {
                            parts.push(format!("{}. {}", i + 1, s));
                        }
                    }
                    if let Some(context) = &spec.context {
                        parts.push(format!("
Context:
{}", context));
                    }
                    if let Some(done) = &spec.done_criteria {
                        parts.push("Done when:".to_string());
                        for d in done {
                            parts.push(format!("- {}", d));
                        }
                    }
                    if let Some(verify) = &spec.verify {
                        parts.push("Verify:".to_string());
                        for v in verify {
                            parts.push(format!("$ {}", v));
                        }
                    }
                    if let Some(boundary) = &spec.boundary {
                        parts.push("DO NOT:".to_string());
                        for b in boundary {
                            parts.push(format!("- {}", b));
                        }
                    }
                    if let Some(read) = &spec.read_first {
                        parts.push("Read first:".to_string());
                        for r in read {
                            parts.push(format!("- {}", r));
                        }
                    }
                    parts.join("
")
                };
                WorkerSpecBridge { name: spec.name.clone(), prompt }
            })
            .collect();

        let spec_names: Vec<String> =
            worker_specs.iter().map(|s| s.name.clone()).collect();

        // HList order must match Haskell Eff '[SpawnWorkersInput', Identity, SpawnWorkerOp]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(worker_specs),
            },
            IdentityHandler,
            SpawnWorkerOpHandler {
                agent_control: self.agent_control.clone(),
            },
        ];

        self.run_and_decode(
            "spawn_workers",
            &mut handlers,
            |results: Vec<SpawnWorkerToolResult>| {
                let workers: Vec<_> = spec_names
                    .iter()
                    .zip(results.iter())
                    .map(|(name, r)| {
                        serde_json::json!({
                            "name": name,
                            "tab_name": r.tab_name,
                            "success": true,
                        })
                    })
                    .collect();
                serde_json::json!({ "workers": workers })
            },
        )
    }

    async fn call_note(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: NoteArgs = serde_json::from_value(args)?;
        debug!(agent = %self.ctx.agent_name, "Tidepool note call via EffectMachine");

        // HList order must match Haskell Eff '[NoteInput', Identity, FormatOp, Inbox, NoteOp]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(NoteToolInput {
                    content: args.content,
                }),
            },
            IdentityHandler,
            FormatOpHandler,
            InboxHandler {
                project_dir: self.project_dir.clone(),
            },
            NoteOpHandler,
        ];

        self.run_and_decode("note", &mut handlers, |_: NoteToolResult| {
            serde_json::json!({"ack": true})
        })
    }

    async fn call_answer_question(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: AnswerQuestionArgs = serde_json::from_value(args)?;
        debug!(
            agent = %args.agent_id,
            question_id = %args.question_id,
            "Tidepool answer_question call via EffectMachine"
        );

        // HList order must match Haskell Eff '[AnswerInput', Inbox, Questions]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(AnswerToolInput {
                    agent_id: args.agent_id,
                    question_id: args.question_id,
                    answer: args.answer,
                }),
            },
            InboxHandler {
                project_dir: self.project_dir.clone(),
            },
            QuestionsHandler {
                registry: self.question_registry.clone(),
            },
        ];

        self.run_and_decode(
            "answer_question",
            &mut handlers,
            |result: AnswerToolResult| {
                serde_json::json!({
                    "status": result.status,
                    "agent_id": result.agent_id,
                    "question_id": result.question_id,
                })
            },
        )
    }

    async fn call_get_messages(&self, args: JsonValue) -> Result<MCPCallOutput> {
        let args: GetAgentMessagesArgs = serde_json::from_value(args)?;
        debug!(
            agent = %args.agent_id.as_deref().unwrap_or(""),
            timeout_secs = args.timeout_secs.unwrap_or(0),
            "Tidepool get_agent_messages call via EffectMachine"
        );

        // HList order must match Haskell Eff '[MessagesInput', MessagesOp]
        let mut handlers = frunk::hlist![
            ToolInputHandler {
                input: Some(MessagesToolInput {
                    agent_id: args.agent_id.unwrap_or_default(),
                    timeout_secs: args.timeout_secs.unwrap_or(0).to_string(),
                }),
            },
            MessagesOpHandler {
                project_dir: self.project_dir.clone(),
            },
        ];

        self.run_and_decode(
            "get_agent_messages",
            &mut handlers,
            |result: MessagesToolResult| {
                let messages: JsonValue = serde_json::from_str(&result.messages_json)
                    .unwrap_or(JsonValue::Array(Vec::new()));
                let count = messages.as_array().map(|a| a.len()).unwrap_or(0);
                serde_json::json!({
                    "messages": messages,
                    "count": count,
                })
            },
        )
    }
}

#[async_trait]
impl RuntimeBackend for TidepoolBackend {
    async fn list_tools(&self, role: &str) -> Result<Vec<ToolDefinition>> {
        debug!(role = %role, "Listing tools from tidepool backend");
        Ok(match role {
            "tl" => vec![
                popup_tool_definition(),
                file_pr_tool_definition(),
                merge_pr_tool_definition(),
                notify_parent_tool_definition(),
                spawn_subtree_tool_definition(),
                spawn_leaf_subtree_tool_definition(),
                spawn_workers_tool_definition(),
                note_tool_definition(),
                answer_question_tool_definition(),
                get_agent_messages_tool_definition(),
            ],
            "dev" => vec![
                file_pr_tool_definition(),
                notify_parent_tool_definition(),
                note_tool_definition(),
            ],
            "worker" => vec![
                notify_parent_tool_definition(),
                note_tool_definition(),
            ],
            _ => vec![],
        })
    }

    async fn call_tool(
        &self,
        role: &str,
        tool_name: &str,
        args: JsonValue,
    ) -> Result<MCPCallOutput> {
        debug!(role = %role, tool = %tool_name, "Executing tool via tidepool");
        match tool_name {
            "popup" => self.call_popup(args).await,
            "file_pr" => self.call_file_pr(args).await,
            "merge_pr" => self.call_merge_pr(args).await,
            "notify_parent" => self.call_notify_parent(args).await,
            "spawn_subtree" => self.call_spawn_subtree(args).await,
            "spawn_leaf_subtree" => self.call_spawn_leaf(args).await,
            "spawn_workers" => self.call_spawn_workers(args).await,
            "note" => self.call_note(args).await,
            "answer_question" => self.call_answer_question(args).await,
            "get_agent_messages" => self.call_get_messages(args).await,
            _ => Ok(MCPCallOutput {
                success: false,
                result: None,
                error: Some(format!("Unknown tool: {}", tool_name)),
            }),
        }
    }

    async fn handle_hook(&self, input: &JsonValue) -> Result<JsonValue> {
        debug!("Handling hook via tidepool (passthrough)");
        let _ = input;
        Ok(serde_json::json!({"continue": true}))
    }

    async fn reload_if_changed(&self) -> Result<bool> {
        Ok(false)
    }

    fn content_hash(&self) -> String {
        "tidepool-static".to_string()
    }
}
