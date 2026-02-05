//! exomonad-sidecar: Rust host with Haskell WASM plugin.
//!
//! This binary runs as a sidecar in each agent container, handling:
//! - Claude Code hooks via WASM plugin
//! - MCP tools via local Rust implementation
//!
//! All IO is handled by Rust; Haskell WASM yields high-level semantic effects.

use exomonad::{config, mcp};

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use exomonad_runtime::services::{git, zellij_events};
use exomonad_runtime::{PluginManager, Services};
use exomonad_shared::protocol::{
    ClaudePreToolUseOutput, HookEventType, HookInput, InternalStopHookOutput, Runtime,
    ServiceRequest,
};
use std::path::PathBuf;
use std::sync::Arc;
use tokio::io::AsyncWriteExt;
use tokio::net::UnixStream;
use tracing::{debug, info, warn};

// ============================================================================
// CLI Types
// ============================================================================

#[derive(Parser)]
#[command(name = "exomonad")]
#[command(about = "ExoMonad: Rust host with Haskell WASM plugin for agent orchestration")]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Handle a Claude Code hook event via WASM plugin
    Hook {
        /// The hook event type to handle
        #[arg(value_enum)]
        event: HookEventType,

        /// The runtime environment (Claude or Gemini)
        #[arg(long, default_value = "claude")]
        runtime: Runtime,
    },

    /// Run as stdio MCP server (Claude Code spawns this)
    ///
    /// Reads config from .exomonad/config.toml in current directory.
    /// Claude Code should configure this in .mcp.json with type: "stdio".
    McpStdio,

    /// Reply to a UI request (sent by Zellij plugin)
    Reply {
        /// Request ID
        #[arg(long)]
        id: String,

        /// JSON payload
        #[arg(long)]
        payload: Option<String>,

        /// Cancel the request
        #[arg(long)]
        cancel: bool,
    },

    /// Warmup the WASM plugin cache
    Warmup {
        /// Path to the WASM file
        path: PathBuf,
    },
}

// ============================================================================
// Hook Handler
// ============================================================================

#[tracing::instrument(skip(plugin, runtime, event_type), fields(event = ?event_type))]
async fn handle_hook(
    plugin: &PluginManager,
    event_type: HookEventType,
    runtime: Runtime,
) -> Result<()> {
    use std::io::Read;

    // Read hook payload from stdin
    let mut stdin_content = String::new();
    std::io::stdin()
        .read_to_string(&mut stdin_content)
        .context("Failed to read stdin")?;

    debug!(
        runtime = ?runtime,
        payload_len = stdin_content.len(),
        "Received hook event"
    );

    // Emit HookReceived event
    let agent_id_str = get_agent_id_from_env();
    if agent_id_str != "unknown" {
        match exomonad_ui_protocol::AgentId::try_from(agent_id_str.clone()) {
            Ok(agent_id) => {
                let event = exomonad_ui_protocol::AgentEvent::HookReceived {
                    agent_id,
                    hook_type: event_type.to_string(),
                    timestamp: zellij_events::now_iso8601(),
                };
                if let Err(e) = zellij_events::emit_event(&event) {
                    warn!("Failed to emit hook:received event: {}", e);
                }
            }
            Err(e) => warn!("Invalid agent_id '{}': {}", agent_id_str, e),
        }
    }

    // Parse the hook input and inject runtime
    let mut hook_input: HookInput =
        serde_json::from_str(&stdin_content).context("Failed to parse hook input")?;
    hook_input.runtime = Some(runtime);
    hook_input.agent_id = if agent_id_str != "unknown" {
        Some(agent_id_str.clone())
    } else {
        None
    };

    // Normalize CLI-specific hook types to internal abstractions before WASM.
    // Both Claude's Stop and Gemini's AfterAgent represent "main agent stop".
    let is_stop_hook = matches!(
        event_type,
        HookEventType::Stop
            | HookEventType::AfterAgent
            | HookEventType::SubagentStop
            | HookEventType::SessionEnd
    );

    let normalized_event_name = match event_type {
        HookEventType::Stop | HookEventType::AfterAgent => "Stop",
        HookEventType::SubagentStop => "SubagentStop",
        HookEventType::SessionEnd => "SessionEnd",
        HookEventType::PreToolUse => "PreToolUse",
        _ => {
            // Pass through unhandled hooks with allow
            debug!(event = ?event_type, "Hook type not implemented in WASM, allowing");
            let output_json = serde_json::to_string(&ClaudePreToolUseOutput::default())
                .context("Failed to serialize output")?;
            println!("{}", output_json);
            return Ok(());
        }
    };
    hook_input.hook_event_name = normalized_event_name.to_string();

    // Handle stop hooks with runtime-specific output translation
    if is_stop_hook {
        // Emit AgentStopped event BEFORE calling WASM (matching previous behavior)
        // Only emit if we have a valid agent ID
        if agent_id_str != "unknown" {
            match exomonad_ui_protocol::AgentId::try_from(agent_id_str.clone()) {
                Ok(agent_id) => {
                    let event = exomonad_ui_protocol::AgentEvent::AgentStopped {
                        agent_id,
                        timestamp: zellij_events::now_iso8601(),
                    };
                    if let Err(e) = zellij_events::emit_event(&event) {
                        warn!("Failed to emit agent:stopped event: {}", e);
                    }
                }
                Err(e) => warn!("Invalid agent_id '{}' for stop event: {}", agent_id_str, e),
            }
        }

        // Call WASM and parse as internal domain type
        let internal_output: InternalStopHookOutput = plugin
            .call("handle_pre_tool_use", &hook_input)
            .await
            .context("WASM handle_pre_tool_use failed")?;

        // Translate to runtime-specific format at the edge
        let output_json = internal_output.to_runtime_json(&runtime);
        println!("{}", output_json);

        // Exit code and event emission based on decision
        if internal_output.decision == exomonad_shared::protocol::StopDecision::Block {
            // Emit StopHookBlocked event for SubagentStop hooks
            if event_type == HookEventType::SubagentStop {
                let agent_id_str = get_agent_id_from_env();
                if agent_id_str != "unknown" {
                    let reason = internal_output
                        .reason
                        .clone()
                        .unwrap_or_else(|| "Hook blocked agent stop".to_string());

                    match exomonad_ui_protocol::AgentId::try_from(agent_id_str.clone()) {
                        Ok(agent_id) => {
                            let event = exomonad_ui_protocol::AgentEvent::StopHookBlocked {
                                agent_id,
                                reason,
                                timestamp: zellij_events::now_iso8601(),
                            };
                            if let Err(e) = zellij_events::emit_event(&event) {
                                warn!("Failed to emit stop_hook:blocked event: {}", e);
                            }
                        }
                        Err(e) => warn!("Invalid agent_id '{}': {}", agent_id_str, e),
                    }
                }
            }
            std::process::exit(2);
        }
    } else {
        // Non-stop hooks: use existing ClaudePreToolUseOutput format
        let output: ClaudePreToolUseOutput = plugin
            .call("handle_pre_tool_use", &hook_input)
            .await
            .context("WASM handle_pre_tool_use failed")?;

        let output_json = serde_json::to_string(&output).context("Failed to serialize output")?;
        println!("{}", output_json);

        if !output.continue_ {
            std::process::exit(2);
        }
    }

    Ok(())
}

// ============================================================================
// Logging
// ============================================================================

/// Initialize logging based on the command mode.
/// - MCP stdio: Logs to ~/.exomonad/logs/sidecar-TIMESTAMP.log
/// - Other modes: Logs to stderr (preserves current behavior)
fn init_logging(command: &Commands) {
    let use_json = std::env::var("EXOMONAD_LOG_FORMAT")
        .map(|v| v.eq_ignore_ascii_case("json"))
        .unwrap_or(false);

    match command {
        Commands::McpStdio => {
            // File-based logging for stdio mode
            // Try to set up file logging, fallback to stderr (or nothing if really broken) if it fails
            let setup_file_logging = || -> Result<PathBuf, String> {
                let home_dir = std::env::var("HOME")
                    .map_err(|_| "HOME environment variable not set".to_string())?;
                let log_dir = PathBuf::from(home_dir).join(".exomonad").join("logs");

                // Create log directory if it doesn't exist
                std::fs::create_dir_all(&log_dir)
                    .map_err(|e| format!("Failed to create log directory: {}", e))?;

                // Generate timestamped filename
                let timestamp = chrono::Local::now().format("%Y-%m-%d-%H-%M-%S");
                let log_file = log_dir.join(format!("sidecar-{}.log", timestamp));

                let file = std::fs::OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(&log_file)
                    .map_err(|e| format!("Failed to open log file: {}", e))?;

                let builder = tracing_subscriber::fmt()
                    .with_env_filter(
                        tracing_subscriber::EnvFilter::from_default_env()
                            .add_directive(tracing::Level::INFO.into()),
                    )
                    .with_writer(std::sync::Arc::new(file))
                    .with_ansi(false); // No ANSI colors in file

                if use_json {
                    builder.json().init();
                } else {
                    builder.init();
                }

                Ok(log_file)
            };

            match setup_file_logging() {
                Ok(log_file) => eprintln!("MCP stdio logging to: {}", log_file.display()),
                Err(e) => {
                    eprintln!(
                        "Failed to setup file logging for MCP stdio: {}. Falling back to stderr.",
                        e
                    );
                    // Fallback to stderr
                    let builder = tracing_subscriber::fmt()
                        .with_env_filter(
                            tracing_subscriber::EnvFilter::from_default_env()
                                .add_directive(tracing::Level::INFO.into()),
                        )
                        .with_writer(std::io::stderr);

                    if use_json {
                        builder.json().init();
                    } else {
                        builder.init();
                    }
                }
            }
        }
        _ => {
            // Stderr logging for other modes (existing behavior)
            let builder = tracing_subscriber::fmt()
                .with_env_filter(
                    tracing_subscriber::EnvFilter::from_default_env()
                        .add_directive(tracing::Level::INFO.into()),
                )
                .with_writer(std::io::stderr);

            if use_json {
                builder.json().init();
            } else {
                builder.init();
            }
        }
    }
}

fn get_agent_id_from_env() -> String {
    // Try branch first
    if let Ok(branch) = git::get_current_branch() {
        if let Some(id) = git::extract_agent_id(&branch) {
            return id;
        }
    }

    // Try components of the current working directory path
    if let Ok(cwd) = std::env::current_dir() {
        // Iterate through path components from right to left
        for component in cwd.components().rev() {
            if let Some(name) = component.as_os_str().to_str() {
                if let Some(id) = git::extract_agent_id(name) {
                    return id;
                }
            }
        }
    }

    "unknown".to_string()
}

// ============================================================================
// Main
// ============================================================================

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize logging based on command type
    init_logging(&cli.command);

    // Discover config (or use default)
    let raw_config = config::Config::discover().unwrap_or_else(|e| {
        debug!(error = %e, "No config found, using defaults");
        config::Config::default()
    });

    // Validate config (exits early with helpful error if invalid)
    let config = raw_config
        .validate()
        .map_err(|e| anyhow::anyhow!("Config validation failed: {}", e))?;

    let wasm_path = config.wasm_path_buf();

    info!(
        role = ?config.role(),
        wasm_path = %wasm_path.display(),
        "Loaded and validated config"
    );

    match cli.command {
        Commands::Hook { event, runtime } => {
            info!(wasm = ?wasm_path, "Loading WASM plugin");

            // Initialize and validate services
            let services = Arc::new(
                Services::new()
                    .validate()
                    .context("Failed to validate services")?,
            );

            // Load WASM plugin
            let plugin = PluginManager::new(wasm_path, services)
                .await
                .context("Failed to load WASM plugin")?;

            info!("WASM plugin loaded and initialized");

            handle_hook(&plugin, event, runtime).await?;
        }

        Commands::McpStdio => {
            // stdio MCP server - Claude Code spawns this process
            // Use config already loaded above
            let project_dir_ref = config.project_dir();
            let project_dir = if project_dir_ref.is_absolute() {
                project_dir_ref.clone()
            } else {
                std::env::current_dir()
                    .context("Failed to get current directory")?
                    .join(project_dir_ref)
            };

            let pid_file = project_dir.join(".exomonad/sidecar.pid");
            let _pid_guard = exomonad::pid::PidGuard::new(&pid_file)?;

            info!(wasm = ?wasm_path, "Loading WASM plugin");

            // Initialize and validate services (secrets loaded from ~/.exomonad/secrets)
            let services = Arc::new(
                Services::new()
                    .validate()
                    .context("Failed to validate services")?,
            );

            // Load WASM plugin
            let plugin = PluginManager::new(wasm_path, services.clone())
                .await
                .context("Failed to load WASM plugin")?;

            let state = mcp::McpState {
                project_dir,
                plugin: Arc::new(plugin),
            };

            // Emit AgentStarted
            let agent_id = get_agent_id_from_env();
            if agent_id != "unknown" {
                match exomonad_ui_protocol::AgentId::try_from(agent_id.clone()) {
                    Ok(id) => {
                        let start_event = exomonad_ui_protocol::AgentEvent::AgentStarted {
                            agent_id: id,
                            timestamp: zellij_events::now_iso8601(),
                        };
                        if let Err(e) = zellij_events::emit_event(&start_event) {
                            warn!("Failed to emit agent:started event: {}", e);
                        }
                    }
                    Err(e) => warn!("Invalid agent_id '{}': {}", agent_id, e),
                }
            }

            mcp::stdio::run_stdio_server(state).await?;

            // Emit AgentStopped
            // Re-fetch branch as it might have changed
            let stop_agent_id = get_agent_id_from_env();
            if stop_agent_id != "unknown" {
                match exomonad_ui_protocol::AgentId::try_from(stop_agent_id.clone()) {
                    Ok(id) => {
                        let stop_event = exomonad_ui_protocol::AgentEvent::AgentStopped {
                            agent_id: id,
                            timestamp: zellij_events::now_iso8601(),
                        };
                        if let Err(e) = zellij_events::emit_event(&stop_event) {
                            warn!("Failed to emit agent:stopped event: {}", e);
                        }
                    }
                    Err(e) => warn!("Invalid agent_id '{}': {}", stop_agent_id, e),
                }
            }
        }

        Commands::Reply {
            id,
            payload,
            cancel,
        } => {
            // Socket path env var or default
            let socket_path = std::env::var("EXOMONAD_CONTROL_SOCKET")
                .unwrap_or_else(|_| ".exomonad/sockets/control.sock".to_string());

            debug!(socket = %socket_path, "Connecting to control socket");

            let mut stream = UnixStream::connect(&socket_path).await.context(format!(
                "Failed to connect to control socket at {}",
                socket_path
            ))?;

            let parsed_payload = if let Some(p) = payload {
                Some(serde_json::from_str(&p).context("Invalid JSON payload")?)
            } else {
                None
            };

            let request = ServiceRequest::UserInteraction {
                request_id: id,
                payload: parsed_payload,
                cancel,
            };

            // NDJSON format: JSON + newline
            let mut json = serde_json::to_vec(&request).context("Serialization failed")?;
            json.push(b'\n');

            stream
                .write_all(&json)
                .await
                .context("Failed to write to socket")?;

            // We don't necessarily wait for response here, it's a push notification
            info!("Sent reply to control socket");
        }

        Commands::Warmup { path } => {
            info!(path = %path.display(), "Warming up WASM plugin cache...");

            // Initialize services (required for PluginManager)
            let services = Arc::new(
                Services::new()
                    .validate()
                    .context("Failed to validate services")?,
            );

            // Load WASM plugin - this triggers compilation and caching
            let _plugin = PluginManager::new(path, services)
                .await
                .context("Failed to load WASM plugin")?;

            info!("WASM plugin warmed up successfully");
        }
    }

    Ok(())
}
